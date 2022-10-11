#These scripts are run with NYU greene high performance computing service
source("../../util.R")
source("../../brms_parameters.R")
library(dplyr)
library(stringr)
library(tidyr)
library(lme4)
library(stats)
library(base)



spr <- load_data("Fillers")
brms_parms <- get_brms_parameters('prior1')
prior1 <- c(prior("normal(300,1000)", class = "Intercept"),  
            prior("normal(0,200)", class = "sd"),    
            prior("normal(0,500)", class = "sigma"))



surps_lstm <- read.csv("../../Surprisals/data/lstm/items_filler.lstm.csv.scaled")
surps_gpt2 <- read.csv("../../Surprisals/data/gpt2/items_filler.gpt2.csv.scaled")
surps_lstm$word_pos = surps_lstm$word_pos + 1# adjust to 1-indexing
surps_gpt2$word_pos = surps_gpt2$word_pos + 1# adjust to 1-indexing

bind_surps <- function(spr, surps) {
  merged <- merge(x=spr, y=surps,
                  by.x=c("Sentence", "WordPosition"), by.y=c("Sentence", "word_pos"), 
                  all.x=TRUE)
  
  merged$item <- merged$item.x
  merged$surprisal_s <- merged$sum_surprisal_s #change to mean if that's more appropriate
  
  with_lags <- merged %>% group_by_at(vars(item, participant)) %>%
    mutate(RT_p1 = lag(RT), 
           RT_p2 = lag(RT_p1), 
           RT_p3 = lag(RT_p2),
           length_p1_s = lag(length_s), 
           length_p2_s = lag(length_p1_s),
           length_p3_s = lag(length_p2_s),
           logfreq_p1_s = lag(logfreq_s), 
           logfreq_p2_s = lag(logfreq_p1_s),
           logfreq_p3_s = lag(logfreq_p2_s),
           surprisal_p1_s = lag(surprisal_s),
           surprisal_p2_s = lag(surprisal_p1_s),
           surprisal_p3_s = lag(surprisal_p2_s)
    )
  
  with_lags$sent_length <- lapply(str_split(with_lags$Sentence, " "), length)
  
  dropped <- subset(with_lags, !is.na(surprisal_s) &
                      !is.na(surprisal_p1_s) & 
                      !is.na(surprisal_p2_s) &
                      !is.na(surprisal_p3_s) &
                      !is.na(logfreq_s) & !is.na(logfreq_p1_s) &
                      !is.na(logfreq_p2_s) & !is.na(logfreq_p3_s) & 
                      (with_lags$sent_length != with_lags$WordPosition))
  
  print(paste0("dropped: ", nrow(with_lags) - nrow(dropped)))
  return(dropped)
}


dropped.lstm <- bind_surps(spr, surps_lstm)
dropped.gpt2 <- bind_surps(spr, surps_gpt2)



i=1
Size=2000
corr=c()
while(i<16){
  half_name <- sample(unique(dropped.gpt2$participant),(Size/2),replace=F)
  rt.1 <- dropped.gpt2[dropped.gpt2$participant%in%half_name,]
  rt.2 <- dropped.gpt2[!dropped.gpt2$participant%in%half_name,]
  wordindex <- distinct(ungroup(dropped.lstm),Sentence,WordPosition)%>%mutate(WordIndex=1:498,randomsample24=sample(c(rep(1,474),rep(2,24)),498,replace=F))
  rt.1 <- left_join(rt.1,wordindex)
  rt.2 <- left_join(rt.2,wordindex)
  lmer1 <- lmer(RT ~ (1|WordIndex)+(1|participant),data=rt.1[rt.1$randomsample24==2,])
  lmer2 <- lmer(RT ~ (1|WordIndex)+(1|participant),data=rt.2[rt.2$randomsample24==2,])
  WordIndex1 <- ranef(lmer1)[['WordIndex']]
  WordIndex2 <- ranef(lmer2)[['WordIndex']]
  WordIndex1$wordinex <- unique(rownames(WordIndex1))
  WordIndex2$wordinex <- unique(rownames(WordIndex2))
  print(cor.test(WordIndex1$`(Intercept)`[WordIndex1$wordinex%in%intersect(WordIndex1$wordinex,WordIndex2$wordinex)],WordIndex2$`(Intercept)`[WordIndex2$wordinex%in%intersect(WordIndex1$wordinex,WordIndex2$wordinex)]))
  i=i+1
}
print(corr)
mean(corr)



sampled_correlations <- data.frame(Correlation=rep(NA,5000),model=rep(c("nosurp","lstmonly","gpt2only","lstm","gpt2"),1000))
for(i in 1){
  wordindex <- distinct(ungroup(dropped.lstm),Sentence,WordPosition)%>%mutate(WordIndex=1:498)
  unique_index <- dropped.lstm %>% ungroup() %>%distinct(Sentence) %>% mutate(train_test=sample(c(rep(1,31),rep(2,8)),39,replace=F))
  dropped.lstm$train_test <- NULL
  dropped.lstm <- left_join(dropped.lstm,unique_index)
  unique_index2 <- dropped.lstm[dropped.lstm$train_test==2,] %>% ungroup() %>%distinct(Sentence,WordPosition)
  train_test <- c()
  smallerthanthree = 0
  for(j in as.vector(table(unique_index2$Sentence))){
    if(j<3)
    {smallerthanthree=1
    train_test <- c(train_test,2)
    print(table(train_test))
    }
    else{
      if(smallerthanthree==1){
        if(j==4){
          fixed = 4
          train_test <- c(train_test,sample(c(rep(2,fixed),rep(3,j-fixed)),j,replace=F))
          smallerthanthree=2
          print(table(train_test))
        }
        else{
          fixed = 5
          train_test <- c(train_test,sample(c(rep(2,fixed),rep(3,j-fixed)),j,replace=F))
          smallerthanthree=0
          print(table(train_test))}
      }
      else{
        if(smallerthanthree==2){
          fixed = 4
          train_test <- c(train_test,sample(c(rep(2,fixed),rep(3,j-fixed)),j,replace=F))
          smallerthanthree=0
          print(table(train_test))
        }
        else{
          fixed = 3
          train_test <- c(train_test,sample(c(rep(2,fixed),rep(3,j-fixed)),j,replace=F))
          print(table(train_test))
        }
      }
    }
  }
  unique_index2$train_test <- train_test
  dropped.lstm$train_test <- NULL
  dropped.lstm <- left_join(dropped.lstm,unique_index2)
  dropped.lstm$train_test <- ifelse(is.na(dropped.lstm$train_test),1,dropped.lstm$train_test)
  dropped.lstm <- left_join(dropped.lstm,wordindex)
  table(distinct(ungroup(dropped.lstm),Sentence,WordPosition,WordIndex,train_test)$train_test)
  dropped.gpt2$train_test <- NULL
  dropped.gpt2 <- left_join(dropped.gpt2,unique_index2)
  dropped.gpt2$train_test <- ifelse(is.na(dropped.gpt2$train_test),1,dropped.gpt2$train_test)
  dropped.gpt2 <- left_join(dropped.gpt2,wordindex)
  filler.model_lstm <- lmer(data=dropped.lstm[dropped.lstm$train_test==1&!is.na(dropped.lstm$RT),],
                            RT ~ surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s +
                              scale(WordPosition) + logfreq_s*length_s + logfreq_p1_s*length_p1_s + 
                              logfreq_p2_s*length_p2_s + logfreq_p3_s*length_p3_s + 
                              (1 + surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s || participant) + (1 | item),
                            control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  filler.model_gpt2 <- lmer(data=dropped.gpt2[dropped.gpt2$train_test==1&!is.na(dropped.gpt2$RT),],
                            RT ~ surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s +
                              scale(WordPosition) + logfreq_s*length_s + logfreq_p1_s*length_p1_s + 
                              logfreq_p2_s*length_p2_s + logfreq_p3_s*length_p3_s + 
                              (1 + surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s || participant) + (1 | item),
                            control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  filler.model_nosurp <- lmer(data=dropped.gpt2[dropped.gpt2$train_test==1&!is.na(dropped.gpt2$RT),],
                            RT ~ scale(WordPosition) + logfreq_s*length_s + logfreq_p1_s*length_p1_s + 
                              logfreq_p2_s*length_p2_s + logfreq_p3_s*length_p3_s + 
                              (1 + logfreq_s + logfreq_p1_s + logfreq_p2_s + logfreq_p3_s || participant) + (1 | item),
                            control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  filler.model_lstm_only <- lmer(data=dropped.lstm[dropped.lstm$train_test==1&!is.na(dropped.lstm$RT),],
                              RT ~ surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s +
                                (1 + surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s || participant) + (1 | item),
                              control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  filler.model_gpt2_only <- lmer(data=dropped.gpt2[dropped.gpt2$train_test==1&!is.na(dropped.gpt2$RT),],
                                 RT ~ surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s +
                                   (1 + surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s || participant) + (1 | item),
                                 control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  dropped.lstm$predicted <- predict(filler.model_lstm, newdata=dropped.lstm, allow.new.levels = TRUE)
  dropped.lstm$predicted_lstm_only <- predict(filler.model_lstm_only, newdata=dropped.lstm, allow.new.levels = TRUE)
  dropped.gpt2$predicted <- predict(filler.model_gpt2, newdata=dropped.gpt2, allow.new.levels = TRUE)
  dropped.gpt2$predicted_nosurp <- predict(filler.model_nosurp, newdata=dropped.gpt2, allow.new.levels = TRUE)
  dropped.gpt2$predicted_gpt2_only <- predict(filler.model_gpt2_only, newdata=dropped.gpt2, allow.new.levels = TRUE)

  
  
  brm_empirical_test <- brm(RT ~ (1|WordIndex)+(1|participant),data=dropped.lstm[dropped.lstm$train_test==2&!is.na(dropped.lstm$RT),],
                            prior = prior1,
                            cores = brms_parms$ncores,
                            iter = 24000,
                            seed = brms_parms$seed,
                            warmup = 12000,
                            control = list(adapt_delta = brms_parms$adapt_delta))
  posterior_samp <- posterior_samples(brm_empirical_test)
  random_names <- colnames(posterior_samp)[grepl('r_WordIndex',colnames(posterior_samp))]
  rm(posterior_samp)
  emp_posterior_samp <- posterior_samples(brm_empirical_test, fixed=TRUE, pars=
                                            random_names)
  print(colnames(emp_posterior_samp))
  print(summary(brm_empirical_test))
  print(colMeans(emp_posterior_samp))
  rm(brm_empirical_test)
  print(warnings())
  brm_predicted_lstm_only <- brm(predicted_lstm_only ~ (1|WordIndex)+(1|participant),data=dropped.lstm[dropped.lstm$train_test==2&!is.na(dropped.lstm$RT),],
                                 prior = prior1,
                                 cores = brms_parms$ncores,
                                 iter = 24000,
                                 seed = brms_parms$seed,
                                 warmup = 12000,
                                 control = list(adapt_delta = brms_parms$adapt_delta))
  lstm_only_posterior_samp <- posterior_samples(brm_predicted_lstm_only, fixed=TRUE, pars=
                                                  random_names)
  print(colnames(lstm_only_posterior_samp))
  print(summary(brm_predicted_lstm_only))
  print(colMeans(lstm_only_posterior_samp))
  rm(brm_predicted_lstm_only)
  print(warnings())
  brm_predicted_gpt2_only <- brm(predicted_gpt2_only ~ (1|WordIndex)+(1|participant),data=dropped.gpt2[dropped.gpt2$train_test==2&!is.na(dropped.gpt2$RT),],
                                 prior = prior1,
                                 cores = brms_parms$ncores,
                                 iter = 24000,
                                 seed = brms_parms$seed,
                                 warmup = 12000,
                                 control = list(adapt_delta = brms_parms$adapt_delta))
  gpt2_only_posterior_samp <- posterior_samples(brm_predicted_gpt2_only, fixed=TRUE, pars=
                                                  random_names)
  print(colnames(gpt2_only_posterior_samp))
  print(summary(brm_predicted_gpt2_only))
  print(colMeans(gpt2_only_posterior_samp))
  rm(brm_predicted_gpt2_only)
  print(warnings())
  brm_predicted_lstm <- brm(predicted ~ (1|WordIndex)+(1|participant),data=dropped.lstm[dropped.lstm$train_test==2&!is.na(dropped.lstm$RT),],
                            prior = prior1,
                            cores = brms_parms$ncores,
                            iter = 24000,
                            seed = brms_parms$seed,
                            warmup = 12000,
                            control = list(adapt_delta = brms_parms$adapt_delta))
  lstm_posterior_samp <- posterior_samples(brm_predicted_lstm, fixed=TRUE, pars=
                                             random_names)
  print(colnames(lstm_posterior_samp))
  print(summary(brm_predicted_lstm))
  print(colMeans(lstm_posterior_samp))
  rm(brm_predicted_lstm)
  print(warnings())
  brm_predicted_gpt2 <- brm(predicted ~ (1|WordIndex)+(1|participant),data=dropped.gpt2[dropped.gpt2$train_test==2&!is.na(dropped.gpt2$RT),],
                            prior = prior1,
                            cores = brms_parms$ncores,
                            iter = 24000,
                            seed = brms_parms$seed,
                            warmup = 12000,
                            control = list(adapt_delta = brms_parms$adapt_delta))
  gpt2_posterior_samp <- posterior_samples(brm_predicted_gpt2, fixed=TRUE, pars=
                                             random_names)
  print(colnames(gpt2_posterior_samp))
  print(summary(brm_predicted_gpt2))
  print(colMeans(gpt2_posterior_samp))
  rm(brm_predicted_gpt2)
  print(warnings())
  brm_predicted_nosurp <- brm(predicted_nosurp ~ (1|WordIndex)+(1|participant),data=dropped.gpt2[dropped.gpt2$train_test==2&!is.na(dropped.gpt2$RT),],
                            prior = prior1,
                            cores = brms_parms$ncores,
                            iter = 24000,
                            seed = brms_parms$seed,
                            warmup = 12000,
                            control = list(adapt_delta = brms_parms$adapt_delta))
  nosurp_posterior_samp <- posterior_samples(brm_predicted_nosurp, fixed=TRUE, pars=
                                             random_names)
  print(colnames(nosurp_posterior_samp))
  print(summary(brm_predicted_nosurp))
  print(colMeans(nosurp_posterior_samp))
  rm(brm_predicted_nosurp)
  print(warnings())
  for(i in 1:1000){
    posterior_onesampleeachitem <- data.frame(emp=rep(NA,24),nosurp=rep(NA,24),lstm_only=rep(NA,24),gpt2_only=rep(NA,24),lstm=rep(NA,24),gpt2=rep(NA,24))
    for(j in 1:24){
      posterior_onesampleeachitem[j,1] <- sample(emp_posterior_samp[,j],1)
      posterior_onesampleeachitem[j,2] <- sample(nosurp_posterior_samp[,j],1)
      posterior_onesampleeachitem[j,3] <- sample(lstm_only_posterior_samp[,j],1)
      posterior_onesampleeachitem[j,4] <- sample(gpt2_only_posterior_samp[,j],1)
      posterior_onesampleeachitem[j,5] <- sample(lstm_posterior_samp[,j],1)
      posterior_onesampleeachitem[j,6] <- sample(gpt2_posterior_samp[,j],1)
      
    }
    sampled_correlations[((i-1)*5+1):((i-1)*5+5),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                        cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                        cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                                        cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,5])$estimate,
                                                                        cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,6])$estimate)
  }
}
aggregate(sampled_correlations$Correlation,by=list(sampled_correlations$model),FUN=mean)
aggregate(sampled_correlations$Correlation,by=list(sampled_correlations$model),FUN=sd)
warnings()

a <- round(runif(1,1,10000),0)
print(a)
saveRDS(sampled_correlations,paste0("filler_sampled_corr_",a,".rds"))
