source("util.R")
rt.data <- read.csv("ClassicGardenPathSet.csv", header=TRUE) %>% mutate(participant=MD5)
rt.data$RT <- ifelse(rt.data$RT>7000,NA,ifelse(rt.data$RT<0,NA,rt.data$RT))
rt.data$Sentence <- str_replace_all(rt.data$Sentence, "%2C", ",")
rt.data$EachWord <- str_replace_all(rt.data$EachWord, "%2C", ",")
rt.data$word <-  tolower(ifelse(substring(rt.data$EachWord,nchar(rt.data$EachWord),nchar(rt.data$EachWord))%in%c(".",","),substring(rt.data$EachWord,1,nchar(rt.data$EachWord)-1),rt.data$EachWord))
fit_P0 <- readRDS("fitClassicGP_prior1_P0.RDS")
fit_P1 <- readRDS("fitClassicGP_prior1_P1.RDS")
fit_P2 <- readRDS("fitClassicGP_prior1_P2.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(list=c(fit_P0,fit_P1,fit_P0,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2))
#ClassicGP
by_item <- reshape_item_output %>%
  mutate(GPE_MVRR = b_AMBUAMB + r_AMBUAMB,
         GPE_NPS = b_AMBUAMB + `b_AMBUAMB:SZM1` + r_AMBUAMB + `r_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB + `b_AMBUAMB:SZM2` + r_AMBUAMB + `r_AMBUAMB:SZM2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = b_AMBUAMB,
         GPE_NPS = b_AMBUAMB+`b_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB+`b_AMBUAMB:SZM2`) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"ClassicGP")
Plot_empirical_construction_level(by_construction,"ClassicGP")
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=0)
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=1)
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=2)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,1)
PredictedRT_df <- Predicting_RT_with_spillover_refactored(rt.data,"ClassicGP")

#RelativeClause
rt.data <- read.csv("RelativeClauseSet.csv", header=TRUE) %>% mutate(participant=MD5)
rt.data$RT <- ifelse(rt.data$RT>7000,NA,ifelse(rt.data$RT<0,NA,rt.data$RT))
rt.data$Sentence <- str_replace_all(rt.data$Sentence, "%2C", ",")
rt.data$EachWord <- str_replace_all(rt.data$EachWord, "%2C", ",")
rt.data$word <-  tolower(ifelse(substring(rt.data$EachWord,nchar(rt.data$EachWord),nchar(rt.data$EachWord))%in%c(".",","),substring(rt.data$EachWord,1,nchar(rt.data$EachWord)-1),rt.data$EachWord))
fit_P0 <- readRDS("fitRelativeClause_prior1_P0.RDS")
fit_P1 <- readRDS("fitRelativeClause_prior1_P1.RDS")
fit_P2 <- readRDS("fitRelativeClause_prior1_P2.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(list=c(fit_P0,fit_P1,fit_P0,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2))
by_item <- reshape_item_output %>%
  mutate(RC = b_Type_num + r_Type_num) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = b_Type_num) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"RelativeClause")
Plot_empirical_construction_level(by_construction,"RelativeClause")
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=0)
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=1)
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=2)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
PredictedRT_df <- Predicting_RT_with_spillover_refactored(rt.data,"RelativeClause")
PredictedRT_df <- PredictedRT_df %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))
fit_verb_lmer_pred_lstm <- lmer(predicted ~ Type_num + (1 + Type_num || participant) + (1 + Type_num || item),data=subset(PredictedRT_df, ROI==0 & model == 'lstm'&!is.na(RT)))
fit_verb_lmer_pred_gpt2 <- lmer(predicted ~ Type_num + (1 + Type_num || participant) + (1 + Type_num || item),data=subset(PredictedRT_df, ROI==0 & model == 'gpt2'&!is.na(RT)))


#AttachmentAmbiguity
rt.data <- read.csv("AttachmentSet.csv", header=TRUE) %>% mutate(participant=MD5)
rt.data$RT <- ifelse(rt.data$RT>7000,NA,ifelse(rt.data$RT<0,NA,rt.data$RT))
rt.data$Sentence <- str_replace_all(rt.data$Sentence, "%2C", ",")
rt.data$EachWord <- str_replace_all(rt.data$EachWord, "%2C", ",")
rt.data$word <-  tolower(ifelse(substring(rt.data$EachWord,nchar(rt.data$EachWord),nchar(rt.data$EachWord))%in%c(".",","),substring(rt.data$EachWord,1,nchar(rt.data$EachWord)-1),rt.data$EachWord))
fit_P0 <- readRDS("fitAttachmentAmbiguity_prior1_P0.RDS")
fit_P1 <- readRDS("fitAttachmentAmbiguity_prior1_P1.RDS")
fit_P2 <- readRDS("fitAttachmentAmbiguity_prior1_P2.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(list=c(fit_P0,fit_P1,fit_P0,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2))
by_item <- reshape_item_output %>%
  mutate(GPE_high = -b_ambiguity + (-1/2)*b_height -r_ambiguity + (-1/2)*r_height,
         GPE_low = -b_ambiguity + (1/2)*b_height -r_ambiguity + (1/2)*r_height)  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = -b_ambiguity + (-1/2)*b_height,
         GPE_low = -b_ambiguity + (1/2)*b_height) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"AttachmentAmbiguity")
Plot_empirical_construction_level(by_construction,"AttachmentAmbiguity")
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=0)
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=1)
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=2)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
PredictedRT_df <- Predicting_RT_with_spillover_refactored(rt.data,"AttachmentAmbiguity")

## Agreement subset
rt.data <- read.csv("RelativeClauseSet.csv", header=TRUE) %>% mutate(participant=MD5)
rt.data$RT <- ifelse(rt.data$RT>7000,NA,ifelse(rt.data$RT<0,NA,rt.data$RT))
rt.data$Sentence <- str_replace_all(rt.data$Sentence, "%2C", ",")
rt.data$EachWord <- str_replace_all(rt.data$EachWord, "%2C", ",")
rt.data$word <-  tolower(ifelse(substring(rt.data$EachWord,nchar(rt.data$EachWord),nchar(rt.data$EachWord))%in%c(".",","),substring(rt.data$EachWord,1,nchar(rt.data$EachWord)-1),rt.data$EachWord))
fit <- readRDS("fitAgreement_prior1.RDS")
reshape_item_output <- reshape_item_dat(fit, 'item')
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(list=c(fit))
by_item <- reshape_item_output %>%
  mutate(GPE = b_pGramU + b_TypeNPZ:pGramU + r_pGramU + r_TypeNPZ:pGramU,
         Agr = b_pGramU + r_pGramU,
         GPE_0 = GPE + 0.5*`b_pGramU:position1` + 0.5 * `b_TypeNPZ:pGramU:position1` + 0.5*`r_pGramU:position1` + 0.5*`r_TypeNPZ:pGramU:position1`,
         GPE_1 = GPE + 0.5*`b_pGramU:position2` + 0.5 * `b_TypeNPZ:pGramU:position2` + 0.5*`r_pGramU:position2` + 0.5*`r_TypeNPZ:pGramU:position2`,
         GPE_2 = GPE  - 0.5 * `b_pGramU:position1` - 0.5 * `b_TypeNPZ:pGramU:position1` -  0.5 * `b_pGramU:position2` - 0.5 * `b_TypeNPZ:pGramU:position2`- 0.5 * `r_pGramU:position1` - 0.5 * `r_TypeNPZ:pGramU:position1` -  0.5 * `r_pGramU:position2` - 0.5 * `r_TypeNPZ:pGramU:position2`,
         Agr_0 = Agr + 0.5 * `b_pGramU:position1` + 0.5 * `r_pGramU:position1`,
         Agr_1 = Agr + 0.5 * `b_pGramU:position2` + 0.5 * `r_pGramU:position2`,
         Agr_2 = Agr  - 0.5 * `b_pGramU:position1`- 0.5 * `b_pGramU:position2` - 0.5 * `r_pGramU:position1`- 0.5 * `r_pGramU:position2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, GPE, Agr, GPE_0, GPE_1, GPE_2, Agr_0, Agr_1, Agr_2) %>%
  gather(key = 'coef', value = 'val', GPE, Agr, GPE_0, GPE_1, GPE_2, Agr_0, Agr_1, Agr_2) %>%
  group_by(item, coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])%>%separate(coef,c('coef','ROI'),sep='_')
by_construction <- reshape_item_output[,c('.draw',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_0 = b_pGramU + b_TypeNPZ:pGramU + 0.5*`b_pGramU:position1` + 0.5 * `b_TypeNPZ:pGramU:position1`,
         GPE_1 = b_pGramU + b_TypeNPZ:pGramU + 0.5*`b_pGramU:position2` + 0.5 * `b_TypeNPZ:pGramU:position2` ,
         GPE_2 = b_pGramU + b_TypeNPZ:pGramU - 0.5 * `b_pGramU:position1` - 0.5 * `b_TypeNPZ:pGramU:position1` -  0.5 * `b_pGramU:position2` - 0.5 * `b_TypeNPZ:pGramU:position2`,
         Agr_0 = b_pGramU + 0.5 * `b_pGramU:position1`,
         Agr_1 = b_pGramU + 0.5 * `b_pGramU:position2`,
         Agr_2 = b_pGramU - 0.5 * `b_pGramU:position1`- 0.5 * `b_pGramU:position2`) %>%
  select(GPE_0,GPE_1,GPE_2,Agr_0,Agr_1,Agr_2) %>% gather(key='coef',value='val')%>%group_by(coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])%>%separate(coef, c('coef', 'ROI'), sep= '_')
Plot_empirical_construction_level(by_construction,"Agreement")
Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=0)
Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=1)
Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=2)
PredictedRT_df <- Predicting_RT_with_spillover_refactored(rt.data,"Agreement")





















rt.data.drop.lstm$SZM1 <-ifelse(rt.data.drop.lstm$CONSTRUCTION=="NPS",1,0)
rt.data.drop.lstm$SZM2 <-ifelse(rt.data.drop.lstm$CONSTRUCTION=="NPZ",1,0)
rt.data.drop.gpt2$SZM1 <-ifelse(rt.data.drop.gpt2$CONSTRUCTION=="NPS",1,0)
rt.data.drop.gpt2$SZM2 <-ifelse(rt.data.drop.gpt2$CONSTRUCTION=="NPZ",1,0)

lmer_lstm_predicted_P0 <- lmer(predicted ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1|participant),data=rt.data.drop.lstm[rt.data.drop.lstm$ROI==0&rt.data.drop.lstm$predicted<=7000&rt.data.drop.lstm$predicted>0,], 
                               control = lmerControl(optimizer ="Nelder_Mead"))
lmer_gpt2_predicted_P0 <- lmer(predicted ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1|participant),data=rt.data.drop.gpt2[rt.data.drop.gpt2$ROI==0&rt.data.drop.gpt2$predicted<=7000&rt.data.drop.gpt2$predicted>0,], 
                               control = lmerControl(optimizer ="Nelder_Mead"))

ranef_col_lstm_P0 <- ranef(lmer_lstm_predicted_P0)[['item']]
itemwise_MVRR_lstm_P0 <- ranef_col_lstm_P0[,'AMBUAMB']+summary(lmer_lstm_predicted_P0)$coefficients[2,1]
itemwise_NPS_lstm_P0 <- ranef_col_lstm_P0[,'AMBUAMB']+ranef_col_lstm_P0[,'AMBUAMB:SZM1']+summary(lmer_lstm_predicted_P0)$coefficients[2,1]+summary(lmer_lstm_predicted_P0)$coefficients[5,1]
itemwise_NPZ_lstm_P0 <- ranef_col_lstm_P0[,'AMBUAMB']+ranef_col_lstm_P0[,'AMBUAMB:SZM2']+summary(lmer_lstm_predicted_P0)$coefficients[2,1]+summary(lmer_lstm_predicted_P0)$coefficients[6,1]
itemwise_lstm_P0 <- data.frame(item=rep(1:24,3),itemwise_predicted=c(itemwise_MVRR_lstm_P0,itemwise_NPS_lstm_P0,itemwise_NPZ_lstm_P0),coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=24),model="lstm")
ranef_col_gpt2_P0 <- ranef(lmer_gpt2_predicted_P0)[['item']]
itemwise_MVRR_gpt2_P0 <- ranef_col_gpt2_P0[,'AMBUAMB']+summary(lmer_gpt2_predicted_P0)$coefficients[2,1]
itemwise_NPS_gpt2_P0 <- ranef_col_gpt2_P0[,'AMBUAMB']+ranef_col_gpt2_P0[,'AMBUAMB:SZM1']+summary(lmer_gpt2_predicted_P0)$coefficients[2,1]+summary(lmer_gpt2_predicted_P0)$coefficients[5,1]
itemwise_NPZ_gpt2_P0 <- ranef_col_gpt2_P0[,'AMBUAMB']+ranef_col_gpt2_P0[,'AMBUAMB:SZM2']+summary(lmer_gpt2_predicted_P0)$coefficients[2,1]+summary(lmer_gpt2_predicted_P0)$coefficients[6,1]
itemwise_gpt2_P0 <- data.frame(item=rep(1:24,3),itemwise_predicted=c(itemwise_MVRR_gpt2_P0,itemwise_NPS_gpt2_P0,itemwise_NPZ_gpt2_P0),coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=24),model="gpt2")

by_item <- arrange(by_item,ROI,coef,)
by_item_P0 <- by_item[by_item$ROI==0,]
by_item_P0 <- rbind(by_item_P0,by_item_P0)
by_item_P0$predicted <- c(itemwise_lstm_P0$itemwise_predicted,itemwise_gpt2_P0$itemwise_predicted)
by_item_P0$model <- c(itemwise_lstm_P0$model,itemwise_gpt2_P0$model)

ggplot(by_item_P0,aes(x=predicted,y=mean))+
  geom_point()+
  facet_grid(model~coef)+
  xlab("Predicted structural effects (ROI=0")+
  ylab("Empirical structural effects (ROI=0")+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size=12,face="bold"))+
  theme(aspect.ratio = 1)







rt.data.freqs.surps <- Lists_predictedRT_df[[1]]
rt.data.freqs.surps$SZM1 <- ifelse(rt.data.freqs.surps$CONSTRUCTION=="NPS",1,0)
rt.data.freqs.surps$SZM2 <- ifelse(rt.data.freqs.surps$CONSTRUCTION=="NPZ",1,0)
coef_surprisal_lstm <- data.frame(mean=summary(lm(surprisal_lstm ~ AMBUAMB*(SZM1+SZM2),data=rt.data.freqs.surps[rt.data.freqs.surps$ROI==0,]))$coefficients[c(2,5,6),1])
coef_surprisal_gpt2 <- data.frame(mean=summary(lm(surprisal_gpt2 ~ AMBUAMB*(SZM1+SZM2),data=rt.data.freqs.surps[rt.data.freqs.surps$ROI==0,]))$coefficients[c(2,5,6),1])
Plot_surprisal_construction_level(coef_surprisal_lstm,coef_surprisal_gpt2,"ClassicGP")


library(ggplot2)
GP_splithalf <- readRDS("split_half_ceiling_results_GP_simple.rds")
aggregate(GP_splithalf$corr_each,by=list(GP_splithalf$size_each,GP_splithalf$type),FUN=mean)
for(i in unique(GP_splithalf$type)){
 g <- ggplot(GP_splithalf[GP_splithalf$type==i,], aes(size_each,corr_each)) + geom_point() + geom_smooth()+xlim(400,2500)+ylim(0,1)+xlab("# of participants")+ylab("estimated ceiling")+ggtitle(paste(i))
 plot(g)
}
RC_splithalf <- readRDS("split_half_ceiling_results_RC_simple.rds")
aggregate(RC_splithalf$corr_each,by=list(RC_splithalf$size_each,RC_splithalf$type),FUN=mean)
for(i in unique(RC_splithalf$type)){
  g <- ggplot(RC_splithalf[RC_splithalf$type==i,], aes(size_each,corr_each)) + geom_point() + geom_smooth()+xlim(400,2500)+ylim(0,1)+xlab("# of participants")+ylab("estimated ceiling")+ggtitle(paste(i))
  plot(g)
}
