#These scripts are run with NYU greene high performance computing service
library(lme4)
library(tidyverse)
library(ggplot2)
library(brms)
library(bayestestR)
source("../shared/util.R")
source("../shared/brms_parameters.R")

rt.data <- load_data("RelativeClause") 

filler.data <- load_data("Fillers") 

rt.data <- Predicting_RT_with_spillover(rt.data, 'RelativeClause')

filler.data <-  Predicting_RT_with_spillover(filler.data, 'filler')

position_fit_lmer_nocor <- lmer(RT ~ scale(WordPosition) + (1 + scale(WordPosition) || participant), subset(filler.data, model=='lstm')) # model doesn't matter for RT. 
summary(position_fit_lmer_nocor)

rt.data$wordpos_predrt <- predict(position_fit_lmer_nocor, rt.data)

rt.data$corrected_rt <- rt.data$RT - rt.data$wordpos_predrt


prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),    
            prior("normal(0,500)", class = "sigma"))




print(getwd())

temp <- list()

i <- 1
for(m in unique(rt.data$model)){
  print(m)
  curr_rc <- subset(rt.data, model == m & !is.na(RT))
  curr_filler <- subset(filler.data, model == m & !is.na(RT))
  print(paste(nrow(curr_filler), nrow(filler.data)))
  
  curr_fit <-  lmer(predicted ~ scale(WordPosition) + (1 + scale(WordPosition) || participant),
                    curr_filler)
  
  print(summary(curr_fit))
  
  curr_rc$wordpos_predicted = predict(curr_fit, curr_rc)
  curr_rc$corrected_predicted = curr_rc$predicted - curr_rc$wordpos_predicted
  
  temp[[i]] <- curr_rc
  i <- i + 1
}

predicted_dat <- dplyr::bind_rows(temp)

predicted_dat <- predicted_dat %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))

rm(temp)



fit_verb_lmer_lstm <- lmer(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==0 & model == 'lstm' &!is.na(RT))
)

saveRDS(fit_verb_lmer_lstm, 'fit_verb_lmer_lstm.rds')

summary(fit_verb_lmer_lstm)

fit_verb_lmer_gpt2 <- lmer(corrected_predicted ~ Type_num +
                             (0 + Type_num || participant) +
                             (1 + Type_num || item),
                           data=subset(predicted_dat, ROI==0 & model == 'gpt2' &!is.na(RT))
)

saveRDS(fit_verb_lmer_gpt2, 'fit_verb_lmer_gpt2.rds')

summary(fit_verb_lmer_gpt2)


brms_parms <- get_brms_parameters('prior1')




fit_verb_bayes_pred_gpt2 <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==0 & model == 'gpt2' &!is.na(RT)),
                                prior = prior1,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_verb_bayes_pred_gpt2, 'fit_verb_bayes_pred_gpt2_prior1.rds')


summary(fit_verb_bayes_pred_gpt2)

fit_det_bayes_pred_gpt2 <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==1 & model == 'gpt2' &!is.na(RT)),
                                prior = prior1,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_det_bayes_pred_gpt2, 'fit_det_bayes_pred_gpt2_prior1.rds')


summary(fit_det_bayes_pred_gpt2)

fit_noun_bayes_pred_gpt2 <- brm(corrected_predicted ~ Type_num +
                                 (0 + Type_num || participant) +
                                 (1 + Type_num || item),
                               data=subset(predicted_dat, ROI==2 & model == 'gpt2' &!is.na(RT)),
                               prior = prior1,
                               cores = brms_parms$ncores,
                               iter = brms_parms$niters,
                               seed = brms_parms$seed,
                               warmup = brms_parms$warmup,
                               control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_noun_bayes_pred_gpt2, 'fit_noun_bayes_pred_gpt2_prior1.rds')


summary(fit_noun_bayes_pred_gpt2)




fit_verb_bayes_pred_lstm <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==0 & model == 'lstm' &!is.na(RT)),
                                prior = prior1,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_verb_bayes_pred_lstm, 'fit_verb_bayes_pred_lstm_prior1.rds')


summary(fit_verb_bayes_pred_lstm)

fit_det_bayes_pred_lstm <- brm(corrected_predicted ~ Type_num +
                                 (0 + Type_num || participant) +
                                 (1 + Type_num || item),
                               data=subset(predicted_dat, ROI==1 & model == 'lstm' &!is.na(RT)),
                               prior = prior1,
                               cores = brms_parms$ncores,
                               iter = brms_parms$niters,
                               seed = brms_parms$seed,
                               warmup = brms_parms$warmup,
                               control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_det_bayes_pred_lstm, 'fit_det_bayes_pred_lstm_prior1.rds')


summary(fit_det_bayes_pred_lstm)

fit_noun_bayes_pred_lstm <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==2 & model == 'lstm' &!is.na(RT)),
                                prior = prior1,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_noun_bayes_pred_lstm, 'fit_noun_bayes_pred_lstm_prior1.rds')


summary(fit_noun_bayes_pred_lstm)





pred_dat_verb_gpt2 <- reshape_item_dat(fit_verb_bayes_pred_gpt2, "item") %>%
  mutate(ROI = 0,
         model = 'gpt2')


pred_dat_verb_lstm <- reshape_item_dat(fit_verb_bayes_pred_lstm, "item") %>%
  mutate(ROI = 0,
         model = 'lstm')

pred_dat_verb <- dplyr::bind_rows(pred_dat_verb_gpt2, pred_dat_verb_lstm)


saveRDS(pred_dat_verb_lstm, 'rc_subset_sampledsumm_lstm.rds')
saveRDS(pred_dat_verb_gpt2, 'rc_subset_sampledsumm_gpt2.rds')

saveRDS(pred_dat_verb, 'rc_subset_sampledsumm_predicted.rds')



for(m in unique(pred_dat_verb$model)){
  curr_dat <- subset(pred_dat_verb, model == m)
  print(paste(nrow(curr_dat), nrow(pred_dat_verb)))
  
  curr_by_construction <- curr_dat %>%
    mutate(diff = b_Type_num,
           coef='RC') %>%
    group_by(ROI, coef) %>%
    summarise(mean = mean(diff),
              lower = quantile(diff, 0.025)[[1]],
              upper = quantile(diff, 0.975)[[1]])
  
  dir <- ''
  
  saveRDS(curr_by_construction, paste0(dir,'by_construction_', m, '.rds'))
  
  curr_by_item <- curr_dat %>%
    mutate(diff = b_Type_num + r_Type_num,
           coef = 'RC',
           item = as.numeric(item)) %>%
    group_by(item,ROI, coef) %>%
    summarise(mean = mean(diff),
              lower = quantile(diff, 0.025)[[1]],
              upper = quantile(diff, 0.975)[[1]]) 
  
  saveRDS(curr_by_item, paste0(dir,'by_item_', m, '.rds'))
}

