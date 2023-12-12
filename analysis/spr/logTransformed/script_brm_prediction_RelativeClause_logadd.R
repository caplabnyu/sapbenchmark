#These scripts are run with NYU greene high performance computing service
source('util_logadd.R')
source('brms_parameters_log.R')

rt.data <- load_data("RelativeClause") 
filler.data <- load_data("Fillers") 

rt.data <- Predicting_RT_with_spillover(rt.data, 'RelativeClause')
filler.data <-  Predicting_RT_with_spillover(filler.data, 'filler')


brms_parms <- get_brms_parameters()

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
  curr_rc$corrected_predicted = exp(curr_rc$predicted) - exp(curr_rc$wordpos_predicted)
  curr_rc$corrected_predicted = ifelse(curr_rc$corrected_predicted==0,1,curr_rc$corrected_predicted)
  curr_rc$corrected_predicted = log(curr_rc$corrected_predicted)
  
  temp[[i]] <- curr_rc
  i <- i + 1
}





predicted_dat <- dplyr::bind_rows(temp)

predicted_dat <- predicted_dat %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))

rm(temp)


predicted_dat$corrected_predicted


fit_verb_bayes_pred_gpt2_log <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==0 & model == 'gpt2' &!is.na(RT)),
                                prior = brms_parms$prior,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_verb_bayes_pred_gpt2_log, 'fit_verb_bayes_pred_gpt2_prior1_logadd.rds')


summary(fit_verb_bayes_pred_gpt2_log)

fit_det_bayes_pred_gpt2_log <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==1 & model == 'gpt2' &!is.na(RT)),
                                prior = brms_parms$prior,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_det_bayes_pred_gpt2_log, 'fit_det_bayes_pred_gpt2_prior1_logadd.rds')


summary(fit_det_bayes_pred_gpt2_log)

fit_noun_bayes_pred_gpt2_log <- brm(corrected_predicted ~ Type_num +
                                 (0 + Type_num || participant) +
                                 (1 + Type_num || item),
                               data=subset(predicted_dat, ROI==2 & model == 'gpt2' &!is.na(RT)),
                               prior = brms_parms$prior,
                               cores = brms_parms$ncores,
                               iter = brms_parms$niters,
                               seed = brms_parms$seed,
                               warmup = brms_parms$warmup,
                               control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_noun_bayes_pred_gpt2_log, 'fit_noun_bayes_pred_gpt2_prior1_logadd.rds')


summary(fit_noun_bayes_pred_gpt2_log)




fit_verb_bayes_pred_lstm_log <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==0 & model == 'lstm' &!is.na(RT)),
                                prior = brms_parms$prior,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_verb_bayes_pred_lstm_log, 'fit_verb_bayes_pred_lstm_prior1_logadd.rds')


summary(fit_verb_bayes_pred_lstm_log)

fit_det_bayes_pred_lstm_log <- brm(corrected_predicted ~ Type_num +
                                 (0 + Type_num || participant) +
                                 (1 + Type_num || item),
                               data=subset(predicted_dat, ROI==1 & model == 'lstm' &!is.na(RT)),
                               prior = brms_parms$prior,
                               cores = brms_parms$ncores,
                               iter = brms_parms$niters,
                               seed = brms_parms$seed,
                               warmup = brms_parms$warmup,
                               control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_det_bayes_pred_lstm_log, 'fit_det_bayes_pred_lstm_prior1_logadd.rds')


summary(fit_det_bayes_pred_lstm_log)

fit_noun_bayes_pred_lstm_log <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==2 & model == 'lstm' &!is.na(RT)),
                                prior = brms_parms$prior,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_noun_bayes_pred_lstm_log, 'fit_noun_bayes_pred_lstm_prior1_logadd.rds')


summary(fit_noun_bayes_pred_lstm_log)




fit_verb_bayes_pred_nosurp_log <- brm(corrected_predicted ~ Type_num +
                                        (0 + Type_num || participant) +
                                        (1 + Type_num || item),
                                      data=subset(predicted_dat, ROI==0 & model == 'nosurp' &!is.na(RT)),
                                      prior = brms_parms$prior,
                                      cores = brms_parms$ncores,
                                      iter = brms_parms$niters,
                                      seed = brms_parms$seed,
                                      warmup = brms_parms$warmup,
                                      control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_verb_bayes_pred_nosurp_log, 'fit_verb_bayes_pred_nosurp_prior1_logadd.rds')


summary(fit_verb_bayes_pred_nosurp_log)

fit_det_bayes_pred_nosurp_log <- brm(corrected_predicted ~ Type_num +
                                       (0 + Type_num || participant) +
                                       (1 + Type_num || item),
                                     data=subset(predicted_dat, ROI==1 & model == 'nosurp' &!is.na(RT)),
                                     prior = brms_parms$prior,
                                     cores = brms_parms$ncores,
                                     iter = brms_parms$niters,
                                     seed = brms_parms$seed,
                                     warmup = brms_parms$warmup,
                                     control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_det_bayes_pred_nosurp_log, 'fit_det_bayes_pred_nosurp_prior1_logadd.rds')


summary(fit_det_bayes_pred_nosurp_log)

fit_noun_bayes_pred_nosurp_log <- brm(corrected_predicted ~ Type_num +
                                        (0 + Type_num || participant) +
                                        (1 + Type_num || item),
                                      data=subset(predicted_dat, ROI==2 & model == 'nosurp' &!is.na(RT)),
                                      prior = brms_parms$prior,
                                      cores = brms_parms$ncores,
                                      iter = brms_parms$niters,
                                      seed = brms_parms$seed,
                                      warmup = brms_parms$warmup,
                                      control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(fit_noun_bayes_pred_nosurp_log, 'fit_noun_bayes_pred_nosurp_prior1_logadd.rds')


summary(fit_noun_bayes_pred_nosurp_log)




