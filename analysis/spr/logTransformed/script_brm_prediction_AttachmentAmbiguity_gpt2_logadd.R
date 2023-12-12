#These scripts are run with NYU greene high performance computing service
source("util_logadd.R")
source('brms_parameters_log.R')

brms_parms <- get_brms_parameters()
rt.data <- load_data("AttachmentAmbiguity")


rt.data$ambiguity <- ifelse(
  rt.data$AMBIG=="Amb",2/3,-1/3)

rt.data <- rt.data %>% 
  mutate(height = case_when(Type=="AttachMulti" ~ 0, 
                            Type=="AttachLow" ~ -1/2,
                            Type=="AttachHigh" ~ 1/2))
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"AttachmentAmbiguity")
print(nrow(PredictedRT_df[PredictedRT_df$model=="lstm",]))
print(nrow(PredictedRT_df[PredictedRT_df$model=="gpt2",]))



###ROI=0, gpt2
brm_prior1_pred_gpt2_0_log <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==0&model=="gpt2"&!is.na(RT)),
                              prior = brms_parms$prior,
                             cores = brms_parms$ncores,
                              iter = 32000,
                              seed = brms_parms$seed,
                              warmup = 16000,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_gpt2_0_log, file="brm_prior1_pred_gpt2_0_logadd.rds")
summary(brm_prior1_pred_gpt2_0_log)



### ROI=1, gpt2
brm_prior1_pred_gpt2_1_log <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==1&model=="gpt2"&!is.na(RT)),
                              prior = brms_parms$prior,
                              cores = brms_parms$ncores,
                              iter = 32000,
                              seed = brms_parms$seed,
                              warmup = 16000,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_gpt2_1_log, file="brm_prior1_pred_gpt2_1_logadd.rds")
summary(brm_prior1_pred_gpt2_1_log)
### ROI=2, gpt2
brm_prior1_pred_gpt2_2_log <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==2&model=="gpt2"&!is.na(RT)),
                              prior = brms_parms$prior,
                              cores = brms_parms$ncores,
                              iter = 32000,
                              seed = brms_parms$seed,
                              warmup = 16000,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_gpt2_2_log, file="brm_prior1_pred_gpt2_2_logadd.rds")
summary(brm_prior1_pred_gpt2_2_log)

