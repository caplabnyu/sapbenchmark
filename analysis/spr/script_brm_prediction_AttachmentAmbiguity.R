#These scripts are run with NYU greene high performance computing service
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(stringr)
library(brms)
library(tidybayes)
source("../shared/util.R")
source("../shared/brms_parameters.R")

brms_parms <- get_brms_parameters('prior1')
rt.data <- load_data("AttachmentAmbiguity")


rt.data$ambiguity <- ifelse(
  rt.data$AMBIG=="Amb",2/3,-1/3)

rt.data <- rt.data %>% 
  mutate(height = case_when(Type=="AttachMulti" ~ 0, 
                            Type=="AttachLow" ~ -1/2,
                            Type=="AttachHigh" ~ 1/2))

prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),
            prior("normal(0,500)", class = "sigma"))
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"AttachmentAmbiguity")
print(nrow(PredictedRT_df[PredictedRT_df$model=="lstm",]))
print(nrow(PredictedRT_df[PredictedRT_df$model=="gpt2",]))



### ROI=0, lstm
brm_prior1_pred_lstm_0 <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==0&model=="lstm"&!is.na(RT)),
                              prior = prior1,
                              cores = brms_parms$ncores,
                              iter = brms_parms$niters,
                              seed = brms_parms$seed,
                              warmup = brms_parms$warmup,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_lstm_0, file="brm_prior1_pred_lstm_0.rds")
summary(brm_prior1_pred_lstm_0)



### ROI=1, lstm
brm_prior1_pred_lstm_1 <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==1&model=="lstm"&!is.na(RT)),
                              prior = prior1,
                              cores = brms_parms$ncores,
                              iter = brms_parms$niters,
                              seed = brms_parms$seed,
                              warmup = brms_parms$warmup,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_lstm_1, file="brm_prior1_pred_lstm_1.rds")
summary(brm_prior1_pred_lstm_1)
### ROI=2, lstm
brm_prior1_pred_lstm_2 <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==2&model=="lstm"&!is.na(RT)),
                              prior = prior1,
                              cores = brms_parms$ncores,
                              iter = brms_parms$niters,
                              seed = brms_parms$seed,
                              warmup = brms_parms$warmup,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_lstm_2, file="brm_prior1_pred_lstm_2.rds")
summary(brm_prior1_pred_lstm_2)


### ROI=0, gpt2
brm_prior1_pred_gpt2_0 <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==0&model=="gpt2"&!is.na(RT)),
                              prior = prior1,
                              cores = brms_parms$ncores,
                              iter = brms_parms$niters,
                              seed = brms_parms$seed,
                              warmup = brms_parms$warmup,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_gpt2_0, file="brm_prior1_pred_gpt2_0.rds")
summary(brm_prior1_pred_gpt2_0)


### ROI=1, gpt2
brm_prior1_pred_gpt2_1 <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==1&model=="gpt2"&!is.na(RT)),
                              prior = prior1,
                              cores = brms_parms$ncores,
                              iter = brms_parms$niters,
                              seed = brms_parms$seed,
                              warmup = brms_parms$warmup,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_gpt2_1, file="brm_prior1_pred_gpt2_1.rds")
summary(brm_prior1_pred_gpt2_1)
### ROI=2, gpt2
brm_prior1_pred_gpt2_2 <- brm(predicted ~ ambiguity + height + (1+ambiguity+height||item) + (1|participant),
                              data=subset(PredictedRT_df, ROI==2&model=="gpt2"&!is.na(RT)),
                              prior = prior1,
                              cores = brms_parms$ncores,
                              iter = brms_parms$niters,
                              seed = brms_parms$seed,
                              warmup = brms_parms$warmup,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_pred_gpt2_2, file="brm_prior1_pred_gpt2_2.rds")
summary(brm_prior1_pred_gpt2_2)