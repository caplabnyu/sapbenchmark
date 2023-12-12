#These scripts are run with NYU greene high performance computing service
source("util_logadd.R")
source("brms_parameters_log.R")
rt.data <- load_data("ClassicGP")
rt.data$SZM1 <- ifelse(rt.data$CONSTRUCTION=="NPS",1,0)
rt.data$SZM2 <- ifelse(rt.data$CONSTRUCTION=="NPZ",1,0)
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"ClassicGP")
brm_param_list <- get_brms_parameters()




brm_predicted_gpt2_GP_P1 <- brm(predicted~AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1|participant),
                                data=subset(PredictedRT_df, ROI==1&model=="gpt2"&!is.na(RT)),
                                iter=44000,
                                cores=brm_param_list$ncores,
                                warmup = 22000,
                                seed = brm_param_list$seed,
                                prior = brm_param_list$prior,
                                control = list(adapt_delta=brm_param_list$adapt_delta))
summary(brm_predicted_gpt2_GP_P1)
saveRDS(brm_predicted_gpt2_GP_P1,"brm_predicted_gpt2_GP_P1_logadd.rds")