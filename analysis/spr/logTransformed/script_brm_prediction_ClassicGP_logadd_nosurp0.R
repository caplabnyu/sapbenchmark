#These scripts are run with NYU greene high performance computing service
source("util_logadd.R")
source("brms_parameters_log.R")
rt.data <- load_data("ClassicGP")
rt.data$SZM1 <- ifelse(rt.data$CONSTRUCTION=="NPS",1,0)
rt.data$SZM2 <- ifelse(rt.data$CONSTRUCTION=="NPZ",1,0)
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"ClassicGP")
brm_param_list <- get_brms_parameters()



brm_predicted_nosurp_GP_P0 <- brm(predicted~AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1|participant),
                                data=subset(PredictedRT_df, ROI==0&model=="nosurp"&!is.na(RT)),
                                iter=80000,
                                cores=brm_param_list$ncores,
                                warmup = 40000,
                                seed = brm_param_list$seed,
                                prior = brm_param_list$prior,
                                control = list(adapt_delta=0.95))

saveRDS(brm_predicted_nosurp_GP_P0,"brm_predicted_nosurp_GP_P0_logadd.rds")

