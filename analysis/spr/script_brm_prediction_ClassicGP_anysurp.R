#These scripts are run with NYU greene high performance computing service
source("../shared/util.R")
source("../shared/brms_parameters.R")
library(brms)
args <- commandArgs(trailingOnly=TRUE)
rt.data <- load_data("ClassicGP")
rt.data$SZM1 <- ifelse(rt.data$CONSTRUCTION=="NPS",1,0)
rt.data$SZM2 <- ifelse(rt.data$CONSTRUCTION=="NPZ",1,0)
PredictedRT_df <- Predicting_RT_with_spillover_anysurp(rt.data, "ClassicGP", args[1])
brm_param_list <- get_brms_parameters("prior1")


prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),    
            prior("normal(0,500)", class = "sigma"))

print("Fitting brm model for S0...")
brm_predicted_P0 <- brm(predicted~AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1|participant),
                                data=subset(PredictedRT_df, ROI==0&model==args[1]&!is.na(RT)),
                                iter=24000,
                                cores=brm_param_list$ncores,
                                warmup = 12000,
                                seed = brm_param_list$seed,
                                prior = prior1,
                                control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_P0, paste0("brm_models/brm_predicted_", args[1], "_ClassicGP_P0.rds"))

print("Fitting brm model for S1...")
brm_predicted_P1 <- brm(predicted~AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1|participant),
                                data=subset(PredictedRT_df, ROI==1&model==args[1]&!is.na(RT)),
                                iter=30000,
                                cores=brm_param_list$ncores,
                                warmup = 15000,
                                seed = brm_param_list$seed,
                                prior = prior1,
                                control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_P1, paste0("brm_models/brm_predicted_", args[1], "_ClassicGP_P1.rds"))

print("Fitting brm model for S2...")
brm_predicted_P2 <- brm(predicted~AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1|participant),
             data=subset(PredictedRT_df, ROI==2&model==args[1]&!is.na(RT)),
             iter=brm_param_list$niters,
             cores=brm_param_list$ncores,
             warmup = brm_param_list$warmup,
             seed = brm_param_list$seed,
             prior = prior1,
             control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_P2, paste0("brm_models/brm_predicted_", args[1], "_ClassicGP_P2.rds"))
