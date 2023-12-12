#These scripts are run with NYU greene high performance computing service
source("util_logadd.R")
source("brms_parameters_log.R")
rt.data <- load_data("Agreement")
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"Agreement")
PredictedRT_df[,ncol(PredictedRT_df)]
PredictedRT_df <- PredictedRT_df[PredictedRT_df$ROI%in%c(0,1,2),]
PredictedRT_df$Type <- as.character(PredictedRT_df$Type)
PredictedRT_df$Type[PredictedRT_df$Type == "AGREE"] <- "AGREE_G"
PredictedRT_df <- PredictedRT_df %>% separate(Type, c("Type", "pGram"), sep="_")
PredictedRT_df$pGram[PredictedRT_df$pGram == "UAMB"] <- "G"
PredictedRT_df$pGram[PredictedRT_df$pGram == "AMB"] <- "U"
PredictedRT_df$pGram[PredictedRT_df$pGram == "UNG"] <- "U"
PredictedRT_df$pGram.coded <- recode(PredictedRT_df$pGram, "U" = 1, "G" = 0)
PredictedRT_df$Type.coded <- recode(PredictedRT_df$Type, "AGREE" = 0, "NPZ" = 1)
PredictedRT_df$position.coded.1 <- recode(PredictedRT_df$ROI, "0"=0.5, "1"=0, "2"=-0.5)
PredictedRT_df$position.coded.2 <- recode(PredictedRT_df$ROI, "0"=0, "1"=0.5, "2"=-0.5)
brm_param_list <- get_brms_parameters()




brm_predicted_nosurp_Agr_P0_log <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                       data=subset(PredictedRT_df, Type=="AGREE"&ROI==0&model=="nosurp"&!is.na(RT)),
                                       iter=34000,
                                       cores=brm_param_list$ncores,
                                       warmup = 17000,
                                       seed = brm_param_list$seed,
                                       prior = brm_param_list$prior,
                                       control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_nosurp_Agr_P0_log,"brm_predicted_nosurp_Agr_P0_logadd.rds")
summary(brm_predicted_nosurp_Agr_P0_log)
rm(brm_predicted_nosurp_Agr_P0_log)

brm_predicted_nosurp_Agr_P1_log <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                       data=subset(PredictedRT_df, Type=="AGREE"&ROI==1&model=="nosurp"&!is.na(RT)),
                                       iter=34000,
                                       cores=brm_param_list$ncores,
                                       warmup = 17000,
                                       seed = brm_param_list$seed,
                                       prior = brm_param_list$prior,
                                       control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_nosurp_Agr_P1_log,"brm_predicted_nosurp_Agr_P1_logadd.rds")
summary(brm_predicted_nosurp_Agr_P1_log)
rm(brm_predicted_nosurp_Agr_P1_log)

brm_predicted_nosurp_Agr_P2_log <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                       data=subset(PredictedRT_df, Type=="AGREE"&ROI==2&model=="nosurp"&!is.na(RT)),
                                       iter=34000,
                                       cores=brm_param_list$ncores,
                                       warmup = 17000,
                                       seed = brm_param_list$seed,
                                       prior = brm_param_list$prior,
                                       control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_nosurp_Agr_P2_log,"brm_predicted_nosurp_Agr_P2_logadd.rds")
summary(brm_predicted_nosurp_Agr_P2_log)
rm(brm_predicted_nosurp_Agr_P2_log)