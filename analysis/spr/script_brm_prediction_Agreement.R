#These scripts are run with NYU greene high performance computing service
source("../util.R")
source("../brms_parameters.R")
library(brms)
rt.data <- load_data("Agreement")
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"Agreement")
PredictedRT_df <- PredictedRT_df[PredictedRT_df$ROI%in%c(0,1,2),]
PredictedRT_df$Type[PredictedRT_df$Type == "AGREE"] <- "AGREE_G"
PredictedRT_df <- PredictedRT_df %>% separate(Type, c("Type", "pGram"), sep="_")
PredictedRT_df$pGram[PredictedRT_df$pGram == "UAMB"] <- "G"
PredictedRT_df$pGram[PredictedRT_df$pGram == "AMB"] <- "U"
PredictedRT_df$pGram[PredictedRT_df$pGram == "UNG"] <- "U"
PredictedRT_df$pGram.coded <- recode(PredictedRT_df$pGram, "U" = 1, "G" = 0)
PredictedRT_df$Type.coded <- recode(PredictedRT_df$Type, "AGREE" = 0, "NPZ" = 1)
PredictedRT_df$position.coded.1 <- recode(PredictedRT_df$ROI, "0"=0.5, "1"=0, "2"=-0.5)
PredictedRT_df$position.coded.2 <- recode(PredictedRT_df$ROI, "0"=0, "1"=0.5, "2"=-0.5)
brm_param_list <- get_brms_parameters("prior1")


prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),    
            prior("normal(0,500)", class = "sigma"))

brm_predicted_lstm_Agr_P0 <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(PredictedRT_df, Type=="AGREE"&ROI==0&model=="lstm"&!is.na(RT)),
                                 iter=brm_param_list$niters,
                                 cores=brm_param_list$ncores,
                                 warmup = brm_param_list$warmup,
                                 seed = brm_param_list$seed,
                                 prior = prior1,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_lstm_Agr_P0,"brm_predicted_lstm_Agr_P0.rds")
summary(brm_predicted_lstm_Agr_P0)
rm(brm_predicted_lstm_Agr_P0)

brm_predicted_lstm_Agr_P1 <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(PredictedRT_df, Type=="AGREE"&ROI==1&model=="lstm"&!is.na(RT)),
                                 iter=brm_param_list$niters,
                                 cores=brm_param_list$ncores,
                                 warmup = brm_param_list$warmup,
                                 seed = brm_param_list$seed,
                                 prior = prior1,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_lstm_Agr_P1,"brm_predicted_lstm_Agr_P1.rds")
summary(brm_predicted_lstm_Agr_P1)
rm(brm_predicted_lstm_Agr_P1)

brm_predicted_lstm_Agr_P2 <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(PredictedRT_df, Type=="AGREE"&ROI==2&model=="lstm"&!is.na(RT)),
                                 iter=brm_param_list$niters,
                                 cores=brm_param_list$ncores,
                                 warmup = brm_param_list$warmup,
                                 seed = brm_param_list$seed,
                                 prior = prior1,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_lstm_Agr_P2,"brm_predicted_lstm_Agr_P2.rds")
summary(brm_predicted_lstm_Agr_P2)
rm(brm_predicted_lstm_Agr_P2)




brm_predicted_gpt2_Agr_P0 <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
             data=subset(PredictedRT_df, Type=="AGREE"&ROI==0&model=="gpt2"&!is.na(RT)),
             iter=brm_param_list$niters,
             cores=brm_param_list$ncores,
             warmup = brm_param_list$warmup,
             seed = brm_param_list$seed,
             prior = prior1,
             control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_gpt2_Agr_P0,"brm_predicted_gpt2_Agr_P0.rds")
summary(brm_predicted_gpt2_Agr_P0)
rm(brm_predicted_gpt2_Agr_P0)

brm_predicted_gpt2_Agr_P1 <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(PredictedRT_df, Type=="AGREE"&ROI==1&model=="gpt2"&!is.na(RT)),
                                 iter=brm_param_list$niters,
                                 cores=brm_param_list$ncores,
                                 warmup = brm_param_list$warmup,
                                 seed = brm_param_list$seed,
                                 prior = prior1,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_gpt2_Agr_P1,"brm_predicted_gpt2_Agr_P1.rds")
summary(brm_predicted_gpt2_Agr_P1)
rm(brm_predicted_gpt2_Agr_P1)

brm_predicted_gpt2_Agr_P2 <- brm(predicted~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(PredictedRT_df, Type=="AGREE"&ROI==2&model=="gpt2"&!is.na(RT)),
                                 iter=15000,
                                 cores=brm_param_list$ncores,
                                 warmup = 7500,
                                 seed = brm_param_list$seed,
                                 prior = prior1,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_predicted_gpt2_Agr_P2,"brm_predicted_gpt2_Agr_P2.rds")
summary(brm_predicted_gpt2_Agr_P2)
rm(brm_predicted_gpt2_Agr_P2)