#These scripts are run with NYU greene high performance computing service
source("util_logadd.R")
source('brms_parameters_log.R')

brm_param_list <- get_brms_parameters()
rt.data <- load_data("Agreement")
rt.data <- rt.data[rt.data$ROI%in%c(0,1,2),]
rt.data$Type <- as.character(rt.data$Type)
rt.data$Type[rt.data$Type == "AGREE"] <- "AGREE_G"
rt.data <- rt.data %>% separate(Type, c("Type", "pGram"), sep="_")
rt.data$pGram[rt.data$pGram == "UAMB"] <- "G"
rt.data$pGram[rt.data$pGram == "AMB"] <- "U"
rt.data$pGram[rt.data$pGram == "UNG"] <- "U"
rt.data$pGram.coded <- recode(rt.data$pGram, "U" = 1, "G" = 0)
rt.data$Type.coded <- recode(rt.data$Type, "AGREE" = 0, "NPZ" = 1)
rt.data$position.coded.1 <- recode(rt.data$ROI, "0"=0.5, "1"=0, "2"=-0.5)
rt.data$position.coded.2 <- recode(rt.data$ROI, "0"=0, "1"=0.5, "2"=-0.5)
rt.data$RT_log <- ifelse(rt.data$RT==0,1,rt.data$RT)
rt.data$RT_log <- log(rt.data$RT_log)


brm_Agr_P0_log <- brm(RT_log~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(rt.data, Type=="AGREE"&ROI==0&!is.na(RT)),
                                 iter=16000,
                                 cores=brm_param_list$ncores,
                                 warmup = 8000,
                                 seed = brm_param_list$seed,
                                 prior = brm_param_list$prior,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_Agr_P0_log,"brm_Agr_P0_log.rds")
summary(brm_Agr_P0_log)
rm(brm_Agr_P0_log)

brm_Agr_P1_log <- brm(RT_log~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(rt.data, Type=="AGREE"&ROI==1&!is.na(RT)),
                                 iter=16000,
                                 cores=brm_param_list$ncores,
                                 warmup = 8000,
                                 seed = brm_param_list$seed,
                                 prior = brm_param_list$prior,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_Agr_P1_log,"brm_Agr_P1_log.rds")
summary(brm_Agr_P1_log)
rm(brm_Agr_P1_log)

brm_Agr_P2_log <- brm(RT_log~pGram.coded+(1+pGram.coded||item)+(1+pGram.coded||participant),
                                 data=subset(rt.data, Type=="AGREE"&ROI==2&!is.na(RT)),
                                 iter=16000,
                                 cores=brm_param_list$ncores,
                                 warmup = 8000,
                                 seed = brm_param_list$seed,
                                 prior = brm_param_list$prior,
                                 control = list(adapt_delta=brm_param_list$adapt_delta))

saveRDS(brm_Agr_P2_log,"brm_Agr_P2_log.rds")
summary(brm_Agr_P2_log)
rm(brm_Agr_P2_log)