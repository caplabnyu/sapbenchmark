source("util.R")
#AttachmentAmbiguity
fit_P0 <- readRDS("Agr_logadd_prior1/brm_Agr_P0_log.rds")
fit_P1 <- readRDS("Agr_logadd_prior1/brm_Agr_P1_log.rds")
fit_P2 <- readRDS("Agr_logadd_prior1/brm_Agr_P2_log.rds")
summary(fit_P0)
fit_P0$prior
summary(fit_P1)
fit_P1$prior
summary(fit_P2)
fit_P2$prior
#get posterior estimates from the iteration (sampling)
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
#concatenate sampling of different ROIs
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),times=c(nrow(reshape_item_output_P0),nrow(reshape_item_output_P1),nrow(reshape_item_output_P2))))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#sum coefficients rowwise to recover the effects of interest for each iteration (sampling)
by_item <- reshape_item_output %>%
  mutate(Agr = exp(b_Intercept+r_Intercept+b_pGram.coded + r_pGram.coded)-exp(b_Intercept+r_Intercept)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = exp(b_Intercept+b_pGram.coded)-exp(b_Intercept)) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item,"Agr_logadd_prior1/by_item_log.rds")
saveRDS(by_construction,"Agr_logadd_prior1/by_construction_log.rds")
#do the same thing for lstm+, gpt2+, nosurprisal
#lstm
fit_P0 <- readRDS("Agr_logadd_prior1/brm_predicted_lstm_Agr_P0_logadd.rds")
fit_P1 <- readRDS("Agr_logadd_prior1/brm_predicted_lstm_Agr_P1_logadd.rds")
fit_P2 <- readRDS("Agr_logadd_prior1/brm_predicted_lstm_Agr_P2_logadd.rds")
summary(fit_P0)
fit_P0$prior
summary(fit_P1)
fit_P1$prior
summary(fit_P2)
fit_P2$prior
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),times=c(nrow(reshape_item_output_P0),nrow(reshape_item_output_P1),nrow(reshape_item_output_P2))))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_lstm <- reshape_item_output %>%
  mutate(Agr = exp(b_Intercept+r_Intercept+b_pGram.coded + r_pGram.coded)-exp(b_Intercept+r_Intercept)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction_lstm <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = exp(b_Intercept+b_pGram.coded)-exp(b_Intercept)) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_lstm,"Agr_logadd_prior1/by_item_lstm_logadd.rds")
saveRDS(by_construction_lstm,"Agr_logadd_prior1/by_construction_lstm_logadd.rds")
fit_P0 <- readRDS("Agr_logadd_prior1/brm_predicted_gpt2_Agr_P0_logadd.rds")
fit_P1 <- readRDS("Agr_logadd_prior1/brm_predicted_gpt2_Agr_P1_logadd.rds")
fit_P2 <- readRDS("Agr_logadd_prior1/brm_predicted_gpt2_Agr_P2_logadd.rds")
summary(fit_P0)
fit_P0$prior
summary(fit_P1)
fit_P1$prior
summary(fit_P2)
fit_P2$prior
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),times=c(nrow(reshape_item_output_P0),nrow(reshape_item_output_P1),nrow(reshape_item_output_P2))))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_gpt2 <- reshape_item_output %>%
  mutate(Agr = exp(b_Intercept+r_Intercept+b_pGram.coded + r_pGram.coded)-exp(b_Intercept+r_Intercept)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction_gpt2 <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = exp(b_Intercept+b_pGram.coded)-exp(b_Intercept)) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_gpt2,"Agr_logadd_prior1/by_item_gpt2_logadd.rds")
saveRDS(by_construction_gpt2,"Agr_logadd_prior1/by_construction_gpt2_logadd.rds")
#nosurp
fit_P0 <- readRDS("Agr_logadd_prior1/brm_predicted_nosurp_Agr_P0_logadd.rds")
fit_P1 <- readRDS("Agr_logadd_prior1/brm_predicted_nosurp_Agr_P1_logadd.rds")
fit_P2 <- readRDS("Agr_logadd_prior1/brm_predicted_nosurp_Agr_P2_logadd.rds")
summary(fit_P0)
fit_P0$prior
summary(fit_P1)
fit_P1$prior
summary(fit_P2)
fit_P2$prior
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),times=c(nrow(reshape_item_output_P0),nrow(reshape_item_output_P1),nrow(reshape_item_output_P2))))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_nosurp <- reshape_item_output %>%
  mutate(Agr = exp(b_Intercept+r_Intercept+b_pGram.coded + r_pGram.coded)-exp(b_Intercept+r_Intercept)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction_nosurp <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = exp(b_Intercept+b_pGram.coded)-exp(b_Intercept)) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_nosurp,"Agr_logadd_prior1/by_item_nosurp_logadd.rds")
saveRDS(by_construction_nosurp,"Agr_logadd_prior1/by_construction_nosurp_logadd.rds")
