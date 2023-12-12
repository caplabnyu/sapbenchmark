source("util.R")
fit_P0 <- readRDS("GP_logadd_prior1/ClassicGP_lognorm_prior_0.rds")
fit_P1 <- readRDS("GP_logadd_prior1/ClassicGP_lognorm_prior_1.rds")
fit_P2 <- readRDS("GP_logadd_prior1/ClassicGP_lognorm_prior_2.rds")
#get posterior estimates from the iteration (sampling)
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
#concatenate sampling of different ROIs
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#sum coefficients rowwise to recover the effects of interest for each iteration (sampling)
by_item <- reshape_item_output %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB+r_Intercept+r_AMBUAMB)-exp(b_Intercept+r_Intercept),
         GPE_NPS = exp(b_Intercept + b_SZM1 + b_AMBUAMB + `b_AMBUAMB:SZM1` + r_Intercept + r_SZM1 + r_AMBUAMB + `r_AMBUAMB:SZM1`) - exp(b_Intercept + b_SZM1 + r_Intercept + r_SZM1),
         GPE_NPZ = exp(b_Intercept + b_SZM2 + b_AMBUAMB + `b_AMBUAMB:SZM2` + r_Intercept + r_SZM2 + r_AMBUAMB + `r_AMBUAMB:SZM2`) - exp(b_Intercept + b_SZM2 + r_Intercept + r_SZM2)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB)-exp(b_Intercept),
         GPE_NPS = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM1`)-exp(b_Intercept+b_SZM1),
         GPE_NPZ = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM2`)-exp(b_Intercept+b_SZM2)) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item,"GP_logadd_prior1/by_item_log.rds")
saveRDS(by_construction,"GP_logadd_prior1/by_construction_log.rds")


fit_P0 <- readRDS("GP_logadd_prior1/brm_predicted_lstm_GP_P0_logadd.rds")
fit_P1 <- readRDS("GP_logadd_prior1/brm_predicted_lstm_GP_P1_logadd.rds")
fit_P2 <- readRDS("GP_logadd_prior1/brm_predicted_lstm_GP_P2_logadd.rds")
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
print(head(reshape_item_output_P0))
nrow(reshape_item_output_P0)
ncol(reshape_item_output_P0)
print(head(reshape_item_output_P1))
nrow(reshape_item_output_P1)
ncol(reshape_item_output_P1)
print(head(reshape_item_output_P2))
nrow(reshape_item_output_P2)
ncol(reshape_item_output_P2)
#concatenate sampling of different ROIs
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),times=c(nrow(reshape_item_output_P0),nrow(reshape_item_output_P1),nrow(reshape_item_output_P2))))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#sum coefficients rowwise to recover the effects of interest for each iteration (sampling)
by_item <- reshape_item_output %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB+r_Intercept+r_AMBUAMB)-exp(b_Intercept+r_Intercept),
         GPE_NPS = exp(b_Intercept + b_SZM1 + b_AMBUAMB + `b_AMBUAMB:SZM1` + r_Intercept + r_SZM1 + r_AMBUAMB + `r_AMBUAMB:SZM1`) - exp(b_Intercept + b_SZM1 + r_Intercept + r_SZM1),
         GPE_NPZ = exp(b_Intercept + b_SZM2 + b_AMBUAMB + `b_AMBUAMB:SZM2` + r_Intercept + r_SZM2 + r_AMBUAMB + `r_AMBUAMB:SZM2`) - exp(b_Intercept + b_SZM2 + r_Intercept + r_SZM2)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB)-exp(b_Intercept),
         GPE_NPS = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM1`)-exp(b_Intercept+b_SZM1),
         GPE_NPZ = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM2`)-exp(b_Intercept+b_SZM2)) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item,"GP_logadd_prior1/by_item_logadd.rds")
saveRDS(by_construction,"GP_logadd_prior1/by_construction_lstm_logadd.rds")

fit_P0 <- readRDS("GP_logadd_prior1/brm_predicted_gpt2_GP_P0_logadd.rds")
fit_P1 <- readRDS("GP_logadd_prior1/brm_predicted_gpt2_GP_P1_logadd.rds")
fit_P2 <- readRDS("GP_logadd_prior1/brm_predicted_gpt2_GP_P2_logadd.rds")
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
print(head(reshape_item_output_P0))
nrow(reshape_item_output_P0)
ncol(reshape_item_output_P0)
print(head(reshape_item_output_P1))
nrow(reshape_item_output_P1)
ncol(reshape_item_output_P1)
print(head(reshape_item_output_P2))
nrow(reshape_item_output_P2)
ncol(reshape_item_output_P2)
#concatenate sampling of different ROIs
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),times=c(nrow(reshape_item_output_P0),nrow(reshape_item_output_P1),nrow(reshape_item_output_P2))))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#sum coefficients rowwise to recover the effects of interest for each iteration (sampling)
by_item <- reshape_item_output %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB+r_Intercept+r_AMBUAMB)-exp(b_Intercept+r_Intercept),
         GPE_NPS = exp(b_Intercept + b_SZM1 + b_AMBUAMB + `b_AMBUAMB:SZM1` + r_Intercept + r_SZM1 + r_AMBUAMB + `r_AMBUAMB:SZM1`) - exp(b_Intercept + b_SZM1 + r_Intercept + r_SZM1),
         GPE_NPZ = exp(b_Intercept + b_SZM2 + b_AMBUAMB + `b_AMBUAMB:SZM2` + r_Intercept + r_SZM2 + r_AMBUAMB + `r_AMBUAMB:SZM2`) - exp(b_Intercept + b_SZM2 + r_Intercept + r_SZM2)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB)-exp(b_Intercept),
         GPE_NPS = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM1`)-exp(b_Intercept+b_SZM1),
         GPE_NPZ = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM2`)-exp(b_Intercept+b_SZM2)) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item,"GP_logadd_prior1/by_item_gpt2_logadd.rds")
saveRDS(by_construction,"GP_logadd_prior1/by_construction_gpt2_logadd.rds")


fit_P0 <- readRDS("GP_logadd_prior1/brm_predicted_nosurp_GP_P0_logadd.rds")
fit_P1 <- readRDS("GP_logadd_prior1/brm_predicted_nosurp_GP_P1_logadd.rds")
fit_P2 <- readRDS("GP_logadd_prior1/brm_predicted_nosurp_GP_P2_logadd.rds")
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
print(head(reshape_item_output_P0))
nrow(reshape_item_output_P0)
ncol(reshape_item_output_P0)
print(head(reshape_item_output_P1))
nrow(reshape_item_output_P1)
ncol(reshape_item_output_P1)
print(head(reshape_item_output_P2))
nrow(reshape_item_output_P2)
ncol(reshape_item_output_P2)
#concatenate sampling of different ROIs
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),times=c(nrow(reshape_item_output_P0),nrow(reshape_item_output_P1),nrow(reshape_item_output_P2))))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#sum coefficients rowwise to recover the effects of interest for each iteration (sampling)
by_item <- reshape_item_output %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB+r_Intercept+r_AMBUAMB)-exp(b_Intercept+r_Intercept),
         GPE_NPS = exp(b_Intercept + b_SZM1 + b_AMBUAMB + `b_AMBUAMB:SZM1` + r_Intercept + r_SZM1 + r_AMBUAMB + `r_AMBUAMB:SZM1`) - exp(b_Intercept + b_SZM1 + r_Intercept + r_SZM1),
         GPE_NPZ = exp(b_Intercept + b_SZM2 + b_AMBUAMB + `b_AMBUAMB:SZM2` + r_Intercept + r_SZM2 + r_AMBUAMB + `r_AMBUAMB:SZM2`) - exp(b_Intercept + b_SZM2 + r_Intercept + r_SZM2)
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = exp(b_Intercept+b_AMBUAMB)-exp(b_Intercept),
         GPE_NPS = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM1`)-exp(b_Intercept+b_SZM1),
         GPE_NPZ = exp(b_Intercept+b_SZM1+b_AMBUAMB+`b_AMBUAMB:SZM2`)-exp(b_Intercept+b_SZM2)) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item,"GP_logadd_prior1/by_item_nosurp_logadd.rds")
saveRDS(by_construction,"GP_logadd_prior1/by_construction_nosurp_logadd.rds")