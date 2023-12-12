source("util.R")
#AttachmentAmbiguity
fit_P0 <- readRDS("AA_logadd_prior1/brm_prior1_AA_emp_0_log.rds")
fit_P1 <- readRDS("AA_logadd_prior1/brm_prior1_AA_emp_1_log.rds")
fit_P2 <- readRDS("AA_logadd_prior1/brm_prior1_AA_emp_2_log.rds")
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
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#sum coefficients rowwise to recover the effects of interest for each iteration (sampling)
by_item <- reshape_item_output %>%
  mutate(GPE_high = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity+(1/2)*b_height-(1/3)*r_ambiguity+(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity),
         GPE_low = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity-(1/2)*b_height-(1/3)*r_ambiguity-(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity))  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = exp(b_Intercept-(1/3)*b_ambiguity+(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity),
         GPE_low = exp(b_Intercept-(1/3)*b_ambiguity-(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity)) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item,"AA_logadd_prior1/by_item_log.rds")
saveRDS(by_construction,"AA_logadd_prior1/by_construction_log.rds")
#do the same thing for lstm+, gpt2+, nosurprisal
#lstm
fit_P0 <- readRDS("AA_logadd_prior1/brm_prior1_pred_lstm_0_logadd.rds")
fit_P1 <- readRDS("AA_logadd_prior1/brm_prior1_pred_lstm_1_logadd.rds")
fit_P2 <- readRDS("AA_logadd_prior1/brm_prior1_pred_lstm_2_logadd.rds")
summary(fit_P0)
fit_P0$prior
summary(fit_P1)
fit_P1$prior
summary(fit_P2)
fit_P2$prior
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_lstm <- reshape_item_output %>%
  mutate(GPE_high = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity+(1/2)*b_height-(1/3)*r_ambiguity+(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity),
         GPE_low = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity-(1/2)*b_height-(1/3)*r_ambiguity-(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity))  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_lstm <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = exp(b_Intercept-(1/3)*b_ambiguity+(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity),
         GPE_low = exp(b_Intercept-(1/3)*b_ambiguity-(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity)) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_lstm,"AA_logadd_prior1/by_item_lstm_logadd.rds")
saveRDS(by_construction_lstm,"AA_logadd_prior1/by_construction_lstm_logadd.rds")
fit_P0 <- readRDS("AA_logadd_prior1/brm_prior1_pred_gpt2_0_logadd.rds")
fit_P1 <- readRDS("AA_logadd_prior1/brm_prior1_pred_gpt2_1_logadd.rds")
fit_P2 <- readRDS("AA_logadd_prior1/brm_prior1_pred_gpt2_2_logadd.rds")
summary(fit_P0)
fit_P0$prior
summary(fit_P1)
fit_P1$prior
summary(fit_P2)
fit_P2$prior
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_gpt2 <- reshape_item_output %>%
  mutate(GPE_high = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity+(1/2)*b_height-(1/3)*r_ambiguity+(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity),
         GPE_low = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity-(1/2)*b_height-(1/3)*r_ambiguity-(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity))  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_gpt2 <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = exp(b_Intercept-(1/3)*b_ambiguity+(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity),
         GPE_low = exp(b_Intercept-(1/3)*b_ambiguity-(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity)) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_gpt2,"AA_logadd_prior1/by_item_gpt2_logadd.rds")
saveRDS(by_construction_gpt2,"AA_logadd_prior1/by_construction_gpt2_logadd.rds")
#nosurp
fit_P0 <- readRDS("AA_logadd_prior1/brm_prior1_pred_nosurp_0_logadd.rds")
fit_P1 <- readRDS("AA_logadd_prior1/brm_prior1_pred_nosurp_1_logadd.rds")
fit_P2 <- readRDS("AA_logadd_prior1/brm_prior1_pred_nosurp_2_logadd.rds")
summary(fit_P0)
fit_P0$prior
summary(fit_P1)
fit_P1$prior
summary(fit_P2)
fit_P2$prior
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_nosurp <- reshape_item_output %>%
  mutate(GPE_high = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity+(1/2)*b_height-(1/3)*r_ambiguity+(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity),
         GPE_low = exp(b_Intercept+r_Intercept-(1/3)*b_ambiguity-(1/2)*b_height-(1/3)*r_ambiguity-(1/2)*r_height)-exp(b_Intercept+r_Intercept+(2/3)*b_ambiguity+(2/3)*r_ambiguity))  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_nosurp <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = exp(b_Intercept-(1/3)*b_ambiguity+(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity),
         GPE_low = exp(b_Intercept-(1/3)*b_ambiguity-(1/2)*b_height)-exp(b_Intercept+(2/3)*b_ambiguity)) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_nosurp,"AA_logadd_prior1/by_item_nosurp_logadd.rds")
saveRDS(by_construction_nosurp,"AA_logadd_prior1/by_construction_nosurp_logadd.rds")
