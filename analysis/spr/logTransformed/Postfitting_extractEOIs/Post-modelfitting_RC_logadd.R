source("util.R")
fit_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_emp_prior1.rds")
fit_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_emp_prior1.rds")
fit_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_emp_prior1.rds")
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
  mutate(RC = exp(b_Intercept+r_Intercept+b_Type_num + r_Type_num)- exp(b_Intercept+r_Intercept)) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = exp(b_Intercept+b_Type_num)-exp(b_Intercept)) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item,"RC_logadd_prior1/by_item_log.rds")
saveRDS(by_construction,"RC_logadd_prior1/by_construction_log.rds")
#do the same thing for lstm+, gpt2+, nosurprisal
#lstm
fit_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_pred_lstm_prior1_logadd.rds")
fit_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_pred_lstm_prior1_logadd.rds")
fit_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_pred_lstm_prior1_logadd.rds")
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
  mutate(RC = exp(b_Intercept+r_Intercept+b_Type_num + r_Type_num)- exp(b_Intercept+r_Intercept)) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction_lstm <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = exp(b_Intercept+b_Type_num)-exp(b_Intercept)) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_lstm,"RC_logadd_prior1/by_item_lstm_logadd.rds")
saveRDS(by_construction_lstm,"RC_logadd_prior1/by_construction_lstm_logadd.rds")
fit_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_pred_gpt2_prior1_logadd.rds")
fit_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_pred_gpt2_prior1_logadd.rds")
fit_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_pred_gpt2_prior1_logadd.rds")
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
  mutate(RC = exp(b_Intercept+r_Intercept+b_Type_num + r_Type_num)- exp(b_Intercept+r_Intercept)) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction_gpt2 <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = exp(b_Intercept+b_Type_num)-exp(b_Intercept)) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_gpt2,"RC_logadd_prior1/by_item_gpt2_logadd.rds")
saveRDS(by_construction_gpt2,"RC_logadd_prior1/by_construction_gpt2_logadd.rds")
#nosurp
fit_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_pred_nosurp_prior1_logadd.rds")
fit_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_pred_nosurp_prior1_logadd.rds")
fit_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_pred_nosurp_prior1_logadd.rds")
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
  mutate(RC = exp(b_Intercept+r_Intercept+b_Type_num + r_Type_num)- exp(b_Intercept+r_Intercept)) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction_nosurp <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = exp(b_Intercept+b_Type_num)-exp(b_Intercept)) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
saveRDS(by_item_nosurp,"RC_logadd_prior1/by_item_nosurp_logadd.rds")
saveRDS(by_construction_nosurp,"RC_logadd_prior1/by_construction_nosurp_logadd.rds")
