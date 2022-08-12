source("util.R")
rt.data <- load_data("ClassicGP")
fit_P0 <- readRDS("prior1_fit_P0.rds")
fit_P1 <- readRDS("prior1_fit_P1.rds")
fit_P2 <- readRDS("prior1_fit_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#ClassicGP
by_item <- reshape_item_output %>%
  mutate(GPE_MVRR = b_AMBUAMB + r_AMBUAMB,
         GPE_NPS = b_AMBUAMB + `b_AMBUAMB:SZM1` + r_AMBUAMB + `r_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB + `b_AMBUAMB:SZM2` + r_AMBUAMB + `r_AMBUAMB:SZM2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = b_AMBUAMB,
         GPE_NPS = b_AMBUAMB+`b_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB+`b_AMBUAMB:SZM2`) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item,"ClassicGP/by_item.rds")
#saveRDS(by_construction,"ClassicGP/by_construction.rds")
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"ClassicGP")
#lstm
fit_P0 <- readRDS("brm_predicted_lstm_GP_P0.rds")
fit_P1 <- readRDS("brm_predicted_lstm_GP_P1.rds")
fit_P2 <- readRDS("brm_predicted_lstm_GP_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#ClassicGP
by_item_lstm <- reshape_item_output %>%
  mutate(GPE_MVRR = b_AMBUAMB + r_AMBUAMB,
         GPE_NPS = b_AMBUAMB + `b_AMBUAMB:SZM1` + r_AMBUAMB + `r_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB + `b_AMBUAMB:SZM2` + r_AMBUAMB + `r_AMBUAMB:SZM2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_lstm <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = b_AMBUAMB,
         GPE_NPS = b_AMBUAMB+`b_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB+`b_AMBUAMB:SZM2`) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_lstm,"ClassicGP/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"ClassicGP/by_construction_lstm.rds")
#gpt2
fit_P0 <- readRDS("brm_predicted_gpt2_GP_P0.rds")
fit_P1 <- readRDS("brm_predicted_gpt2_GP_P1.rds")
fit_P2 <- readRDS("brm_predicted_gpt2_GP_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#ClassicGP
by_item_gpt2 <- reshape_item_output %>%
  mutate(GPE_MVRR = b_AMBUAMB + r_AMBUAMB,
         GPE_NPS = b_AMBUAMB + `b_AMBUAMB:SZM1` + r_AMBUAMB + `r_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB + `b_AMBUAMB:SZM2` + r_AMBUAMB + `r_AMBUAMB:SZM2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  gather(key = 'coef', value = 'val', GPE_MVRR, GPE_NPS, GPE_NPZ) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_gpt2 <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = b_AMBUAMB,
         GPE_NPS = b_AMBUAMB+`b_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB+`b_AMBUAMB:SZM2`) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_gpt2,"ClassicGP/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"ClassicGP/by_construction_gpt2.rds")
Plot_empirical_construction_level(by_construction,"ClassicGP")
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=0)
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=1)
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=2)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,1)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,2)
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"ClassicGP")

###
#RelativeClause
rt.data <- load_data("RelativeClause")
rt.data <- rt.data %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))
fit_P0 <- readRDS("fit_verb_bayes_prior1.RDS")
fit_P1 <- readRDS("fit_det_bayes_prior1.RDS")
fit_P2 <- readRDS("fit_noun_bayes_prior1.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item <- reshape_item_output %>%
  mutate(RC = b_Type_num + r_Type_num) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = b_Type_num) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item,"RelativeClause/by_item.rds")
#saveRDS(by_construction,"RelativeClause/by_construction.rds")
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"RelativeClause")
#lstm
fit_P0 <- readRDS("fit_verb_bayes_pred_lstm_prior1.RDS")
fit_P1 <- readRDS("fit_det_bayes_pred_lstm_prior1.RDS")
fit_P2 <- readRDS("fit_noun_bayes_pred_lstm_prior1.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_lstm <- reshape_item_output %>%
  mutate(RC = b_Type_num + r_Type_num) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction_lstm <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = b_Type_num) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_lstm,"RelativeClause/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"RelativeClause/by_construction_lstm.rds")
#gpt2
fit_P0 <- readRDS("fit_verb_bayes_pred_gpt2_prior1.RDS")
fit_P1 <- readRDS("fit_det_bayes_pred_gpt2_prior1.RDS")
fit_P2 <- readRDS("fit_noun_bayes_pred_gpt2_prior1.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_gpt2 <- reshape_item_output %>%
  mutate(RC = b_Type_num + r_Type_num) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction_gpt2 <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = b_Type_num) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_gpt2,"RelativeClause/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"RelativeClause/by_construction_gpt2.rds")
Plot_empirical_construction_level(by_construction,"RelativeClause")
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=0)
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=1)
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=2)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"RelativeClause")


###
#AttachmentAmbiguity
rt.data <- load_data("AttachmentAmbiguity")
rt.data$ambiguity <- ifelse(
  rt.data$AMBIG=="Amb",2/3,-1/3)
rt.data <- rt.data %>% 
  mutate(height = case_when(Type=="AttachMulti" ~ 0, 
                            Type=="AttachLow" ~ -1/2,
                            Type=="AttachHigh" ~ 1/2))
fit_P0 <- readRDS("brm_prior1_0_emp.RDS")
fit_P1 <- readRDS("brm_prior1_1_emp.RDS")
fit_P2 <- readRDS("brm_prior1_2_emp.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item <- reshape_item_output %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height -r_ambiguity + (1/2)*r_height,
         GPE_low = -b_ambiguity - (1/2)*b_height -r_ambiguity - (1/2)*r_height)  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height,
         GPE_low = -b_ambiguity - (1/2)*b_height) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item,"AttachmentAmbiguity/by_item.rds")
#saveRDS(by_construction,"AttachmentAmbiguity/by_construction.rds")
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"AttachmentAmbiguity")
#lstm
fit_P0 <- readRDS("brm_prior1_pred_lstm_0.RDS")
fit_P1 <- readRDS("brm_prior1_pred_lstm_1.RDS")
fit_P2 <- readRDS("brm_prior1_pred_lstm_2.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_lstm <- reshape_item_output %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height -r_ambiguity + (1/2)*r_height,
         GPE_low = -b_ambiguity - (1/2)*b_height -r_ambiguity - (1/2)*r_height)  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_lstm <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height,
         GPE_low = -b_ambiguity - (1/2)*b_height) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_lstm,"AttachmentAmbiguity/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"AttachmentAmbiguity/by_construction_lstm.rds")
#gpt2
fit_P0 <- readRDS("brm_prior1_pred_gpt2_0.RDS")
fit_P1 <- readRDS("brm_prior1_pred_gpt2_1.RDS")
fit_P2 <- readRDS("brm_prior1_pred_gpt2_2.RDS")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_gpt2 <- reshape_item_output %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height -r_ambiguity + (1/2)*r_height,
         GPE_low = -b_ambiguity - (1/2)*b_height -r_ambiguity - (1/2)*r_height)  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_gpt2 <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height,
         GPE_low = -b_ambiguity - (1/2)*b_height) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_gpt2,"AttachmentAmbiguity/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"AttachmentAmbiguity/by_construction_gpt2.rds")
Plot_empirical_construction_level(by_construction,"AttachmentAmbiguity")
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=0)
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=1)
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=2)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)


###
## Agreement subset
rt.data <- load_data("Agreement")
fit <- readRDS("brm_RT_Agr.RDS")
reshape_item_output <- reshape_item_dat(fit, 'item')
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit)
by_item <- reshape_item_output %>%
  mutate(Agr = b_pGram.coded + r_pGram.coded,
         Agr_0 = Agr + 0.5 * `b_pGram.coded:position.coded.1` + 0.5 * `r_pGram.coded:position.coded.1`,
         Agr_1 = Agr + 0.5 * `b_pGram.coded:position.coded.2` + 0.5 * `r_pGram.coded:position.coded.2`,
         Agr_2 = Agr - 0.5 * `b_pGram.coded:position.coded.1` - 0.5 * `b_pGram.coded:position.coded.2` - 0.5 * `r_pGram.coded:position.coded.1`- 0.5 * `r_pGram.coded:position.coded.2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, Agr_0, Agr_1, Agr_2) %>%
  gather(key = 'coef', value = 'val', Agr_0, Agr_1, Agr_2) %>%
  group_by(item, coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])%>%separate(coef,c('coef','ROI'),sep='_')
by_construction <- reshape_item_output[,c('.draw',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr_0 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.1`,
         Agr_1 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.2`,
         Agr_2 = b_pGram.coded - 0.5 * `b_pGram.coded:position.coded.1`- 0.5 * `b_pGram.coded:position.coded.2`) %>%
  select(Agr_0,Agr_1,Agr_2) %>% gather(key='coef',value='val')%>%group_by(coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])%>%separate(coef, c('coef', 'ROI'), sep= '_')
#saveRDS(by_item,"Agreement/by_item.rds")
#saveRDS(by_construction,"Agreement/by_construction.rds")
Plot_empirical_construction_level(by_construction,"Agreement")
#lstm
fit <- readRDS("brm_predicted_lstm_Agr.RDS")
reshape_item_output <- reshape_item_dat(fit, 'item')
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit)
by_item_lstm <- reshape_item_output %>%
  mutate(Agr = b_pGram.coded + r_pGram.coded,
         Agr_0 = Agr + 0.5 * `b_pGram.coded:position.coded.1` + 0.5 * `r_pGram.coded:position.coded.1`,
         Agr_1 = Agr + 0.5 * `b_pGram.coded:position.coded.2` + 0.5 * `r_pGram.coded:position.coded.2`,
         Agr_2 = Agr - 0.5 * `b_pGram.coded:position.coded.1` - 0.5 * `b_pGram.coded:position.coded.2` - 0.5 * `r_pGram.coded:position.coded.1`- 0.5 * `r_pGram.coded:position.coded.2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, Agr_0, Agr_1, Agr_2) %>%
  gather(key = 'coef', value = 'val', Agr_0, Agr_1, Agr_2) %>%
  group_by(item, coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])%>%separate(coef,c('coef','ROI'),sep='_')
by_construction_lstm <- reshape_item_output[,c('.draw',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr_0 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.1`,
         Agr_1 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.2`,
         Agr_2 = b_pGram.coded - 0.5 * `b_pGram.coded:position.coded.1`- 0.5 * `b_pGram.coded:position.coded.2`) %>%
  select(Agr_0,Agr_1,Agr_2) %>% gather(key='coef',value='val')%>%group_by(coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])%>%separate(coef, c('coef', 'ROI'), sep= '_')
#saveRDS(by_item_lstm,"Agreement/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"Agreement/by_construction_lstm.rds")
#gpt2
fit <- readRDS("brm_predicted_gpt2_Agr.RDS")
reshape_item_output <- reshape_item_dat(fit, 'item')
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit)
by_item_gpt2 <- reshape_item_output %>%
  mutate(Agr = b_pGram.coded + r_pGram.coded,
         Agr_0 = Agr + 0.5 * `b_pGram.coded:position.coded.1` + 0.5 * `r_pGram.coded:position.coded.1`,
         Agr_1 = Agr + 0.5 * `b_pGram.coded:position.coded.2` + 0.5 * `r_pGram.coded:position.coded.2`,
         Agr_2 = Agr - 0.5 * `b_pGram.coded:position.coded.1` - 0.5 * `b_pGram.coded:position.coded.2` - 0.5 * `r_pGram.coded:position.coded.1`- 0.5 * `r_pGram.coded:position.coded.2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, Agr_0, Agr_1, Agr_2) %>%
  gather(key = 'coef', value = 'val', Agr_0, Agr_1, Agr_2) %>%
  group_by(item, coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])%>%separate(coef,c('coef','ROI'),sep='_')
by_construction_gpt2 <- reshape_item_output[,c('.draw',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr_0 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.1`,
         Agr_1 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.2`,
         Agr_2 = b_pGram.coded - 0.5 * `b_pGram.coded:position.coded.1`- 0.5 * `b_pGram.coded:position.coded.2`) %>%
  select(Agr_0,Agr_1,Agr_2) %>% gather(key='coef',value='val')%>%group_by(coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])%>%separate(coef, c('coef', 'ROI'), sep= '_')
#saveRDS(by_item_gpt2,"Agreement/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"Agreement/by_construction_gpt2.rds")
Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=0)
Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=1)
Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=2)
PredictedRT_df <- Predicting_RT_with_spillover(rt.data,"Agreement")