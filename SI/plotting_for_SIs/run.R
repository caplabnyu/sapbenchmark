source("../../analysis/shared/util.R")
#ClassicGP
#empirical
rt.data <- load_data("ClassicGP")
fit_P0 <- readRDS("../../plots/spr/ClassicGP/prior1_fit_P0.rds")
fit_P1 <- readRDS("../../plots/spr/ClassicGP/prior1_fit_P1.rds")
fit_P2 <- readRDS("../../plots/spr/ClassicGP/prior1_fit_P2.rds")
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
#saveRDS(by_item,"../../plots/spr/ClassicGP/by_item.rds")
#saveRDS(by_construction,"../../plots/spr/ClassicGP/ClassicGP/by_construction.rds")
Plot_empirical_construction_level(by_construction,"ClassicGP")
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=0)
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=1)
Plot_itemwise_by_magnitude(by_item,"ClassicGP",ROI=2)
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"ClassicGP")
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,1)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,2)
#do the same thing for lstm+, gpt2+, nosurprisal
#lstm+
fit_P0 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_lstm_GP_P0.rds")
fit_P1 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_lstm_GP_P1.rds")
fit_P2 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_lstm_GP_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
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
#saveRDS(by_item_lstm,"../../plots/spr/ClassicGP/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"../spr/plots/spr/ClassicGP/by_construction_lstm.rds")
Plot_empirical_construction_level(by_construction_lstm,"ClassicGP")
#gpt2
fit_P0 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_gpt2_GP_P0.rds")
fit_P1 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_gpt2_GP_P1.rds")
fit_P2 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_gpt2_GP_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
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
#saveRDS(by_item_gpt2,"../../plots/spr/ClassicGP/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"../../plots/spr/ClassicGP/by_construction_gpt2.rds")
Plot_empirical_construction_level(by_construction_gpt2,"ClassicGP")
#nosurp
fit_P0 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_nosurp_GP_P0.rds")
fit_P1 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_nosurp_GP_P1.rds")
fit_P2 <- readRDS("../../plots/spr/ClassicGP/brm_predicted_nosurp_GP_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_nosurp <- reshape_item_output %>%
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
by_construction_nosurp <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_MVRR = b_AMBUAMB,
         GPE_NPS = b_AMBUAMB+`b_AMBUAMB:SZM1`,
         GPE_NPZ = b_AMBUAMB+`b_AMBUAMB:SZM2`) %>%
  select(ROI,GPE_MVRR,GPE_NPS,GPE_NPZ) %>% gather(key='coef',value='val',GPE_MVRR, GPE_NPS, GPE_NPZ)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_nosurp,"../../plots/spr/ClassicGP/by_item_nosurp.rds")
#saveRDS(by_construction_nosurp,"../../plots/spr/ClassicGP/by_construction_nosurp.rds")
Plot_empirical_construction_level(by_construction_nosurp,"ClassicGP")



#RelativeClause
#empirical
rt.data <- load_data("RelativeClause")
#get posterior estimates from the iteration (sampling)
fit_P0 <- readRDS("../../plots/spr/RelativeClause/fit_verb_bayes_prior1.rds")
fit_P1 <- readRDS("../../plots/spr/RelativeClause/fit_det_bayes_prior1.rds")
fit_P2 <- readRDS("../../plots/spr/RelativeClause/fit_noun_bayes_prior1.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
#concatenate sampling of different ROIs
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
#sum coefficients rowwise to recover the effects of interest for each iteration (sampling)
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
#saveRDS(by_item,"../../plots/spr/RelativeClause/by_item.rds")
#saveRDS(by_construction,"../../plots/spr/RelativeClause/by_construction.rds")
#by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"RelativeClause")
Plot_empirical_construction_level(by_construction,"RelativeClause")
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=0)
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=1)
Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI=2)
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
#do the same thing for lstm+, gpt2+, nosurprisal
#lstm
fit_P0 <- readRDS("../../plots/spr/RelativeClause/fit_verb_bayes_pred_lstm_prior1.rds")
fit_P1 <- readRDS("../../plots/spr/RelativeClause/fit_det_bayes_pred_lstm_prior1.rds")
fit_P2 <- readRDS("../../plots/spr/RelativeClause/fit_noun_bayes_pred_lstm_prior1.rds")
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
#saveRDS(by_item_lstm,"../../plots/spr/RelativeClause/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"../../plots/spr/RelativeClause/by_construction_lstm.rds")
Plot_empirical_construction_level(by_construction_lstm,"RelativeClause")
#gpt2
fit_P0 <- readRDS("../../plots/spr/RelativeClause/fit_verb_bayes_pred_gpt2_prior1.rds")
fit_P1 <- readRDS("../../plots/spr/RelativeClause/fit_det_bayes_pred_gpt2_prior1.rds")
fit_P2 <- readRDS("../../plots/spr/RelativeClause/fit_noun_bayes_pred_gpt2_prior1.rds")
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
#saveRDS(by_item_gpt2,"../../plots/spr/RelativeClause/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"../../plots/spr/RelativeClause/by_construction_gpt2.rds")
Plot_empirical_construction_level(by_construction_gpt2,"RelativeClause")
#nosurp
fit_P0 <- readRDS("../../plots/spr/RelativeClause/fit_verb_bayes_pred_nosurp_prior1.rds")
fit_P1 <- readRDS("../../plots/spr/RelativeClause/fit_det_bayes_pred_nosurp_prior1.rds")
fit_P2 <- readRDS("../../plots/spr/RelativeClause/fit_noun_bayes_pred_nosurp_prior1.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_nosurp <- reshape_item_output %>%
  mutate(RC = b_Type_num + r_Type_num) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, RC) %>%
  gather(key = 'coef', value = 'val', RC) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='RC')
by_construction_nosurp <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(RC = b_Type_num) %>%
  select(ROI,RC) %>% gather(key='coef',value='val',RC)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_nosurp,"../../plots/spr/RelativeClause/by_item_nosurp.rds")
#saveRDS(by_construction_nosurp,"../../plots/spr/RelativeClause/by_construction_nosurp.rds")
Plot_empirical_construction_level(by_construction_nosurp,"RelativeClause")




#AttachmentAmbiguity
rt.data <- load_data("AttachmentAmbiguity")
fit_P0 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_0_emp.rds")
fit_P1 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_0_emp.rds")
fit_P2 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_0_emp.rds")
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
#saveRDS(by_item,"../../plots/spr/AttachmentAmbiguity/by_item.rds")
#saveRDS(by_construction,"../../plots/spr/AttachmentAmbiguity/by_construction.rds")
#by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"AttachmentAmbiguity")
Plot_empirical_construction_level(by_construction,"AttachmentAmbiguity")
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=0)
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=1)
Plot_itemwise_by_magnitude(by_item,"AttachmentAmbiguity",ROI=2)
#do the same thing for lstm+, gpt2+, nosurprisal
#lstm
fit_P0 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_lstm_0.rds")
fit_P1 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_lstm_1.rds")
fit_P2 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_lstm_2.rds")
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
#saveRDS(by_item_lstm,"../../plots/spr/AttachmentAmbiguity/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"../../plots/spr/AttachmentAmbiguity/by_construction_lstm.rds")
Plot_empirical_construction_level(by_construction_lstm,"AttachmentAmbiguity")
fit_P0 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_gpt2_0.rds")
fit_P1 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_gpt2_1.rds")
fit_P2 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_gpt2_2.rds")
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
#saveRDS(by_item_gpt2,"../../plots/spr/AttachmentAmbiguity/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"../../plots/spr/AttachmentAmbiguity/by_construction_gpt2.rds")
Plot_empirical_construction_level(by_construction_gpt2,"AttachmentAmbiguity")
#nosurp
fit_P0 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_nosurp_0.rds")
fit_P1 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_nosurp_1.rds")
fit_P2 <- readRDS("../../plots/spr/AttachmentAmbiguity/brm_prior1_pred_nosurp_2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_nosurp <- reshape_item_output %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height -r_ambiguity + (1/2)*r_height,
         GPE_low = -b_ambiguity - (1/2)*b_height -r_ambiguity - (1/2)*r_height)  %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, GPE_high,GPE_low) %>%
  gather(key = 'coef', value = 'val', GPE_high,GPE_low) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])
by_construction_nosurp <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_high = -b_ambiguity + (1/2)*b_height,
         GPE_low = -b_ambiguity - (1/2)*b_height) %>%
  select(ROI,GPE_high,GPE_low) %>% gather(key='coef',value='val',GPE_high,GPE_low)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_nosurp,"../../plots/spr/AttachmentAmbiguity/by_item_nosurp.rds")
#saveRDS(by_construction_nosurp,"../../plots/spr/AttachmentAmbiguity/by_construction_nosurp.rds")
Plot_empirical_construction_level(by_construction_nosurp,"AttachmentAmbiguity")



## Agreement subset
#empirical
rt.data <- load_data("Agreement")
fit_P0 <- readRDS("../../plots/spr/Agreement/brm_RT_Agr_P0.rds")
fit_P1 <- readRDS("../../plots/spr/Agreement/brm_RT_Agr_P1.rds")
fit_P2 <- readRDS("../../plots/spr/Agreement/brm_RT_Agr_P2.rds")
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
  mutate(Agr = b_pGram.coded + r_pGram.coded
         ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = b_pGram.coded) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item,"../../plots/spr/Agreement/by_item.rds")
#saveRDS(by_construction,"../../plots/spr/Agreement/by_construction.rds")
#do the same thing for lstm+, gpt2+, nosurprisal
#lstm
fit_P0 <- readRDS("../../plots/spr/Agreement/brm_predicted_lstm_Agr_P0.rds")
fit_P1 <- readRDS("../../plots/spr/Agreement/brm_predicted_lstm_Agr_P1.rds")
fit_P2 <- readRDS("../../plots/spr/Agreement/brm_predicted_lstm_Agr_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_lstm <- reshape_item_output %>%
  mutate(Agr = b_pGram.coded + r_pGram.coded
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction_lstm <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = b_pGram.coded) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_lstm,"../../plots/spr/Agreement/by_item_lstm.rds")
#saveRDS(by_construction_lstm,"../../plots/spr/Agreement/by_construction_lstm.rds")
#gpt2
fit_P0 <- readRDS("../../plots/spr/Agreement/brm_predicted_gpt2_Agr_P0.rds")
fit_P1 <- readRDS("../../plots/spr/Agreement/brm_predicted_gpt2_Agr_P1.rds")
fit_P2 <- readRDS("../../plots/spr/Agreement/brm_predicted_gpt2_Agr_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_gpt2 <- reshape_item_output %>%
  mutate(Agr = b_pGram.coded + r_pGram.coded
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction_gpt2 <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = b_pGram.coded) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_gpt2,"../../plots/spr/Agreement/by_item_gpt2.rds")
#saveRDS(by_construction_gpt2,"../../plots/spr/Agreement/by_construction_gpt2.rds")
#nosurp
fit_P0 <- readRDS("../../plots/spr/Agreement/brm_predicted_nosurp_Agr_P0.rds")
fit_P1 <- readRDS("../../plots/spr/Agreement/brm_predicted_nosurp_Agr_P1.rds")
fit_P2 <- readRDS("../../plots/spr/Agreement/brm_predicted_nosurp_Agr_P2.rds")
reshape_item_output_P0 <- reshape_item_dat(fit_P0, 'item')
reshape_item_output_P1 <- reshape_item_dat(fit_P1, 'item')
reshape_item_output_P2 <- reshape_item_dat(fit_P2, 'item')
reshape_item_output <- rbind(reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)%>%mutate(ROI = rep(c("0","1","2"),each=nrow(reshape_item_output_P0)))
reshape_item_output$item <- as.numeric(reshape_item_output$item)
rm(fit_P0,fit_P1,fit_P2,reshape_item_output_P0,reshape_item_output_P1,reshape_item_output_P2)
by_item_nosurp <- reshape_item_output %>%
  mutate(Agr = b_pGram.coded + r_pGram.coded
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, ROI, Agr) %>%
  gather(key = 'coef', value = 'val', Agr) %>%
  group_by(item, ROI , coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]]) %>% mutate(coef='Agr')
by_construction_nosurp <- reshape_item_output[,c('.draw','ROI',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(Agr = b_pGram.coded) %>%
  select(ROI,Agr) %>% gather(key='coef',value='val',Agr)%>%group_by(ROI,coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])
#saveRDS(by_item_nosurp,"../../plots/spr/Agreement/by_item_nosurp.rds")
#saveRDS(by_construction_nosurp,"../../plots/spr/Agreement/by_construction_nosurp.rds")