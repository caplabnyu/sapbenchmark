library(brms)
library(dplyr)
library(ggplot2)

emp_AA_P0 <- readRDS("AttachmentAmbiguity/brm_prior1_0_emp.rds")
#emp_AA_P1 <- readRDS("AttachmentAmbiguity/brm_prior1_1_emp.rds")
#emp_AA_P2 <- readRDS("AttachmentAmbiguity/brm_prior1_2_emp.rds")
#lstm_AA_P0 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_lstm_0.rds")
#lstm_AA_P1 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_lstm_1.rds")
#lstm_AA_P2 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_lstm_2.rds")
#gpt2_AA_P0 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_gpt2_0.rds")
#gpt2_AA_P1 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_gpt2_1.rds")
#gpt2_AA_P2 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_gpt2_2.rds")
#lstmonly_AA_P0 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_lstmonly_0.rds")
#lstmonly_AA_P1 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_lstmonly_1.rds")
#lstmonly_AA_P2 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_lstmonly_2.rds")
#gpt2only_AA_P0 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_gpt2only_0.rds")
#gpt2only_AA_P1 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_gpt2only_1.rds")
#gpt2only_AA_P2 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_gpt2only_2.rds")
#nosurp_AA_P0 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_0_nosurp.rds")
#nosurp_AA_P1 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_1_nosurp.rds")
#nosurp_AA_P2 <- readRDS("AttachmentAmbiguity/brm_prior1_pred_2_nosurp.rds")





#posterior_samp <- posterior_samples(emp_AA_P0)
#randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(ambiguity|height|Intercept)',colnames(posterior_samp))]
#saveRDS(randomslope_names,"AttachmentAmbiguity/AA_randomslopesnames.rds")
randomslope_names <- readRDS("AttachmentAmbiguity/AA_randomslopesnames.rds")

emp_AA_P0_posterior_samp <- posterior_samples(emp_AA_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstm_AA_P0_posterior_samp <- posterior_samples(lstm_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2_AA_P0_posterior_samp <- posterior_samples(gpt2_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstmonly_AA_P0_posterior_samp <- posterior_samples(lstmonly_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2only_AA_P0_posterior_samp <- posterior_samples(gpt2only_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
nosurp_AA_P0_posterior_samp <- posterior_samples(nosurp_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
emp_AA_P1_posterior_samp <- posterior_samples(emp_AA_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstm_AA_P1_posterior_samp <- posterior_samples(lstm_AA_P1, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2_AA_P1_posterior_samp <- posterior_samples(gpt2_AA_P1, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstmonly_AA_P1_posterior_samp <- posterior_samples(lstmonly_AA_P1, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2only_AA_P1_posterior_samp <- posterior_samples(gpt2only_AA_P1, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
nosurp_AA_P1_posterior_samp <- posterior_samples(nosurp_AA_P1, fixed=TRUE, pars=
                                                   c("b_Intercept","b_ambiguity","b_height",randomslope_names))
emp_AA_P2_posterior_samp <- posterior_samples(emp_AA_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstm_AA_P2_posterior_samp <- posterior_samples(lstm_AA_P2, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2_AA_P2_posterior_samp <- posterior_samples(gpt2_AA_P2, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstmonly_AA_P2_posterior_samp <- posterior_samples(lstmonly_AA_P2, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2only_AA_P2_posterior_samp <- posterior_samples(gpt2only_AA_P2, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
nosurp_AA_P2_posterior_samp <- posterior_samples(nosurp_AA_P2, fixed=TRUE, pars=
                                                   c("b_Intercept","b_ambiguity","b_height",randomslope_names))

rm(emp_AA_P0, lstm_AA_P0, gpt2_AA_P0, lstmonly_AA_P0, gpt2only_AA_P0, nosurp_AA_P0, emp_AA_P1, lstm_AA_P1,
gpt2_AA_P1, lstmonly_AA_P1,gpt2only_AA_P1, nosurp_AA_P1, emp_AA_P2, lstm_AA_P2, gpt2_AA_P2, lstmonly_AA_P2, gpt2only_AA_P2,nosurp_AA_P2,posterior_samp)
#saveRDS(emp_AA_P0_posterior_samp,"AttachmentAmbiguity/emp_AA_P0_posterior_samp.rds")
#saveRDS(lstm_AA_P0_posterior_samp,"AttachmentAmbiguity/lstm_AA_P0_posterior_samp.rds")
#saveRDS(gpt2_AA_P0_posterior_samp,"AttachmentAmbiguity/gpt2_AA_P0_posterior_samp.rds")
#saveRDS(lstmonly_AA_P0_posterior_samp,"AttachmentAmbiguity/lstmonly_AA_P0_posterior_samp.rds")
#saveRDS(gpt2only_AA_P0_posterior_samp,"AttachmentAmbiguity/gpt2only_AA_P0_posterior_samp.rds")
#saveRDS(nosurp_AA_P0_posterior_samp,"AttachmentAmbiguity/nosurp_AA_P0_posterior_samp.rds")
#saveRDS(emp_AA_P1_posterior_samp,"AttachmentAmbiguity/emp_AA_P1_posterior_samp.rds")
#saveRDS(lstm_AA_P1_posterior_samp,"AttachmentAmbiguity/lstm_AA_P1_posterior_samp.rds")
#saveRDS(gpt2_AA_P1_posterior_samp,"AttachmentAmbiguity/gpt2_AA_P1_posterior_samp.rds")
#saveRDS(lstmonly_AA_P1_posterior_samp,"AttachmentAmbiguity/lstmonly_AA_P1_posterior_samp.rds")
#saveRDS(gpt2only_AA_P1_posterior_samp,"AttachmentAmbiguity/gpt2only_AA_P1_posterior_samp.rds")
#saveRDS(nosurp_AA_P1_posterior_samp,"AttachmentAmbiguity/nosurp_AA_P1_posterior_samp.rds")
#saveRDS(emp_AA_P2_posterior_samp,"AttachmentAmbiguity/emp_AA_P2_posterior_samp.rds")
#saveRDS(lstm_AA_P2_posterior_samp,"AttachmentAmbiguity/lstm_AA_P2_posterior_samp.rds")
#saveRDS(gpt2_AA_P2_posterior_samp,"AttachmentAmbiguity/gpt2_AA_P2_posterior_samp.rds")
#saveRDS(lstmonly_AA_P2_posterior_samp,"AttachmentAmbiguity/lstmonly_AA_P2_posterior_samp.rds")
#saveRDS(gpt2only_AA_P2_posterior_samp,"AttachmentAmbiguity/gpt2only_AA_P2_posterior_samp.rds")
#saveRDS(nosurp_AA_P2_posterior_samp,"AttachmentAmbiguity/nosurp_AA_P2_posterior_samp.rds")

emp_AA_P0_posterior_samp <- readRDS("AttachmentAmbiguity/emp_AA_P0_posterior_samp.rds")
lstm_AA_P0_posterior_samp <- readRDS("AttachmentAmbiguity/lstm_AA_P0_posterior_samp.rds")
gpt2_AA_P0_posterior_samp <- readRDS("AttachmentAmbiguity/gpt2_AA_P0_posterior_samp.rds")
nosurp_AA_P0_posterior_samp <- readRDS("AttachmentAmbiguity/nosurp_AA_P0_posterior_samp.rds")
emp_AA_P1_posterior_samp <- readRDS("AttachmentAmbiguity/emp_AA_P1_posterior_samp.rds")
lstm_AA_P1_posterior_samp <- readRDS("AttachmentAmbiguity/lstm_AA_P1_posterior_samp.rds")
gpt2_AA_P1_posterior_samp <- readRDS("AttachmentAmbiguity/gpt2_AA_P1_posterior_samp.rds")
nosurp_AA_P1_posterior_samp <- readRDS("AttachmentAmbiguity/nosurp_AA_P1_posterior_samp.rds")
emp_AA_P2_posterior_samp <- readRDS("AttachmentAmbiguity/emp_AA_P2_posterior_samp.rds")
lstm_AA_P2_posterior_samp <- readRDS("AttachmentAmbiguity/lstm_AA_P2_posterior_samp.rds")
gpt2_AA_P2_posterior_samp <- readRDS("AttachmentAmbiguity/gpt2_AA_P2_posterior_samp.rds")
nosurp_AA_P2_posterior_samp <- readRDS("AttachmentAmbiguity/nosurp_AA_P2_posterior_samp.rds")




posterior_emp_high_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=0)
posterior_emp_low_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=0)
posterior_emp_high_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=1)
posterior_emp_low_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=1)
posterior_emp_high_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=2)
posterior_emp_low_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=2)
posterior_lstm_high_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=0)
posterior_lstm_low_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=0)
posterior_lstm_high_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=1)
posterior_lstm_low_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=1)
posterior_lstm_high_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=2)
posterior_lstm_low_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=2)
posterior_gpt2_high_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=0)
posterior_gpt2_low_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=0)
posterior_gpt2_high_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=1)
posterior_gpt2_low_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=1)
posterior_gpt2_high_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=2)
posterior_gpt2_low_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=2)
posterior_lstmonly_high_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=0)
posterior_lstmonly_low_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=0)
posterior_lstmonly_high_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=1)
posterior_lstmonly_low_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=1)
posterior_lstmonly_high_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=2)
posterior_lstmonly_low_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=2)
posterior_gpt2only_high_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=0)
posterior_gpt2only_low_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=0)
posterior_gpt2only_high_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=1)
posterior_gpt2only_low_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=1)
posterior_gpt2only_high_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=2)
posterior_gpt2only_low_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=2)
posterior_nosurp_high_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=0)
posterior_nosurp_low_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=0)
posterior_nosurp_high_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=1)
posterior_nosurp_low_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=1)
posterior_nosurp_high_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_high",ROI=2)
posterior_nosurp_low_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="GPE_low",ROI=2)


posterior_emp_multi_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="multi",ROI=0)
posterior_emp_multi_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="multi",ROI=1)
posterior_emp_multi_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=49:72,EOI="multi",ROI=2)




ncols <- ncol(emp_AA_P0_posterior_samp)
emp_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(emp_AA_P0_posterior_samp[,2])+(1/2)*emp_AA_P0_posterior_samp[,3]+(-1)*emp_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*emp_AA_P0_posterior_samp[,(1+51):(24+51)]
emp_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(emp_AA_P0_posterior_samp[,2])-(1/2)*emp_AA_P0_posterior_samp[,3]+(-1)*emp_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*emp_AA_P0_posterior_samp[,(1+51):(24+51)]
emp_AA_P0_posterior_samp[,(49+ncols):(72+ncols)] <- emp_AA_P0_posterior_samp[,1]+emp_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P0_posterior_samp[,2])+(2/3)*emp_AA_P0_posterior_samp[,(1+27):(24+27)]
emp_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(emp_AA_P1_posterior_samp[,2])+(1/2)*emp_AA_P1_posterior_samp[,3]+(-1)*emp_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*emp_AA_P1_posterior_samp[,(1+51):(24+51)]
emp_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(emp_AA_P1_posterior_samp[,2])-(1/2)*emp_AA_P1_posterior_samp[,3]+(-1)*emp_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*emp_AA_P1_posterior_samp[,(1+51):(24+51)]
emp_AA_P1_posterior_samp[,(49+ncols):(72+ncols)] <- emp_AA_P1_posterior_samp[,1]+emp_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P1_posterior_samp[,2])+(2/3)*emp_AA_P1_posterior_samp[,(1+27):(24+27)]
emp_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(emp_AA_P2_posterior_samp[,2])+(1/2)*emp_AA_P2_posterior_samp[,3]+(-1)*emp_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*emp_AA_P2_posterior_samp[,(1+51):(24+51)]
emp_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(emp_AA_P2_posterior_samp[,2])-(1/2)*emp_AA_P2_posterior_samp[,3]+(-1)*emp_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*emp_AA_P2_posterior_samp[,(1+51):(24+51)]
emp_AA_P2_posterior_samp[,(49+ncols):(72+ncols)] <- emp_AA_P2_posterior_samp[,1]+emp_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P2_posterior_samp[,2])+(2/3)*emp_AA_P2_posterior_samp[,(1+27):(24+27)]
lstm_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(lstm_AA_P0_posterior_samp[,2])+(1/2)*lstm_AA_P0_posterior_samp[,3]+(-1)*lstm_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*lstm_AA_P0_posterior_samp[,(1+51):(24+51)]
lstm_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(lstm_AA_P0_posterior_samp[,2])-(1/2)*lstm_AA_P0_posterior_samp[,3]+(-1)*lstm_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*lstm_AA_P0_posterior_samp[,(1+51):(24+51)]
lstm_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(lstm_AA_P1_posterior_samp[,2])+(1/2)*lstm_AA_P1_posterior_samp[,3]+(-1)*lstm_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*lstm_AA_P1_posterior_samp[,(1+51):(24+51)]
lstm_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(lstm_AA_P1_posterior_samp[,2])-(1/2)*lstm_AA_P1_posterior_samp[,3]+(-1)*lstm_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*lstm_AA_P1_posterior_samp[,(1+51):(24+51)]
lstm_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(lstm_AA_P2_posterior_samp[,2])+(1/2)*lstm_AA_P2_posterior_samp[,3]+(-1)*lstm_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*lstm_AA_P2_posterior_samp[,(1+51):(24+51)]
lstm_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(lstm_AA_P2_posterior_samp[,2])-(1/2)*lstm_AA_P2_posterior_samp[,3]+(-1)*lstm_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*lstm_AA_P2_posterior_samp[,(1+51):(24+51)]
gpt2_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(gpt2_AA_P0_posterior_samp[,2])+(1/2)*gpt2_AA_P0_posterior_samp[,3]+(-1)*gpt2_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2_AA_P0_posterior_samp[,(1+51):(24+51)]
gpt2_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(gpt2_AA_P0_posterior_samp[,2])-(1/2)*gpt2_AA_P0_posterior_samp[,3]+(-1)*gpt2_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2_AA_P0_posterior_samp[,(1+51):(24+51)]
gpt2_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(gpt2_AA_P1_posterior_samp[,2])+(1/2)*gpt2_AA_P1_posterior_samp[,3]+(-1)*gpt2_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2_AA_P1_posterior_samp[,(1+51):(24+51)]
gpt2_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(gpt2_AA_P1_posterior_samp[,2])-(1/2)*gpt2_AA_P1_posterior_samp[,3]+(-1)*gpt2_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2_AA_P1_posterior_samp[,(1+51):(24+51)]
gpt2_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(gpt2_AA_P2_posterior_samp[,2])+(1/2)*gpt2_AA_P2_posterior_samp[,3]+(-1)*gpt2_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2_AA_P2_posterior_samp[,(1+51):(24+51)]
gpt2_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(gpt2_AA_P2_posterior_samp[,2])-(1/2)*gpt2_AA_P2_posterior_samp[,3]+(-1)*gpt2_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2_AA_P2_posterior_samp[,(1+51):(24+51)]
lstmonly_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(lstmonly_AA_P0_posterior_samp[,2])+(1/2)*lstmonly_AA_P0_posterior_samp[,3]+(-1)*lstmonly_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*lstmonly_AA_P0_posterior_samp[,(1+51):(24+51)]
lstmonly_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(lstmonly_AA_P0_posterior_samp[,2])-(1/2)*lstmonly_AA_P0_posterior_samp[,3]+(-1)*lstmonly_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*lstmonly_AA_P0_posterior_samp[,(1+51):(24+51)]
lstmonly_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(lstmonly_AA_P1_posterior_samp[,2])+(1/2)*lstmonly_AA_P1_posterior_samp[,3]+(-1)*lstmonly_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*lstmonly_AA_P1_posterior_samp[,(1+51):(24+51)]
lstmonly_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(lstmonly_AA_P1_posterior_samp[,2])-(1/2)*lstmonly_AA_P1_posterior_samp[,3]+(-1)*lstmonly_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*lstmonly_AA_P1_posterior_samp[,(1+51):(24+51)]
lstmonly_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(lstmonly_AA_P2_posterior_samp[,2])+(1/2)*lstmonly_AA_P2_posterior_samp[,3]+(-1)*lstmonly_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*lstmonly_AA_P2_posterior_samp[,(1+51):(24+51)]
lstmonly_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(lstmonly_AA_P2_posterior_samp[,2])-(1/2)*lstmonly_AA_P2_posterior_samp[,3]+(-1)*lstmonly_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*lstmonly_AA_P2_posterior_samp[,(1+51):(24+51)]
gpt2only_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(gpt2only_AA_P0_posterior_samp[,2])+(1/2)*gpt2only_AA_P0_posterior_samp[,3]+(-1)*gpt2only_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2only_AA_P0_posterior_samp[,(1+51):(24+51)]
gpt2only_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(gpt2only_AA_P0_posterior_samp[,2])-(1/2)*gpt2only_AA_P0_posterior_samp[,3]+(-1)*gpt2only_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2only_AA_P0_posterior_samp[,(1+51):(24+51)]
gpt2only_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(gpt2only_AA_P1_posterior_samp[,2])+(1/2)*gpt2only_AA_P1_posterior_samp[,3]+(-1)*gpt2only_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2only_AA_P1_posterior_samp[,(1+51):(24+51)]
gpt2only_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(gpt2only_AA_P1_posterior_samp[,2])-(1/2)*gpt2only_AA_P1_posterior_samp[,3]+(-1)*gpt2only_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2only_AA_P1_posterior_samp[,(1+51):(24+51)]
gpt2only_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(gpt2only_AA_P2_posterior_samp[,2])+(1/2)*gpt2only_AA_P2_posterior_samp[,3]+(-1)*gpt2only_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2only_AA_P2_posterior_samp[,(1+51):(24+51)]
gpt2only_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(gpt2only_AA_P2_posterior_samp[,2])-(1/2)*gpt2only_AA_P2_posterior_samp[,3]+(-1)*gpt2only_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2only_AA_P2_posterior_samp[,(1+51):(24+51)]
nosurp_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(nosurp_AA_P0_posterior_samp[,2])+(1/2)*nosurp_AA_P0_posterior_samp[,3]+(-1)*nosurp_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*nosurp_AA_P0_posterior_samp[,(1+51):(24+51)]
nosurp_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(nosurp_AA_P0_posterior_samp[,2])-(1/2)*nosurp_AA_P0_posterior_samp[,3]+(-1)*nosurp_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*nosurp_AA_P0_posterior_samp[,(1+51):(24+51)]
nosurp_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(nosurp_AA_P1_posterior_samp[,2])+(1/2)*nosurp_AA_P1_posterior_samp[,3]+(-1)*nosurp_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*nosurp_AA_P1_posterior_samp[,(1+51):(24+51)]
nosurp_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(nosurp_AA_P1_posterior_samp[,2])-(1/2)*nosurp_AA_P1_posterior_samp[,3]+(-1)*nosurp_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*nosurp_AA_P1_posterior_samp[,(1+51):(24+51)]
nosurp_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- (-1)*(nosurp_AA_P2_posterior_samp[,2])+(1/2)*nosurp_AA_P2_posterior_samp[,3]+(-1)*nosurp_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*nosurp_AA_P2_posterior_samp[,(1+51):(24+51)]
nosurp_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- (-1)*(nosurp_AA_P2_posterior_samp[,2])-(1/2)*nosurp_AA_P2_posterior_samp[,3]+(-1)*nosurp_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*nosurp_AA_P2_posterior_samp[,(1+51):(24+51)]


for(i in 1:24){
  posterior_emp_high_P0[i,]$mean <- mean(emp_AA_P0_posterior_samp[,i+ncols])
  posterior_emp_high_P0[i,]$SE <- sd(emp_AA_P0_posterior_samp[,i+ncols])
  posterior_emp_high_P0[i,]$upper <- quantile(emp_AA_P0_posterior_samp[,i+ncols],0.975)
  posterior_emp_high_P0[i,]$lower <- quantile(emp_AA_P0_posterior_samp[,i+ncols],0.025)
  posterior_emp_high_P1[i,]$mean <- mean(emp_AA_P1_posterior_samp[,i+ncols])
  posterior_emp_high_P1[i,]$SE <- sd(emp_AA_P1_posterior_samp[,i+ncols])
  posterior_emp_high_P1[i,]$upper <- quantile(emp_AA_P1_posterior_samp[,i+ncols],0.975)
  posterior_emp_high_P1[i,]$lower <- quantile(emp_AA_P1_posterior_samp[,i+ncols],0.025)
  posterior_emp_high_P2[i,]$mean <- mean(emp_AA_P2_posterior_samp[,i+ncols])
  posterior_emp_high_P2[i,]$SE <- sd(emp_AA_P2_posterior_samp[,i+ncols])
  posterior_emp_high_P2[i,]$upper <- quantile(emp_AA_P2_posterior_samp[,i+ncols],0.975)
  posterior_emp_high_P2[i,]$lower <- quantile(emp_AA_P2_posterior_samp[,i+ncols],0.025)
  posterior_emp_low_P0[i,]$mean <- mean(emp_AA_P0_posterior_samp[,i+ncols+24])
  posterior_emp_low_P0[i,]$SE <- sd(emp_AA_P0_posterior_samp[,i+ncols+24])
  posterior_emp_low_P0[i,]$upper <- quantile(emp_AA_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_low_P0[i,]$lower <- quantile(emp_AA_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_low_P1[i,]$mean <- mean(emp_AA_P1_posterior_samp[,i+ncols+24])
  posterior_emp_low_P1[i,]$SE <- sd(emp_AA_P1_posterior_samp[,i+ncols+24])
  posterior_emp_low_P1[i,]$upper <- quantile(emp_AA_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_low_P1[i,]$lower <- quantile(emp_AA_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_low_P2[i,]$mean <- mean(emp_AA_P2_posterior_samp[,i+ncols+24])
  posterior_emp_low_P2[i,]$SE <- sd(emp_AA_P2_posterior_samp[,i+ncols+24])
  posterior_emp_low_P2[i,]$upper <- quantile(emp_AA_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_low_P2[i,]$lower <- quantile(emp_AA_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_multi_P0[i,]$mean <- mean(emp_AA_P0_posterior_samp[,i+ncols+48])
  posterior_emp_multi_P0[i,]$SE <- sd(emp_AA_P0_posterior_samp[,i+ncols+48])
  posterior_emp_multi_P0[i,]$upper <- quantile(emp_AA_P0_posterior_samp[,i+ncols+48],0.975)
  posterior_emp_multi_P0[i,]$lower <- quantile(emp_AA_P0_posterior_samp[,i+ncols+48],0.025)
  posterior_emp_multi_P1[i,]$mean <- mean(emp_AA_P1_posterior_samp[,i+ncols+48])
  posterior_emp_multi_P1[i,]$SE <- sd(emp_AA_P1_posterior_samp[,i+ncols+48])
  posterior_emp_multi_P1[i,]$upper <- quantile(emp_AA_P1_posterior_samp[,i+ncols+48],0.975)
  posterior_emp_multi_P1[i,]$lower <- quantile(emp_AA_P1_posterior_samp[,i+ncols+48],0.025)
  posterior_emp_multi_P2[i,]$mean <- mean(emp_AA_P2_posterior_samp[,i+ncols+48])
  posterior_emp_multi_P2[i,]$SE <- sd(emp_AA_P2_posterior_samp[,i+ncols+48])
  posterior_emp_multi_P2[i,]$upper <- quantile(emp_AA_P2_posterior_samp[,i+ncols+48],0.975)
  posterior_emp_multi_P2[i,]$lower <- quantile(emp_AA_P2_posterior_samp[,i+ncols+48],0.025)
  posterior_lstm_high_P0[i,]$mean <- mean(lstm_AA_P0_posterior_samp[,i+ncols])
  posterior_lstm_high_P0[i,]$SE <- sd(lstm_AA_P0_posterior_samp[,i+ncols])
  posterior_lstm_high_P0[i,]$upper <- quantile(lstm_AA_P0_posterior_samp[,i+ncols],0.975)
  posterior_lstm_high_P0[i,]$lower <- quantile(lstm_AA_P0_posterior_samp[,i+ncols],0.025)
  posterior_lstm_high_P1[i,]$mean <- mean(lstm_AA_P1_posterior_samp[,i+ncols])
  posterior_lstm_high_P1[i,]$SE <- sd(lstm_AA_P1_posterior_samp[,i+ncols])
  posterior_lstm_high_P1[i,]$upper <- quantile(lstm_AA_P1_posterior_samp[,i+ncols],0.975)
  posterior_lstm_high_P1[i,]$lower <- quantile(lstm_AA_P1_posterior_samp[,i+ncols],0.025)
  posterior_lstm_high_P2[i,]$mean <- mean(lstm_AA_P2_posterior_samp[,i+ncols])
  posterior_lstm_high_P2[i,]$SE <- sd(lstm_AA_P2_posterior_samp[,i+ncols])
  posterior_lstm_high_P2[i,]$upper <- quantile(lstm_AA_P2_posterior_samp[,i+ncols],0.975)
  posterior_lstm_high_P2[i,]$lower <- quantile(lstm_AA_P2_posterior_samp[,i+ncols],0.025)
  posterior_lstm_low_P0[i,]$mean <- mean(lstm_AA_P0_posterior_samp[,i+ncols+24])
  posterior_lstm_low_P0[i,]$SE <- sd(lstm_AA_P0_posterior_samp[,i+ncols+24])
  posterior_lstm_low_P0[i,]$upper <- quantile(lstm_AA_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_lstm_low_P0[i,]$lower <- quantile(lstm_AA_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_lstm_low_P1[i,]$mean <- mean(lstm_AA_P1_posterior_samp[,i+ncols+24])
  posterior_lstm_low_P1[i,]$SE <- sd(lstm_AA_P1_posterior_samp[,i+ncols+24])
  posterior_lstm_low_P1[i,]$upper <- quantile(lstm_AA_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_lstm_low_P1[i,]$lower <- quantile(lstm_AA_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_lstm_low_P2[i,]$mean <- mean(lstm_AA_P2_posterior_samp[,i+ncols+24])
  posterior_lstm_low_P2[i,]$SE <- sd(lstm_AA_P2_posterior_samp[,i+ncols+24])
  posterior_lstm_low_P2[i,]$upper <- quantile(lstm_AA_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_lstm_low_P2[i,]$lower <- quantile(lstm_AA_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2_high_P0[i,]$mean <- mean(gpt2_AA_P0_posterior_samp[,i+ncols])
  posterior_gpt2_high_P0[i,]$SE <- sd(gpt2_AA_P0_posterior_samp[,i+ncols])
  posterior_gpt2_high_P0[i,]$upper <- quantile(gpt2_AA_P0_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_high_P0[i,]$lower <- quantile(gpt2_AA_P0_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_high_P1[i,]$mean <- mean(gpt2_AA_P1_posterior_samp[,i+ncols])
  posterior_gpt2_high_P1[i,]$SE <- sd(gpt2_AA_P1_posterior_samp[,i+ncols])
  posterior_gpt2_high_P1[i,]$upper <- quantile(gpt2_AA_P1_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_high_P1[i,]$lower <- quantile(gpt2_AA_P1_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_high_P2[i,]$mean <- mean(gpt2_AA_P2_posterior_samp[,i+ncols])
  posterior_gpt2_high_P2[i,]$SE <- sd(gpt2_AA_P2_posterior_samp[,i+ncols])
  posterior_gpt2_high_P2[i,]$upper <- quantile(gpt2_AA_P2_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_high_P2[i,]$lower <- quantile(gpt2_AA_P2_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_low_P0[i,]$mean <- mean(gpt2_AA_P0_posterior_samp[,i+ncols+24])
  posterior_gpt2_low_P0[i,]$SE <- sd(gpt2_AA_P0_posterior_samp[,i+ncols+24])
  posterior_gpt2_low_P0[i,]$upper <- quantile(gpt2_AA_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2_low_P0[i,]$lower <- quantile(gpt2_AA_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2_low_P1[i,]$mean <- mean(gpt2_AA_P1_posterior_samp[,i+ncols+24])
  posterior_gpt2_low_P1[i,]$SE <- sd(gpt2_AA_P1_posterior_samp[,i+ncols+24])
  posterior_gpt2_low_P1[i,]$upper <- quantile(gpt2_AA_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2_low_P1[i,]$lower <- quantile(gpt2_AA_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2_low_P2[i,]$mean <- mean(gpt2_AA_P2_posterior_samp[,i+ncols+24])
  posterior_gpt2_low_P2[i,]$SE <- sd(gpt2_AA_P2_posterior_samp[,i+ncols+24])
  posterior_gpt2_low_P2[i,]$upper <- quantile(gpt2_AA_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2_low_P2[i,]$lower <- quantile(gpt2_AA_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_lstmonly_high_P0[i,]$mean <- mean(lstmonly_AA_P0_posterior_samp[,i+ncols])
  posterior_lstmonly_high_P0[i,]$SE <- sd(lstmonly_AA_P0_posterior_samp[,i+ncols])
  posterior_lstmonly_high_P0[i,]$upper <- quantile(lstmonly_AA_P0_posterior_samp[,i+ncols],0.975)
  posterior_lstmonly_high_P0[i,]$lower <- quantile(lstmonly_AA_P0_posterior_samp[,i+ncols],0.025)
  posterior_lstmonly_high_P1[i,]$mean <- mean(lstmonly_AA_P1_posterior_samp[,i+ncols])
  posterior_lstmonly_high_P1[i,]$SE <- sd(lstmonly_AA_P1_posterior_samp[,i+ncols])
  posterior_lstmonly_high_P1[i,]$upper <- quantile(lstmonly_AA_P1_posterior_samp[,i+ncols],0.975)
  posterior_lstmonly_high_P1[i,]$lower <- quantile(lstmonly_AA_P1_posterior_samp[,i+ncols],0.025)
  posterior_lstmonly_high_P2[i,]$mean <- mean(lstmonly_AA_P2_posterior_samp[,i+ncols])
  posterior_lstmonly_high_P2[i,]$SE <- sd(lstmonly_AA_P2_posterior_samp[,i+ncols])
  posterior_lstmonly_high_P2[i,]$upper <- quantile(lstmonly_AA_P2_posterior_samp[,i+ncols],0.975)
  posterior_lstmonly_high_P2[i,]$lower <- quantile(lstmonly_AA_P2_posterior_samp[,i+ncols],0.025)
  posterior_lstmonly_low_P0[i,]$mean <- mean(lstmonly_AA_P0_posterior_samp[,i+ncols+24])
  posterior_lstmonly_low_P0[i,]$SE <- sd(lstmonly_AA_P0_posterior_samp[,i+ncols+24])
  posterior_lstmonly_low_P0[i,]$upper <- quantile(lstmonly_AA_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_lstmonly_low_P0[i,]$lower <- quantile(lstmonly_AA_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_lstmonly_low_P1[i,]$mean <- mean(lstmonly_AA_P1_posterior_samp[,i+ncols+24])
  posterior_lstmonly_low_P1[i,]$SE <- sd(lstmonly_AA_P1_posterior_samp[,i+ncols+24])
  posterior_lstmonly_low_P1[i,]$upper <- quantile(lstmonly_AA_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_lstmonly_low_P1[i,]$lower <- quantile(lstmonly_AA_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_lstmonly_low_P2[i,]$mean <- mean(lstmonly_AA_P2_posterior_samp[,i+ncols+24])
  posterior_lstmonly_low_P2[i,]$SE <- sd(lstmonly_AA_P2_posterior_samp[,i+ncols+24])
  posterior_lstmonly_low_P2[i,]$upper <- quantile(lstmonly_AA_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_lstmonly_low_P2[i,]$lower <- quantile(lstmonly_AA_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2only_high_P0[i,]$mean <- mean(gpt2only_AA_P0_posterior_samp[,i+ncols])
  posterior_gpt2only_high_P0[i,]$SE <- sd(gpt2only_AA_P0_posterior_samp[,i+ncols])
  posterior_gpt2only_high_P0[i,]$upper <- quantile(gpt2only_AA_P0_posterior_samp[,i+ncols],0.975)
  posterior_gpt2only_high_P0[i,]$lower <- quantile(gpt2only_AA_P0_posterior_samp[,i+ncols],0.025)
  posterior_gpt2only_high_P1[i,]$mean <- mean(gpt2only_AA_P1_posterior_samp[,i+ncols])
  posterior_gpt2only_high_P1[i,]$SE <- sd(gpt2only_AA_P1_posterior_samp[,i+ncols])
  posterior_gpt2only_high_P1[i,]$upper <- quantile(gpt2only_AA_P1_posterior_samp[,i+ncols],0.975)
  posterior_gpt2only_high_P1[i,]$lower <- quantile(gpt2only_AA_P1_posterior_samp[,i+ncols],0.025)
  posterior_gpt2only_high_P2[i,]$mean <- mean(gpt2only_AA_P2_posterior_samp[,i+ncols])
  posterior_gpt2only_high_P2[i,]$SE <- sd(gpt2only_AA_P2_posterior_samp[,i+ncols])
  posterior_gpt2only_high_P2[i,]$upper <- quantile(gpt2only_AA_P2_posterior_samp[,i+ncols],0.975)
  posterior_gpt2only_high_P2[i,]$lower <- quantile(gpt2only_AA_P2_posterior_samp[,i+ncols],0.025)
  posterior_gpt2only_low_P0[i,]$mean <- mean(gpt2only_AA_P0_posterior_samp[,i+ncols+24])
  posterior_gpt2only_low_P0[i,]$SE <- sd(gpt2only_AA_P0_posterior_samp[,i+ncols+24])
  posterior_gpt2only_low_P0[i,]$upper <- quantile(gpt2only_AA_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2only_low_P0[i,]$lower <- quantile(gpt2only_AA_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2only_low_P1[i,]$mean <- mean(gpt2only_AA_P1_posterior_samp[,i+ncols+24])
  posterior_gpt2only_low_P1[i,]$SE <- sd(gpt2only_AA_P1_posterior_samp[,i+ncols+24])
  posterior_gpt2only_low_P1[i,]$upper <- quantile(gpt2only_AA_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2only_low_P1[i,]$lower <- quantile(gpt2only_AA_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2only_low_P2[i,]$mean <- mean(gpt2only_AA_P2_posterior_samp[,i+ncols+24])
  posterior_gpt2only_low_P2[i,]$SE <- sd(gpt2only_AA_P2_posterior_samp[,i+ncols+24])
  posterior_gpt2only_low_P2[i,]$upper <- quantile(gpt2only_AA_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2only_low_P2[i,]$lower <- quantile(gpt2only_AA_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_nosurp_high_P0[i,]$mean <- mean(nosurp_AA_P0_posterior_samp[,i+ncols])
  posterior_nosurp_high_P0[i,]$SE <- sd(nosurp_AA_P0_posterior_samp[,i+ncols])
  posterior_nosurp_high_P0[i,]$upper <- quantile(nosurp_AA_P0_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_high_P0[i,]$lower <- quantile(nosurp_AA_P0_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_high_P1[i,]$mean <- mean(nosurp_AA_P1_posterior_samp[,i+ncols])
  posterior_nosurp_high_P1[i,]$SE <- sd(nosurp_AA_P1_posterior_samp[,i+ncols])
  posterior_nosurp_high_P1[i,]$upper <- quantile(nosurp_AA_P1_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_high_P1[i,]$lower <- quantile(nosurp_AA_P1_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_high_P2[i,]$mean <- mean(nosurp_AA_P2_posterior_samp[,i+ncols])
  posterior_nosurp_high_P2[i,]$SE <- sd(nosurp_AA_P2_posterior_samp[,i+ncols])
  posterior_nosurp_high_P2[i,]$upper <- quantile(nosurp_AA_P2_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_high_P2[i,]$lower <- quantile(nosurp_AA_P2_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_low_P0[i,]$mean <- mean(nosurp_AA_P0_posterior_samp[,i+ncols+24])
  posterior_nosurp_low_P0[i,]$SE <- sd(nosurp_AA_P0_posterior_samp[,i+ncols+24])
  posterior_nosurp_low_P0[i,]$upper <- quantile(nosurp_AA_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_nosurp_low_P0[i,]$lower <- quantile(nosurp_AA_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_nosurp_low_P1[i,]$mean <- mean(nosurp_AA_P1_posterior_samp[,i+ncols+24])
  posterior_nosurp_low_P1[i,]$SE <- sd(nosurp_AA_P1_posterior_samp[,i+ncols+24])
  posterior_nosurp_low_P1[i,]$upper <- quantile(nosurp_AA_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_nosurp_low_P1[i,]$lower <- quantile(nosurp_AA_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_nosurp_low_P2[i,]$mean <- mean(nosurp_AA_P2_posterior_samp[,i+ncols+24])
  posterior_nosurp_low_P2[i,]$SE <- sd(nosurp_AA_P2_posterior_samp[,i+ncols+24])
  posterior_nosurp_low_P2[i,]$upper <- quantile(nosurp_AA_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_nosurp_low_P2[i,]$lower <- quantile(nosurp_AA_P2_posterior_samp[,i+ncols+24],0.025)
}


by_item_multi <- data.frame(item=c(posterior_emp_multi_P0$item,posterior_emp_multi_P1$item,posterior_emp_multi_P2$item,posterior_emp_multi_P0$item,posterior_emp_multi_P1$item,posterior_emp_multi_P2$item),
                      ROI=c(posterior_emp_multi_P0$ROI,posterior_emp_multi_P1$ROI,posterior_emp_multi_P2$ROI,posterior_emp_multi_P0$ROI,posterior_emp_multi_P1$ROI,posterior_emp_multi_P2$ROI),
                      coef=rep(c("multi","multi"),each=72),
                      mean=c(posterior_emp_multi_P0$mean,posterior_emp_multi_P1$mean,posterior_emp_multi_P2$mean,posterior_emp_multi_P0$mean,posterior_emp_multi_P1$mean,posterior_emp_multi_P2$mean),
                      lower=c(posterior_emp_multi_P0$lower,posterior_emp_multi_P1$lower,posterior_emp_multi_P2$lower,posterior_emp_multi_P0$lower,posterior_emp_multi_P1$lower,posterior_emp_multi_P2$lower),
                      upper=c(posterior_emp_multi_P0$upper,posterior_emp_multi_P1$upper,posterior_emp_multi_P2$upper,posterior_emp_multi_P0$upper,posterior_emp_multi_P1$upper,posterior_emp_multi_P2$upper))
by_item <- data.frame(item=c(posterior_emp_high_P0$item,posterior_emp_high_P1$item,posterior_emp_high_P2$item,posterior_emp_low_P0$item,posterior_emp_low_P1$item,posterior_emp_low_P2$item),
                      ROI=c(posterior_emp_high_P0$ROI,posterior_emp_high_P1$ROI,posterior_emp_high_P2$ROI,posterior_emp_low_P0$ROI,posterior_emp_low_P1$ROI,posterior_emp_low_P2$ROI),
                      coef=rep(c("GPE_high","GPE_low"),each=72),
                      mean=c(posterior_emp_high_P0$mean,posterior_emp_high_P1$mean,posterior_emp_high_P2$mean,posterior_emp_low_P0$mean,posterior_emp_low_P1$mean,posterior_emp_low_P2$mean),
                      lower=c(posterior_emp_high_P0$lower,posterior_emp_high_P1$lower,posterior_emp_high_P2$lower,posterior_emp_low_P0$lower,posterior_emp_low_P1$lower,posterior_emp_low_P2$lower),
                      upper=c(posterior_emp_high_P0$upper,posterior_emp_high_P1$upper,posterior_emp_high_P2$upper,posterior_emp_low_P0$upper,posterior_emp_low_P1$upper,posterior_emp_low_P2$upper))
by_item_lstm <- data.frame(item=c(posterior_lstm_high_P0$item,posterior_lstm_high_P1$item,posterior_lstm_high_P2$item,posterior_lstm_low_P0$item,posterior_lstm_low_P1$item,posterior_lstm_low_P2$item),
                           ROI=c(posterior_lstm_high_P0$ROI,posterior_lstm_high_P1$ROI,posterior_lstm_high_P2$ROI,posterior_lstm_low_P0$ROI,posterior_lstm_low_P1$ROI,posterior_lstm_low_P2$ROI),
                           coef=rep(c("GPE_high","GPE_low"),each=72),
                           mean=c(posterior_lstm_high_P0$mean,posterior_lstm_high_P1$mean,posterior_lstm_high_P2$mean,posterior_lstm_low_P0$mean,posterior_lstm_low_P1$mean,posterior_lstm_low_P2$mean),
                           lower=c(posterior_lstm_high_P0$lower,posterior_lstm_high_P1$lower,posterior_lstm_high_P2$lower,posterior_lstm_low_P0$lower,posterior_lstm_low_P1$lower,posterior_lstm_low_P2$lower),
                           upper=c(posterior_lstm_high_P0$upper,posterior_lstm_high_P1$upper,posterior_lstm_high_P2$upper,posterior_lstm_low_P0$upper,posterior_lstm_low_P1$upper,posterior_lstm_low_P2$upper))
by_item_gpt2 <- data.frame(item=c(posterior_gpt2_high_P0$item,posterior_gpt2_high_P1$item,posterior_gpt2_high_P2$item,posterior_gpt2_low_P0$item,posterior_gpt2_low_P1$item,posterior_gpt2_low_P2$item),
                           ROI=c(posterior_gpt2_high_P0$ROI,posterior_gpt2_high_P1$ROI,posterior_gpt2_high_P2$ROI,posterior_gpt2_low_P0$ROI,posterior_gpt2_low_P1$ROI,posterior_gpt2_low_P2$ROI),
                           coef=rep(c("GPE_high","GPE_low"),each=72),
                           mean=c(posterior_gpt2_high_P0$mean,posterior_gpt2_high_P1$mean,posterior_gpt2_high_P2$mean,posterior_gpt2_low_P0$mean,posterior_gpt2_low_P1$mean,posterior_gpt2_low_P2$mean),
                           lower=c(posterior_gpt2_high_P0$lower,posterior_gpt2_high_P1$lower,posterior_gpt2_high_P2$lower,posterior_gpt2_low_P0$lower,posterior_gpt2_low_P1$lower,posterior_gpt2_low_P2$lower),
                           upper=c(posterior_gpt2_high_P0$upper,posterior_gpt2_high_P1$upper,posterior_gpt2_high_P2$upper,posterior_gpt2_low_P0$upper,posterior_gpt2_low_P1$upper,posterior_gpt2_low_P2$upper))
by_item_lstmonly <- data.frame(item=c(posterior_lstmonly_high_P0$item,posterior_lstmonly_high_P1$item,posterior_lstmonly_high_P2$item,posterior_lstmonly_low_P0$item,posterior_lstmonly_low_P1$item,posterior_lstmonly_low_P2$item),
                           ROI=c(posterior_lstmonly_high_P0$ROI,posterior_lstmonly_high_P1$ROI,posterior_lstmonly_high_P2$ROI,posterior_lstmonly_low_P0$ROI,posterior_lstmonly_low_P1$ROI,posterior_lstmonly_low_P2$ROI),
                           coef=rep(c("GPE_high","GPE_low"),each=72),
                           mean=c(posterior_lstmonly_high_P0$mean,posterior_lstmonly_high_P1$mean,posterior_lstmonly_high_P2$mean,posterior_lstmonly_low_P0$mean,posterior_lstmonly_low_P1$mean,posterior_lstmonly_low_P2$mean),
                           lower=c(posterior_lstmonly_high_P0$lower,posterior_lstmonly_high_P1$lower,posterior_lstmonly_high_P2$lower,posterior_lstmonly_low_P0$lower,posterior_lstmonly_low_P1$lower,posterior_lstmonly_low_P2$lower),
                           upper=c(posterior_lstmonly_high_P0$upper,posterior_lstmonly_high_P1$upper,posterior_lstmonly_high_P2$upper,posterior_lstmonly_low_P0$upper,posterior_lstmonly_low_P1$upper,posterior_lstmonly_low_P2$upper))
by_item_gpt2only <- data.frame(item=c(posterior_gpt2only_high_P0$item,posterior_gpt2only_high_P1$item,posterior_gpt2only_high_P2$item,posterior_gpt2only_low_P0$item,posterior_gpt2only_low_P1$item,posterior_gpt2only_low_P2$item),
                               ROI=c(posterior_gpt2only_high_P0$ROI,posterior_gpt2only_high_P1$ROI,posterior_gpt2only_high_P2$ROI,posterior_gpt2only_low_P0$ROI,posterior_gpt2only_low_P1$ROI,posterior_gpt2only_low_P2$ROI),
                               coef=rep(c("GPE_high","GPE_low"),each=72),
                               mean=c(posterior_gpt2only_high_P0$mean,posterior_gpt2only_high_P1$mean,posterior_gpt2only_high_P2$mean,posterior_gpt2only_low_P0$mean,posterior_gpt2only_low_P1$mean,posterior_gpt2only_low_P2$mean),
                               lower=c(posterior_gpt2only_high_P0$lower,posterior_gpt2only_high_P1$lower,posterior_gpt2only_high_P2$lower,posterior_gpt2only_low_P0$lower,posterior_gpt2only_low_P1$lower,posterior_gpt2only_low_P2$lower),
                               upper=c(posterior_gpt2only_high_P0$upper,posterior_gpt2only_high_P1$upper,posterior_gpt2only_high_P2$upper,posterior_gpt2only_low_P0$upper,posterior_gpt2only_low_P1$upper,posterior_gpt2only_low_P2$upper))
by_item_nosurp <- data.frame(item=c(posterior_nosurp_high_P0$item,posterior_nosurp_high_P1$item,posterior_nosurp_high_P2$item,posterior_nosurp_low_P0$item,posterior_nosurp_low_P1$item,posterior_nosurp_low_P2$item),
                             ROI=c(posterior_nosurp_high_P0$ROI,posterior_nosurp_high_P1$ROI,posterior_nosurp_high_P2$ROI,posterior_nosurp_low_P0$ROI,posterior_nosurp_low_P1$ROI,posterior_nosurp_low_P2$ROI),
                             coef=rep(c("GPE_high","GPE_low"),each=72),
                             mean=c(posterior_nosurp_high_P0$mean,posterior_nosurp_high_P1$mean,posterior_nosurp_high_P2$mean,posterior_nosurp_low_P0$mean,posterior_nosurp_low_P1$mean,posterior_nosurp_low_P2$mean),
                             lower=c(posterior_nosurp_high_P0$lower,posterior_nosurp_high_P1$lower,posterior_nosurp_high_P2$lower,posterior_nosurp_low_P0$lower,posterior_nosurp_low_P1$lower,posterior_nosurp_low_P2$lower),
                             upper=c(posterior_nosurp_high_P0$upper,posterior_nosurp_high_P1$upper,posterior_nosurp_high_P2$upper,posterior_nosurp_low_P0$upper,posterior_nosurp_low_P1$upper,posterior_nosurp_low_P2$upper))
by_item_multi <- arrange(by_item_multi,item,ROI,coef)
by_item <- arrange(by_item,item,ROI,coef)
by_item_lstm <- arrange(by_item_lstm,item,ROI,coef)
by_item_gpt2 <- arrange(by_item_gpt2,item,ROI,coef)
by_item_lstmonly <- arrange(by_item_lstmonly,item,ROI,coef)
by_item_gpt2only <- arrange(by_item_gpt2only,item,ROI,coef)
by_item_nosurp <- arrange(by_item_nosurp,item,ROI,coef)
saveRDS(by_item_multi,"AttachmentAmbiguity/by_item_multi.rds")
saveRDS(by_item,"AttachmentAmbiguity/by_item.rds")
saveRDS(by_item_lstm,"AttachmentAmbiguity/by_item_lstm.rds")
saveRDS(by_item_gpt2,"AttachmentAmbiguity/by_item_gpt2.rds")
saveRDS(by_item_lstmonly,"AttachmentAmbiguity/by_item_lstmonly.rds")
saveRDS(by_item_gpt2only,"AttachmentAmbiguity/by_item_gpt2only.rds")
saveRDS(by_item_nosurp,"AttachmentAmbiguity/by_item_nosurp.rds")

by_construction <- data.frame(ROI=rep(c(0,1,2),2),coef=rep(c("GPE_high","GPE_low"),each=3),mean=c(mean(-emp_AA_P0_posterior_samp$b_ambiguity+0.5*emp_AA_P0_posterior_samp$b_height),mean(-emp_AA_P1_posterior_samp$b_ambiguity+0.5*emp_AA_P1_posterior_samp$b_height),mean(-emp_AA_P2_posterior_samp$b_ambiguity+0.5*emp_AA_P2_posterior_samp$b_height),mean(-emp_AA_P0_posterior_samp$b_ambiguity-0.5*emp_AA_P0_posterior_samp$b_height),mean(-emp_AA_P1_posterior_samp$b_ambiguity-0.5*emp_AA_P1_posterior_samp$b_height),mean(-emp_AA_P2_posterior_samp$b_ambiguity-0.5*emp_AA_P2_posterior_samp$b_height)),
                              lower=c(quantile(-emp_AA_P0_posterior_samp$b_ambiguity+0.5*emp_AA_P0_posterior_samp$b_height,0.025),quantile(-emp_AA_P1_posterior_samp$b_ambiguity+0.5*emp_AA_P1_posterior_samp$b_height,0.025),quantile(-emp_AA_P2_posterior_samp$b_ambiguity+0.5*emp_AA_P2_posterior_samp$b_height,0.025),quantile(-emp_AA_P0_posterior_samp$b_ambiguity-0.5*emp_AA_P0_posterior_samp$b_height,0.025),quantile(-emp_AA_P1_posterior_samp$b_ambiguity-0.5*emp_AA_P1_posterior_samp$b_height,0.025),quantile(-emp_AA_P2_posterior_samp$b_ambiguity-0.5*emp_AA_P2_posterior_samp$b_height,0.025)),
                              upper=c(quantile(-emp_AA_P0_posterior_samp$b_ambiguity+0.5*emp_AA_P0_posterior_samp$b_height,0.975),quantile(-emp_AA_P1_posterior_samp$b_ambiguity+0.5*emp_AA_P1_posterior_samp$b_height,0.975),quantile(-emp_AA_P2_posterior_samp$b_ambiguity+0.5*emp_AA_P2_posterior_samp$b_height,0.975),quantile(-emp_AA_P0_posterior_samp$b_ambiguity-0.5*emp_AA_P0_posterior_samp$b_height,0.975),quantile(-emp_AA_P1_posterior_samp$b_ambiguity-0.5*emp_AA_P1_posterior_samp$b_height,0.975),quantile(-emp_AA_P2_posterior_samp$b_ambiguity-0.5*emp_AA_P2_posterior_samp$b_height,0.975)))
by_construction_lstm <- data.frame(ROI=rep(c(0,1,2),2),coef=rep(c("GPE_high","GPE_low"),each=3),mean=c(mean(-lstm_AA_P0_posterior_samp$b_ambiguity+0.5*lstm_AA_P0_posterior_samp$b_height),mean(-lstm_AA_P1_posterior_samp$b_ambiguity+0.5*lstm_AA_P1_posterior_samp$b_height),mean(-lstm_AA_P2_posterior_samp$b_ambiguity+0.5*lstm_AA_P2_posterior_samp$b_height),mean(-lstm_AA_P0_posterior_samp$b_ambiguity-0.5*lstm_AA_P0_posterior_samp$b_height),mean(-lstm_AA_P1_posterior_samp$b_ambiguity-0.5*lstm_AA_P1_posterior_samp$b_height),mean(-lstm_AA_P2_posterior_samp$b_ambiguity-0.5*lstm_AA_P2_posterior_samp$b_height)),
                                   lower=c(quantile(-lstm_AA_P0_posterior_samp$b_ambiguity+0.5*lstm_AA_P0_posterior_samp$b_height,0.025),quantile(-lstm_AA_P1_posterior_samp$b_ambiguity+0.5*lstm_AA_P1_posterior_samp$b_height,0.025),quantile(-lstm_AA_P2_posterior_samp$b_ambiguity+0.5*lstm_AA_P2_posterior_samp$b_height,0.025),quantile(-lstm_AA_P0_posterior_samp$b_ambiguity-0.5*lstm_AA_P0_posterior_samp$b_height,0.025),quantile(-lstm_AA_P1_posterior_samp$b_ambiguity-0.5*lstm_AA_P1_posterior_samp$b_height,0.025),quantile(-lstm_AA_P2_posterior_samp$b_ambiguity-0.5*lstm_AA_P2_posterior_samp$b_height,0.025)),
                                   upper=c(quantile(-lstm_AA_P0_posterior_samp$b_ambiguity+0.5*lstm_AA_P0_posterior_samp$b_height,0.975),quantile(-lstm_AA_P1_posterior_samp$b_ambiguity+0.5*lstm_AA_P1_posterior_samp$b_height,0.975),quantile(-lstm_AA_P2_posterior_samp$b_ambiguity+0.5*lstm_AA_P2_posterior_samp$b_height,0.975),quantile(-lstm_AA_P0_posterior_samp$b_ambiguity-0.5*lstm_AA_P0_posterior_samp$b_height,0.975),quantile(-lstm_AA_P1_posterior_samp$b_ambiguity-0.5*lstm_AA_P1_posterior_samp$b_height,0.975),quantile(-lstm_AA_P2_posterior_samp$b_ambiguity-0.5*lstm_AA_P2_posterior_samp$b_height,0.975)))
by_construction_gpt2 <- data.frame(ROI=rep(c(0,1,2),2),coef=rep(c("GPE_high","GPE_low"),each=3),mean=c(mean(-gpt2_AA_P0_posterior_samp$b_ambiguity+0.5*gpt2_AA_P0_posterior_samp$b_height),mean(-gpt2_AA_P1_posterior_samp$b_ambiguity+0.5*gpt2_AA_P1_posterior_samp$b_height),mean(-gpt2_AA_P2_posterior_samp$b_ambiguity+0.5*gpt2_AA_P2_posterior_samp$b_height),mean(-gpt2_AA_P0_posterior_samp$b_ambiguity-0.5*gpt2_AA_P0_posterior_samp$b_height),mean(-gpt2_AA_P1_posterior_samp$b_ambiguity-0.5*gpt2_AA_P1_posterior_samp$b_height),mean(-gpt2_AA_P2_posterior_samp$b_ambiguity-0.5*gpt2_AA_P2_posterior_samp$b_height)),
                                   lower=c(quantile(-gpt2_AA_P0_posterior_samp$b_ambiguity+0.5*gpt2_AA_P0_posterior_samp$b_height,0.025),quantile(-gpt2_AA_P1_posterior_samp$b_ambiguity+0.5*gpt2_AA_P1_posterior_samp$b_height,0.025),quantile(-gpt2_AA_P2_posterior_samp$b_ambiguity+0.5*gpt2_AA_P2_posterior_samp$b_height,0.025),quantile(-gpt2_AA_P0_posterior_samp$b_ambiguity-0.5*gpt2_AA_P0_posterior_samp$b_height,0.025),quantile(-gpt2_AA_P1_posterior_samp$b_ambiguity-0.5*gpt2_AA_P1_posterior_samp$b_height,0.025),quantile(-gpt2_AA_P2_posterior_samp$b_ambiguity-0.5*gpt2_AA_P2_posterior_samp$b_height,0.025)),
                                   upper=c(quantile(-gpt2_AA_P0_posterior_samp$b_ambiguity+0.5*gpt2_AA_P0_posterior_samp$b_height,0.975),quantile(-gpt2_AA_P1_posterior_samp$b_ambiguity+0.5*gpt2_AA_P1_posterior_samp$b_height,0.975),quantile(-gpt2_AA_P2_posterior_samp$b_ambiguity+0.5*gpt2_AA_P2_posterior_samp$b_height,0.975),quantile(-gpt2_AA_P0_posterior_samp$b_ambiguity-0.5*gpt2_AA_P0_posterior_samp$b_height,0.975),quantile(-gpt2_AA_P1_posterior_samp$b_ambiguity-0.5*gpt2_AA_P1_posterior_samp$b_height,0.975),quantile(-gpt2_AA_P2_posterior_samp$b_ambiguity-0.5*gpt2_AA_P2_posterior_samp$b_height,0.975)))
by_construction_gpt2only <- data.frame(ROI=rep(c(0,1,2),2),coef=rep(c("GPE_high","GPE_low"),each=3),mean=c(mean(-gpt2only_AA_P0_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P0_posterior_samp$b_height),mean(-gpt2only_AA_P1_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P1_posterior_samp$b_height),mean(-gpt2only_AA_P2_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P2_posterior_samp$b_height),mean(-gpt2only_AA_P0_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P0_posterior_samp$b_height),mean(-gpt2only_AA_P1_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P1_posterior_samp$b_height),mean(-gpt2only_AA_P2_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P2_posterior_samp$b_height)),
                                       lower=c(quantile(-gpt2only_AA_P0_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P0_posterior_samp$b_height,0.025),quantile(-gpt2only_AA_P1_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P1_posterior_samp$b_height,0.025),quantile(-gpt2only_AA_P2_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P2_posterior_samp$b_height,0.025),quantile(-gpt2only_AA_P0_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P0_posterior_samp$b_height,0.025),quantile(-gpt2only_AA_P1_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P1_posterior_samp$b_height,0.025),quantile(-gpt2only_AA_P2_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P2_posterior_samp$b_height,0.025)),
                                       upper=c(quantile(-gpt2only_AA_P0_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P0_posterior_samp$b_height,0.975),quantile(-gpt2only_AA_P1_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P1_posterior_samp$b_height,0.975),quantile(-gpt2only_AA_P2_posterior_samp$b_ambiguity+0.5*gpt2only_AA_P2_posterior_samp$b_height,0.975),quantile(-gpt2only_AA_P0_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P0_posterior_samp$b_height,0.975),quantile(-gpt2only_AA_P1_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P1_posterior_samp$b_height,0.975),quantile(-gpt2only_AA_P2_posterior_samp$b_ambiguity-0.5*gpt2only_AA_P2_posterior_samp$b_height,0.975)))
by_construction_lstmonly <- data.frame(ROI=rep(c(0,1,2),2),coef=rep(c("GPE_high","GPE_low"),each=3),mean=c(mean(-lstmonly_AA_P0_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P0_posterior_samp$b_height),mean(-lstmonly_AA_P1_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P1_posterior_samp$b_height),mean(-lstmonly_AA_P2_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P2_posterior_samp$b_height),mean(-lstmonly_AA_P0_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P0_posterior_samp$b_height),mean(-lstmonly_AA_P1_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P1_posterior_samp$b_height),mean(-lstmonly_AA_P2_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P2_posterior_samp$b_height)),
                                       lower=c(quantile(-lstmonly_AA_P0_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P0_posterior_samp$b_height,0.025),quantile(-lstmonly_AA_P1_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P1_posterior_samp$b_height,0.025),quantile(-lstmonly_AA_P2_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P2_posterior_samp$b_height,0.025),quantile(-lstmonly_AA_P0_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P0_posterior_samp$b_height,0.025),quantile(-lstmonly_AA_P1_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P1_posterior_samp$b_height,0.025),quantile(-lstmonly_AA_P2_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P2_posterior_samp$b_height,0.025)),
                                       upper=c(quantile(-lstmonly_AA_P0_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P0_posterior_samp$b_height,0.975),quantile(-lstmonly_AA_P1_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P1_posterior_samp$b_height,0.975),quantile(-lstmonly_AA_P2_posterior_samp$b_ambiguity+0.5*lstmonly_AA_P2_posterior_samp$b_height,0.975),quantile(-lstmonly_AA_P0_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P0_posterior_samp$b_height,0.975),quantile(-lstmonly_AA_P1_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P1_posterior_samp$b_height,0.975),quantile(-lstmonly_AA_P2_posterior_samp$b_ambiguity-0.5*lstmonly_AA_P2_posterior_samp$b_height,0.975)))
by_construction_nosurp <- data.frame(ROI=rep(c(0,1,2),2),coef=rep(c("GPE_high","GPE_low"),each=3),mean=c(mean(-nosurp_AA_P0_posterior_samp$b_ambiguity+0.5*nosurp_AA_P0_posterior_samp$b_height),mean(-nosurp_AA_P1_posterior_samp$b_ambiguity+0.5*nosurp_AA_P1_posterior_samp$b_height),mean(-nosurp_AA_P2_posterior_samp$b_ambiguity+0.5*nosurp_AA_P2_posterior_samp$b_height),mean(-nosurp_AA_P0_posterior_samp$b_ambiguity-0.5*nosurp_AA_P0_posterior_samp$b_height),mean(-nosurp_AA_P1_posterior_samp$b_ambiguity-0.5*nosurp_AA_P1_posterior_samp$b_height),mean(-nosurp_AA_P2_posterior_samp$b_ambiguity-0.5*nosurp_AA_P2_posterior_samp$b_height)),
                                     lower=c(quantile(-nosurp_AA_P0_posterior_samp$b_ambiguity+0.5*nosurp_AA_P0_posterior_samp$b_height,0.025),quantile(-nosurp_AA_P1_posterior_samp$b_ambiguity+0.5*nosurp_AA_P1_posterior_samp$b_height,0.025),quantile(-nosurp_AA_P2_posterior_samp$b_ambiguity+0.5*nosurp_AA_P2_posterior_samp$b_height,0.025),quantile(-nosurp_AA_P0_posterior_samp$b_ambiguity-0.5*nosurp_AA_P0_posterior_samp$b_height,0.025),quantile(-nosurp_AA_P1_posterior_samp$b_ambiguity-0.5*nosurp_AA_P1_posterior_samp$b_height,0.025),quantile(-nosurp_AA_P2_posterior_samp$b_ambiguity-0.5*nosurp_AA_P2_posterior_samp$b_height,0.025)),
                                     upper=c(quantile(-nosurp_AA_P0_posterior_samp$b_ambiguity+0.5*nosurp_AA_P0_posterior_samp$b_height,0.975),quantile(-nosurp_AA_P1_posterior_samp$b_ambiguity+0.5*nosurp_AA_P1_posterior_samp$b_height,0.975),quantile(-nosurp_AA_P2_posterior_samp$b_ambiguity+0.5*nosurp_AA_P2_posterior_samp$b_height,0.975),quantile(-nosurp_AA_P0_posterior_samp$b_ambiguity-0.5*nosurp_AA_P0_posterior_samp$b_height,0.975),quantile(-nosurp_AA_P1_posterior_samp$b_ambiguity-0.5*nosurp_AA_P1_posterior_samp$b_height,0.975),quantile(-nosurp_AA_P2_posterior_samp$b_ambiguity-0.5*nosurp_AA_P2_posterior_samp$b_height,0.975)))

saveRDS(by_construction,"AttachmentAmbiguity/by_construction.rds")
saveRDS(by_construction_lstm,"AttachmentAmbiguity/by_construction_lstm.rds")
saveRDS(by_construction_gpt2,"AttachmentAmbiguity/by_construction_gpt2.rds")
saveRDS(by_construction_lstmonly,"AttachmentAmbiguity/by_construction_lstmonly.rds")
saveRDS(by_construction_gpt2only,"AttachmentAmbiguity/by_construction_gpt2only.rds")
saveRDS(by_construction_nosurp,"AttachmentAmbiguity/by_construction_nosurp.rds")

#P0
sampled_correlations_P0 <- data.frame(Correlation=rep(NA,10000),EOI=rep(c("GPE_high","GPE_low"),5000),model=rep(c("lstm","lstm","gpt2","gpt2","nosurp","nosurp","lstmonly","lstmonly","gpt2only","gpt2only"),1000),ROI=0)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,48),lstm=rep(NA,48),gpt2=rep(NA,48),nosurp=rep(NA,48),lstmonly=rep(NA,48),gpt2only=rep(NA,48),EOI=c(rep("GPE_high",24),rep("GPE_low",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,5] <- sample(lstmonly_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,6] <- sample(gpt2only_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,5] <- sample(lstmonly_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,6] <- sample(gpt2only_AA_P0_posterior_samp[,j+24+ncols],1)
  }
  sampled_correlations_P0[((i-1)*10+1):((i-1)*10+10),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,5])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,5])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,6])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,6])$estimate)
}
#P0


#P1
sampled_correlations_P1 <- data.frame(Correlation=rep(NA,10000),EOI=rep(c("GPE_high","GPE_low"),5000),model=rep(c("lstm","lstm","gpt2","gpt2","nosurp","nosurp","lstmonly","lstmonly","gpt2only","gpt2only"),1000),ROI=1)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,48),lstm=rep(NA,48),gpt2=rep(NA,48),nosurp=rep(NA,48),lstmonly=rep(NA,48),gpt2only=rep(NA,48),EOI=c(rep("GPE_high",24),rep("GPE_low",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,5] <- sample(lstmonly_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,6] <- sample(gpt2only_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,5] <- sample(lstmonly_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,6] <- sample(gpt2only_AA_P1_posterior_samp[,j+24+ncols],1)
  }
  sampled_correlations_P1[((i-1)*10+1):((i-1)*10+10),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,5])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,5])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,6])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,6])$estimate)
}
#P1



#P2
sampled_correlations_P2 <- data.frame(Correlation=rep(NA,10000),EOI=rep(c("GPE_high","GPE_low"),5000),model=rep(c("lstm","lstm","gpt2","gpt2","nosurp","nosurp","lstmonly","lstmonly","gpt2only","gpt2only"),1000),ROI=2)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,48),lstm=rep(NA,48),gpt2=rep(NA,48),nosurp=rep(NA,48),lstmonly=rep(NA,48),gpt2only=rep(NA,48),EOI=c(rep("GPE_high",24),rep("GPE_low",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,5] <- sample(lstmonly_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,6] <- sample(gpt2only_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,5] <- sample(lstmonly_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,6] <- sample(gpt2only_AA_P2_posterior_samp[,j+24+ncols],1)
  }
  sampled_correlations_P2[((i-1)*10+1):((i-1)*10+10),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,5])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,5])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,6])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,6])$estimate)
}
#P2


sampled_correlations_maxregion <- rbind(sampled_correlations_P0[sampled_correlations_P0$EOI=="GPE_low",],sampled_correlations_P1[sampled_correlations_P1$EOI=="GPE_high",])
for(i in unique(sampled_correlations_maxregion$EOI)){
  for(j in unique(sampled_correlations_maxregion$model)){
    hist(sampled_correlations_maxregion$Correlation[sampled_correlations_maxregion$EOI==i&sampled_correlations_maxregion$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
#saveRDS(sampled_correlations_maxregion,"sampled_correlations_maxregion_AA.rds")

for_plotting_AA <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=mean)
colnames(for_plotting_AA) <- c("EOI","model","Correlation")
for_plotting_AA$SE <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=sd)$x
for_plotting_AA$ROI <- 0

temp <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=sd)$x
temp$ROI <- 1
for_plotting_AA <- rbind(for_plotting_AA,temp)
rm(temp)

temp <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=sd)$x
temp$ROI <- 2
for_plotting_AA <- rbind(for_plotting_AA,temp)
rm(temp)

#saveRDS(for_plotting_AA,"for_plotting_AA.rds")

for_plotting_AA_max <- for_plotting_AA[(for_plotting_AA$EOI=="GPE_low"&for_plotting_AA$ROI==0)|(for_plotting_AA$EOI=="GPE_high"&for_plotting_AA$ROI==1),]


ggplot(for_plotting_AA_max,aes(x=Correlation,y=EOI,fill=model))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))


ggplot(for_plotting_AA,aes(x=Correlation,y=EOI,fill=model))+
  facet_grid(~ROI)+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))





cor.test(posterior_emp_high_P0$mean,posterior_gpt2only_high_P0$mean)$estimate
cor.test(posterior_emp_high_P0$mean,posterior_lstmonly_high_P0$mean)$estimate
cor.test(posterior_emp_high_P0$mean,posterior_gpt2_high_P0$mean)$estimate
cor.test(posterior_emp_high_P0$mean,posterior_lstm_high_P0$mean)$estimate
cor.test(posterior_emp_high_P0$mean,posterior_nosurp_high_P0$mean)$estimate
cor.test(posterior_emp_low_P0$mean,posterior_gpt2only_low_P0$mean)$estimate
cor.test(posterior_emp_low_P0$mean,posterior_lstmonly_low_P0$mean)$estimate
cor.test(posterior_emp_low_P0$mean,posterior_gpt2_low_P0$mean)$estimate
cor.test(posterior_emp_low_P0$mean,posterior_lstm_low_P0$mean)$estimate
cor.test(posterior_emp_low_P0$mean,posterior_nosurp_low_P0$mean)$estimate
cor.test(posterior_emp_high_P1$mean,posterior_gpt2only_high_P1$mean)$estimate
cor.test(posterior_emp_high_P1$mean,posterior_lstmonly_high_P1$mean)$estimate
cor.test(posterior_emp_high_P1$mean,posterior_gpt2_high_P1$mean)$estimate
cor.test(posterior_emp_high_P1$mean,posterior_lstm_high_P1$mean)$estimate
cor.test(posterior_emp_high_P1$mean,posterior_nosurp_high_P1$mean)$estimate
cor.test(posterior_emp_low_P1$mean,posterior_gpt2only_low_P1$mean)$estimate
cor.test(posterior_emp_low_P1$mean,posterior_lstmonly_low_P1$mean)$estimate
cor.test(posterior_emp_low_P1$mean,posterior_gpt2_low_P1$mean)$estimate
cor.test(posterior_emp_low_P1$mean,posterior_lstm_low_P1$mean)$estimate
cor.test(posterior_emp_low_P1$mean,posterior_nosurp_low_P1$mean)$estimate
cor.test(posterior_emp_high_P2$mean,posterior_gpt2only_high_P2$mean)$estimate
cor.test(posterior_emp_high_P2$mean,posterior_lstmonly_high_P2$mean)$estimate
cor.test(posterior_emp_high_P2$mean,posterior_gpt2_high_P2$mean)$estimate
cor.test(posterior_emp_high_P2$mean,posterior_lstm_high_P2$mean)$estimate
cor.test(posterior_emp_high_P2$mean,posterior_nosurp_high_P2$mean)$estimate
cor.test(posterior_emp_low_P2$mean,posterior_gpt2only_low_P2$mean)$estimate
cor.test(posterior_emp_low_P2$mean,posterior_lstmonly_low_P2$mean)$estimate
cor.test(posterior_emp_low_P2$mean,posterior_gpt2_low_P2$mean)$estimate
cor.test(posterior_emp_low_P2$mean,posterior_lstm_low_P2$mean)$estimate
cor.test(posterior_emp_low_P2$mean,posterior_nosurp_low_P2$mean)$estimate


posterior_emp_high_P0$model <- "emp" 
posterior_emp_high_P1$model <- "emp" 
posterior_emp_high_P2$model <- "emp" 
posterior_lstm_high_P0$model <- "lstm" 
posterior_lstm_high_P1$model <- "lstm" 
posterior_lstm_high_P2$model <- "lstm" 
posterior_gpt2_high_P0$model <- "gpt2" 
posterior_gpt2_high_P1$model <- "gpt2" 
posterior_gpt2_high_P2$model <- "gpt2" 
posterior_lstmonly_high_P0$model <- "lstmonly" 
posterior_lstmonly_high_P1$model <- "lstmonly" 
posterior_lstmonly_high_P2$model <- "lstmonly" 
posterior_gpt2only_high_P0$model <- "gpt2only" 
posterior_gpt2only_high_P1$model <- "gpt2only" 
posterior_gpt2only_high_P2$model <- "gpt2only" 
posterior_nosurp_high_P0$model <- "nosurp" 
posterior_nosurp_high_P1$model <- "nosurp" 
posterior_nosurp_high_P2$model <- "nosurp"  
posterior_emp_low_P0$model <- "emp" 
posterior_emp_low_P1$model <- "emp" 
posterior_emp_low_P2$model <- "emp" 
posterior_lstm_low_P0$model <- "lstm" 
posterior_lstm_low_P1$model <- "lstm" 
posterior_lstm_low_P2$model <- "lstm" 
posterior_gpt2_low_P0$model <- "gpt2" 
posterior_gpt2_low_P1$model <- "gpt2" 
posterior_gpt2_low_P2$model <- "gpt2" 
posterior_lstmonly_low_P0$model <- "lstmonly" 
posterior_lstmonly_low_P1$model <- "lstmonly" 
posterior_lstmonly_low_P2$model <- "lstmonly" 
posterior_gpt2only_low_P0$model <- "gpt2only" 
posterior_gpt2only_low_P1$model <- "gpt2only" 
posterior_gpt2only_low_P2$model <- "gpt2only" 
posterior_nosurp_low_P0$model <- "nosurp" 
posterior_nosurp_low_P1$model <- "nosurp" 
posterior_nosurp_low_P2$model <- "nosurp" 


df_pointestimate <- 
  data.frame(emp=c(posterior_emp_high_P0$mean,posterior_emp_low_P0$mean,posterior_emp_high_P1$mean,posterior_emp_low_P1$mean,posterior_emp_high_P2$mean,posterior_emp_low_P2$mean),
             lstm=c(posterior_lstm_high_P0$mean,posterior_lstm_low_P0$mean,posterior_lstm_high_P1$mean,posterior_lstm_low_P1$mean,posterior_lstm_high_P2$mean,posterior_lstm_low_P2$mean),
             gpt2=c(posterior_gpt2_high_P0$mean,posterior_gpt2_low_P0$mean,posterior_gpt2_high_P1$mean,posterior_gpt2_low_P1$mean,posterior_gpt2_high_P2$mean,posterior_gpt2_low_P2$mean),
             lstmonly=c(posterior_lstmonly_high_P0$mean,posterior_lstmonly_low_P0$mean,posterior_lstmonly_high_P1$mean,posterior_lstmonly_low_P1$mean,posterior_lstmonly_high_P2$mean,posterior_lstmonly_low_P2$mean),
             gpt2only=c(posterior_gpt2only_high_P0$mean,posterior_gpt2only_low_P0$mean,posterior_gpt2only_high_P1$mean,posterior_gpt2only_low_P1$mean,posterior_gpt2only_high_P2$mean,posterior_gpt2only_low_P2$mean),
             nosurp=c(posterior_nosurp_high_P0$mean,posterior_nosurp_low_P0$mean,posterior_nosurp_high_P1$mean,posterior_nosurp_low_P1$mean,posterior_nosurp_high_P2$mean,posterior_nosurp_low_P2$mean),
             EOI=rep(c("GPE_high","GPE_low"),each=24),
             item = rep(49:72,6),
             ROI=rep(c(0,1,2),each=48))
saveRDS(df_pointestimate,"df_pointestimate_AA.rds")
