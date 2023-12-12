library(lme4)
library(tidyr)
library(stringr)
library(posterior)
library(tidybayes)
library(tidyverse)
library(dplyr)
library(brms)

emp_GP_P0 <- readRDS("GP_logadd_prior1/ClassicGP_lognorm_prior_0.rds")
emp_GP_P1 <- readRDS("GP_logadd_prior1/ClassicGP_lognorm_prior_1.rds")
emp_GP_P2 <- readRDS("GP_logadd_prior1/ClassicGP_lognorm_prior_2.rds")
lstm_GP_P0 <- readRDS("GP_logadd_prior1/brm_predicted_lstm_GP_P0_logadd.rds")
lstm_GP_P1 <- readRDS("GP_logadd_prior1/brm_predicted_lstm_GP_P1_logadd.rds")
lstm_GP_P2 <- readRDS("GP_logadd_prior1/brm_predicted_lstm_GP_P2_logadd.rds")
gpt2_GP_P0 <- readRDS("GP_logadd_prior1/brm_predicted_gpt2_GP_P0_logadd.rds")
gpt2_GP_P1 <- readRDS("GP_logadd_prior1/brm_predicted_gpt2_GP_P1_logadd.rds")
gpt2_GP_P2 <- readRDS("GP_logadd_prior1/brm_predicted_gpt2_GP_P2_logadd.rds")
nosurp_GP_P0 <- readRDS("GP_logadd_prior1/brm_predicted_nosurp_GP_P0_logadd.rds")
nosurp_GP_P1 <- readRDS("GP_logadd_prior1/brm_predicted_nosurp_GP_P1_logadd.rds")
nosurp_GP_P2 <- readRDS("GP_logadd_prior1/brm_predicted_nosurp_GP_P2_logadd.rds")



posterior_samp <- posterior_samples(emp_GP_P0)
randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(AMBUAMB|SZM|Intercept)',colnames(posterior_samp))]
saveRDS(randomslope_names,"GP_logadd_prior1/GP_randomslopesnames.rds")
randomslope_names <- readRDS("GP_logadd_prior1/GP_randomslopesnames.rds")

emp_GP_P0_posterior_samp <- posterior_samples(emp_GP_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
lstm_GP_P0_posterior_samp <- posterior_samples(lstm_GP_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
gpt2_GP_P0_posterior_samp <- posterior_samples(gpt2_GP_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
nosurp_GP_P0_posterior_samp <- posterior_samples(nosurp_GP_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
emp_GP_P1_posterior_samp <- posterior_samples(emp_GP_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
lstm_GP_P1_posterior_samp <- posterior_samples(lstm_GP_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
gpt2_GP_P1_posterior_samp <- posterior_samples(gpt2_GP_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
nosurp_GP_P1_posterior_samp <- posterior_samples(nosurp_GP_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
emp_GP_P2_posterior_samp <- posterior_samples(emp_GP_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
lstm_GP_P2_posterior_samp <- posterior_samples(lstm_GP_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
gpt2_GP_P2_posterior_samp <- posterior_samples(gpt2_GP_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
nosurp_GP_P2_posterior_samp <- posterior_samples(nosurp_GP_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))

rm(emp_GP_P0, lstm_GP_P0, gpt2_GP_P0, nosurp_GP_P0, emp_GP_P1, lstm_GP_P1,
gpt2_GP_P1, nosurp_GP_P1, emp_GP_P2, lstm_GP_P2, gpt2_GP_P2, nosurp_GP_P2,posterior_samp)
saveRDS(emp_GP_P0_posterior_samp,"GP_logadd_prior1/emp_GP_P0_posterior_samp.rds")
saveRDS(lstm_GP_P0_posterior_samp,"GP_logadd_prior1/lstm_GP_P0_posterior_samp.rds")
saveRDS(gpt2_GP_P0_posterior_samp,"GP_logadd_prior1/gpt2_GP_P0_posterior_samp.rds")
saveRDS(nosurp_GP_P0_posterior_samp,"GP_logadd_prior1/nosurp_GP_P0_posterior_samp.rds")
saveRDS(emp_GP_P1_posterior_samp,"GP_logadd_prior1/emp_GP_P1_posterior_samp.rds")
saveRDS(lstm_GP_P1_posterior_samp,"GP_logadd_prior1/lstm_GP_P1_posterior_samp.rds")
saveRDS(gpt2_GP_P1_posterior_samp,"GP_logadd_prior1/gpt2_GP_P1_posterior_samp.rds")
saveRDS(nosurp_GP_P1_posterior_samp,"GP_logadd_prior1/nosurp_GP_P1_posterior_samp.rds")
saveRDS(emp_GP_P2_posterior_samp,"GP_logadd_prior1/emp_GP_P2_posterior_samp.rds")
saveRDS(lstm_GP_P2_posterior_samp,"GP_logadd_prior1/lstm_GP_P2_posterior_samp.rds")
saveRDS(gpt2_GP_P2_posterior_samp,"GP_logadd_prior1/gpt2_GP_P2_posterior_samp.rds")
saveRDS(nosurp_GP_P2_posterior_samp,"GP_logadd_prior1/nosurp_GP_P2_posterior_samp.rds")

emp_GP_P0_posterior_samp <- readRDS("GP_logadd_prior1/emp_GP_P0_posterior_samp.rds")
lstm_GP_P0_posterior_samp <- readRDS("GP_logadd_prior1/lstm_GP_P0_posterior_samp.rds")
gpt2_GP_P0_posterior_samp <- readRDS("GP_logadd_prior1/gpt2_GP_P0_posterior_samp.rds")
nosurp_GP_P0_posterior_samp <- readRDS("GP_logadd_prior1/nosurp_GP_P0_posterior_samp.rds")
emp_GP_P1_posterior_samp <- readRDS("GP_logadd_prior1/emp_GP_P1_posterior_samp.rds")
lstm_GP_P1_posterior_samp <- readRDS("GP_logadd_prior1/lstm_GP_P1_posterior_samp.rds")
gpt2_GP_P1_posterior_samp <- readRDS("GP_logadd_prior1/gpt2_GP_P1_posterior_samp.rds")
nosurp_GP_P1_posterior_samp <- readRDS("GP_logadd_prior1/nosurp_GP_P1_posterior_samp.rds")
emp_GP_P2_posterior_samp <- readRDS("GP_logadd_prior1/emp_GP_P2_posterior_samp.rds")
lstm_GP_P2_posterior_samp <- readRDS("GP_logadd_prior1/lstm_GP_P2_posterior_samp.rds")
gpt2_GP_P2_posterior_samp <- readRDS("GP_logadd_prior1/gpt2_GP_P2_posterior_samp.rds")
nosurp_GP_P2_posterior_samp <- readRDS("GP_logadd_prior1/nosurp_GP_P2_posterior_samp.rds")




posterior_emp_MVRR_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=0)
posterior_emp_NPS_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=0)
posterior_emp_NPZ_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=0)
posterior_emp_MVRR_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=1)
posterior_emp_NPS_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=1)
posterior_emp_NPZ_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=1)
posterior_emp_MVRR_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=2)
posterior_emp_NPS_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=2)
posterior_emp_NPZ_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=2)
posterior_lstm_MVRR_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=0)
posterior_lstm_NPS_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=0)
posterior_lstm_NPZ_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=0)
posterior_lstm_MVRR_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=1)
posterior_lstm_NPS_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=1)
posterior_lstm_NPZ_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=1)
posterior_lstm_MVRR_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=2)
posterior_lstm_NPS_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=2)
posterior_lstm_NPZ_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=2)
posterior_gpt2_MVRR_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=0)
posterior_gpt2_NPS_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=0)
posterior_gpt2_NPZ_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=0)
posterior_gpt2_MVRR_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=1)
posterior_gpt2_NPS_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=1)
posterior_gpt2_NPZ_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=1)
posterior_gpt2_MVRR_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=2)
posterior_gpt2_NPS_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=2)
posterior_gpt2_NPZ_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=2)
posterior_nosurp_MVRR_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=0)
posterior_nosurp_NPS_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=0)
posterior_nosurp_NPZ_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=0)
posterior_nosurp_MVRR_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=1)
posterior_nosurp_NPS_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=1)
posterior_nosurp_NPZ_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=1)
posterior_nosurp_MVRR_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_MVRR",ROI=2)
posterior_nosurp_NPS_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPS",ROI=2)
posterior_nosurp_NPZ_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="GPE_NPZ",ROI=2)

posterior_emp_unambMVRR_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambMVRR",ROI=0)
posterior_emp_unambNPS_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambNPS",ROI=0)
posterior_emp_unambNPZ_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambNPZ",ROI=0)
posterior_emp_unambMVRR_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambMVRR",ROI=1)
posterior_emp_unambNPS_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambNPS",ROI=1)
posterior_emp_unambNPZ_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambNPZ",ROI=1)
posterior_emp_unambMVRR_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambMVRR",ROI=2)
posterior_emp_unambNPS_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambNPS",ROI=2)
posterior_emp_unambNPZ_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=1:24,EOI="unambNPZ",ROI=2)





ncols <- ncol(emp_GP_P0_posterior_samp)
emp_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,(1+6):(24+6)]+emp_GP_P0_posterior_samp[,2]+emp_GP_P0_posterior_samp[,(1+30):(24+30)])-exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,(1+6):(24+6)])
emp_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,3]+emp_GP_P0_posterior_samp[,(1+6):(24+6)]+emp_GP_P0_posterior_samp[,(1+54):(24+54)]+emp_GP_P0_posterior_samp[,2]+emp_GP_P0_posterior_samp[,5]+emp_GP_P0_posterior_samp[,(1+30):(24+30)]+emp_GP_P0_posterior_samp[,(1+102):(24+102)])-exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,3]+emp_GP_P0_posterior_samp[,(1+6):(24+6)]+emp_GP_P0_posterior_samp[,(1+54):(24+54)])
emp_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,4]+emp_GP_P0_posterior_samp[,(1+6):(24+6)]+emp_GP_P0_posterior_samp[,(1+78):(24+78)]+emp_GP_P0_posterior_samp[,2]+emp_GP_P0_posterior_samp[,6]+emp_GP_P0_posterior_samp[,(1+30):(24+30)]+emp_GP_P0_posterior_samp[,(1+126):(24+126)])-exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,4]+emp_GP_P0_posterior_samp[,(1+6):(24+6)]+emp_GP_P0_posterior_samp[,(1+78):(24+78)])
emp_GP_P0_posterior_samp[,(73+ncols):(96+ncols)] <- exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,(1+6):(24+6)])
emp_GP_P0_posterior_samp[,(97+ncols):(120+ncols)] <- exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,3]+emp_GP_P0_posterior_samp[,(1+6):(24+6)]+emp_GP_P0_posterior_samp[,(1+54):(24+54)])
emp_GP_P0_posterior_samp[,(121+ncols):(144+ncols)] <- exp(emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,4]+emp_GP_P0_posterior_samp[,(1+6):(24+6)]+emp_GP_P0_posterior_samp[,(1+78):(24+78)])
emp_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,(1+6):(24+6)]+emp_GP_P1_posterior_samp[,2]+emp_GP_P1_posterior_samp[,(1+30):(24+30)])-exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,(1+6):(24+6)])
emp_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,3]+emp_GP_P1_posterior_samp[,(1+6):(24+6)]+emp_GP_P1_posterior_samp[,(1+54):(24+54)]+emp_GP_P1_posterior_samp[,2]+emp_GP_P1_posterior_samp[,5]+emp_GP_P1_posterior_samp[,(1+30):(24+30)]+emp_GP_P1_posterior_samp[,(1+102):(24+102)])-exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,3]+emp_GP_P1_posterior_samp[,(1+6):(24+6)]+emp_GP_P1_posterior_samp[,(1+54):(24+54)])
emp_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,4]+emp_GP_P1_posterior_samp[,(1+6):(24+6)]+emp_GP_P1_posterior_samp[,(1+78):(24+78)]+emp_GP_P1_posterior_samp[,2]+emp_GP_P1_posterior_samp[,6]+emp_GP_P1_posterior_samp[,(1+30):(24+30)]+emp_GP_P1_posterior_samp[,(1+126):(24+126)])-exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,4]+emp_GP_P1_posterior_samp[,(1+6):(24+6)]+emp_GP_P1_posterior_samp[,(1+78):(24+78)])
emp_GP_P1_posterior_samp[,(73+ncols):(96+ncols)] <- exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,(1+6):(24+6)])
emp_GP_P1_posterior_samp[,(97+ncols):(120+ncols)] <- exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,3]+emp_GP_P1_posterior_samp[,(1+6):(24+6)]+emp_GP_P1_posterior_samp[,(1+54):(24+54)])
emp_GP_P1_posterior_samp[,(121+ncols):(144+ncols)] <- exp(emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,4]+emp_GP_P1_posterior_samp[,(1+6):(24+6)]+emp_GP_P1_posterior_samp[,(1+78):(24+78)])
emp_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,(1+6):(24+6)]+emp_GP_P2_posterior_samp[,2]+emp_GP_P2_posterior_samp[,(1+30):(24+30)])-exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,(1+6):(24+6)])
emp_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,3]+emp_GP_P2_posterior_samp[,(1+6):(24+6)]+emp_GP_P2_posterior_samp[,(1+54):(24+54)]+emp_GP_P2_posterior_samp[,2]+emp_GP_P2_posterior_samp[,5]+emp_GP_P2_posterior_samp[,(1+30):(24+30)]+emp_GP_P2_posterior_samp[,(1+102):(24+102)])-exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,3]+emp_GP_P2_posterior_samp[,(1+6):(24+6)]+emp_GP_P2_posterior_samp[,(1+54):(24+54)])
emp_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,4]+emp_GP_P2_posterior_samp[,(1+6):(24+6)]+emp_GP_P2_posterior_samp[,(1+78):(24+78)]+emp_GP_P2_posterior_samp[,2]+emp_GP_P2_posterior_samp[,6]+emp_GP_P2_posterior_samp[,(1+30):(24+30)]+emp_GP_P2_posterior_samp[,(1+126):(24+126)])-exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,4]+emp_GP_P2_posterior_samp[,(1+6):(24+6)]+emp_GP_P2_posterior_samp[,(1+78):(24+78)])
emp_GP_P2_posterior_samp[,(73+ncols):(96+ncols)] <- exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,(1+6):(24+6)])
emp_GP_P2_posterior_samp[,(97+ncols):(120+ncols)] <- exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,3]+emp_GP_P2_posterior_samp[,(1+6):(24+6)]+emp_GP_P2_posterior_samp[,(1+54):(24+54)])
emp_GP_P2_posterior_samp[,(121+ncols):(144+ncols)] <- exp(emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,4]+emp_GP_P2_posterior_samp[,(1+6):(24+6)]+emp_GP_P2_posterior_samp[,(1+78):(24+78)])
lstm_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,(1+6):(24+6)]+lstm_GP_P0_posterior_samp[,2]+lstm_GP_P0_posterior_samp[,(1+30):(24+30)])-exp(lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,(1+6):(24+6)])
lstm_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,3]+lstm_GP_P0_posterior_samp[,(1+6):(24+6)]+lstm_GP_P0_posterior_samp[,(1+54):(24+54)]+lstm_GP_P0_posterior_samp[,2]+lstm_GP_P0_posterior_samp[,5]+lstm_GP_P0_posterior_samp[,(1+30):(24+30)]+lstm_GP_P0_posterior_samp[,(1+102):(24+102)])-exp(lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,3]+lstm_GP_P0_posterior_samp[,(1+6):(24+6)]+lstm_GP_P0_posterior_samp[,(1+54):(24+54)])
lstm_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- exp(lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,4]+lstm_GP_P0_posterior_samp[,(1+6):(24+6)]+lstm_GP_P0_posterior_samp[,(1+78):(24+78)]+lstm_GP_P0_posterior_samp[,2]+lstm_GP_P0_posterior_samp[,6]+lstm_GP_P0_posterior_samp[,(1+30):(24+30)]+lstm_GP_P0_posterior_samp[,(1+126):(24+126)])-exp(lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,4]+lstm_GP_P0_posterior_samp[,(1+6):(24+6)]+lstm_GP_P0_posterior_samp[,(1+78):(24+78)])
lstm_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,(1+6):(24+6)]+lstm_GP_P1_posterior_samp[,2]+lstm_GP_P1_posterior_samp[,(1+30):(24+30)])-exp(lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,(1+6):(24+6)])
lstm_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,3]+lstm_GP_P1_posterior_samp[,(1+6):(24+6)]+lstm_GP_P1_posterior_samp[,(1+54):(24+54)]+lstm_GP_P1_posterior_samp[,2]+lstm_GP_P1_posterior_samp[,5]+lstm_GP_P1_posterior_samp[,(1+30):(24+30)]+lstm_GP_P1_posterior_samp[,(1+102):(24+102)])-exp(lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,3]+lstm_GP_P1_posterior_samp[,(1+6):(24+6)]+lstm_GP_P1_posterior_samp[,(1+54):(24+54)])
lstm_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- exp(lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,4]+lstm_GP_P1_posterior_samp[,(1+6):(24+6)]+lstm_GP_P1_posterior_samp[,(1+78):(24+78)]+lstm_GP_P1_posterior_samp[,2]+lstm_GP_P1_posterior_samp[,6]+lstm_GP_P1_posterior_samp[,(1+30):(24+30)]+lstm_GP_P1_posterior_samp[,(1+126):(24+126)])-exp(lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,4]+lstm_GP_P1_posterior_samp[,(1+6):(24+6)]+lstm_GP_P1_posterior_samp[,(1+78):(24+78)])
lstm_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,(1+6):(24+6)]+lstm_GP_P2_posterior_samp[,2]+lstm_GP_P2_posterior_samp[,(1+30):(24+30)])-exp(lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,(1+6):(24+6)])
lstm_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,3]+lstm_GP_P2_posterior_samp[,(1+6):(24+6)]+lstm_GP_P2_posterior_samp[,(1+54):(24+54)]+lstm_GP_P2_posterior_samp[,2]+lstm_GP_P2_posterior_samp[,5]+lstm_GP_P2_posterior_samp[,(1+30):(24+30)]+lstm_GP_P2_posterior_samp[,(1+102):(24+102)])-exp(lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,3]+lstm_GP_P2_posterior_samp[,(1+6):(24+6)]+lstm_GP_P2_posterior_samp[,(1+54):(24+54)])
lstm_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- exp(lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,4]+lstm_GP_P2_posterior_samp[,(1+6):(24+6)]+lstm_GP_P2_posterior_samp[,(1+78):(24+78)]+lstm_GP_P2_posterior_samp[,2]+lstm_GP_P2_posterior_samp[,6]+lstm_GP_P2_posterior_samp[,(1+30):(24+30)]+lstm_GP_P2_posterior_samp[,(1+126):(24+126)])-exp(lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,4]+lstm_GP_P2_posterior_samp[,(1+6):(24+6)]+lstm_GP_P2_posterior_samp[,(1+78):(24+78)])
gpt2_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P0_posterior_samp[,2]+gpt2_GP_P0_posterior_samp[,(1+30):(24+30)])-exp(gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,(1+6):(24+6)])
gpt2_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,3]+gpt2_GP_P0_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P0_posterior_samp[,(1+54):(24+54)]+gpt2_GP_P0_posterior_samp[,2]+gpt2_GP_P0_posterior_samp[,5]+gpt2_GP_P0_posterior_samp[,(1+30):(24+30)]+gpt2_GP_P0_posterior_samp[,(1+102):(24+102)])-exp(gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,3]+gpt2_GP_P0_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P0_posterior_samp[,(1+54):(24+54)])
gpt2_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- exp(gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,4]+gpt2_GP_P0_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P0_posterior_samp[,(1+78):(24+78)]+gpt2_GP_P0_posterior_samp[,2]+gpt2_GP_P0_posterior_samp[,6]+gpt2_GP_P0_posterior_samp[,(1+30):(24+30)]+gpt2_GP_P0_posterior_samp[,(1+126):(24+126)])-exp(gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,4]+gpt2_GP_P0_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P0_posterior_samp[,(1+78):(24+78)])
gpt2_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P1_posterior_samp[,2]+gpt2_GP_P1_posterior_samp[,(1+30):(24+30)])-exp(gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,(1+6):(24+6)])
gpt2_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,3]+gpt2_GP_P1_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P1_posterior_samp[,(1+54):(24+54)]+gpt2_GP_P1_posterior_samp[,2]+gpt2_GP_P1_posterior_samp[,5]+gpt2_GP_P1_posterior_samp[,(1+30):(24+30)]+gpt2_GP_P1_posterior_samp[,(1+102):(24+102)])-exp(gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,3]+gpt2_GP_P1_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P1_posterior_samp[,(1+54):(24+54)])
gpt2_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- exp(gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,4]+gpt2_GP_P1_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P1_posterior_samp[,(1+78):(24+78)]+gpt2_GP_P1_posterior_samp[,2]+gpt2_GP_P1_posterior_samp[,6]+gpt2_GP_P1_posterior_samp[,(1+30):(24+30)]+gpt2_GP_P1_posterior_samp[,(1+126):(24+126)])-exp(gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,4]+gpt2_GP_P1_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P1_posterior_samp[,(1+78):(24+78)])
gpt2_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P2_posterior_samp[,2]+gpt2_GP_P2_posterior_samp[,(1+30):(24+30)])-exp(gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,(1+6):(24+6)])
gpt2_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,3]+gpt2_GP_P2_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P2_posterior_samp[,(1+54):(24+54)]+gpt2_GP_P2_posterior_samp[,2]+gpt2_GP_P2_posterior_samp[,5]+gpt2_GP_P2_posterior_samp[,(1+30):(24+30)]+gpt2_GP_P2_posterior_samp[,(1+102):(24+102)])-exp(gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,3]+gpt2_GP_P2_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P2_posterior_samp[,(1+54):(24+54)])
gpt2_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- exp(gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,4]+gpt2_GP_P2_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P2_posterior_samp[,(1+78):(24+78)]+gpt2_GP_P2_posterior_samp[,2]+gpt2_GP_P2_posterior_samp[,6]+gpt2_GP_P2_posterior_samp[,(1+30):(24+30)]+gpt2_GP_P2_posterior_samp[,(1+126):(24+126)])-exp(gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,4]+gpt2_GP_P2_posterior_samp[,(1+6):(24+6)]+gpt2_GP_P2_posterior_samp[,(1+78):(24+78)])
nosurp_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P0_posterior_samp[,2]+nosurp_GP_P0_posterior_samp[,(1+30):(24+30)])-exp(nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,(1+6):(24+6)])
nosurp_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,3]+nosurp_GP_P0_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P0_posterior_samp[,(1+54):(24+54)]+nosurp_GP_P0_posterior_samp[,2]+nosurp_GP_P0_posterior_samp[,5]+nosurp_GP_P0_posterior_samp[,(1+30):(24+30)]+nosurp_GP_P0_posterior_samp[,(1+102):(24+102)])-exp(nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,3]+nosurp_GP_P0_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P0_posterior_samp[,(1+54):(24+54)])
nosurp_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- exp(nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,4]+nosurp_GP_P0_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P0_posterior_samp[,(1+78):(24+78)]+nosurp_GP_P0_posterior_samp[,2]+nosurp_GP_P0_posterior_samp[,6]+nosurp_GP_P0_posterior_samp[,(1+30):(24+30)]+nosurp_GP_P0_posterior_samp[,(1+126):(24+126)])-exp(nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,4]+nosurp_GP_P0_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P0_posterior_samp[,(1+78):(24+78)])
nosurp_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P1_posterior_samp[,2]+nosurp_GP_P1_posterior_samp[,(1+30):(24+30)])-exp(nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,(1+6):(24+6)])
nosurp_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,3]+nosurp_GP_P1_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P1_posterior_samp[,(1+54):(24+54)]+nosurp_GP_P1_posterior_samp[,2]+nosurp_GP_P1_posterior_samp[,5]+nosurp_GP_P1_posterior_samp[,(1+30):(24+30)]+nosurp_GP_P1_posterior_samp[,(1+102):(24+102)])-exp(nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,3]+nosurp_GP_P1_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P1_posterior_samp[,(1+54):(24+54)])
nosurp_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- exp(nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,4]+nosurp_GP_P1_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P1_posterior_samp[,(1+78):(24+78)]+nosurp_GP_P1_posterior_samp[,2]+nosurp_GP_P1_posterior_samp[,6]+nosurp_GP_P1_posterior_samp[,(1+30):(24+30)]+nosurp_GP_P1_posterior_samp[,(1+126):(24+126)])-exp(nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,4]+nosurp_GP_P1_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P1_posterior_samp[,(1+78):(24+78)])
nosurp_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P2_posterior_samp[,2]+nosurp_GP_P2_posterior_samp[,(1+30):(24+30)])-exp(nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,(1+6):(24+6)])
nosurp_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,3]+nosurp_GP_P2_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P2_posterior_samp[,(1+54):(24+54)]+nosurp_GP_P2_posterior_samp[,2]+nosurp_GP_P2_posterior_samp[,5]+nosurp_GP_P2_posterior_samp[,(1+30):(24+30)]+nosurp_GP_P2_posterior_samp[,(1+102):(24+102)])-exp(nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,3]+nosurp_GP_P2_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P2_posterior_samp[,(1+54):(24+54)])
nosurp_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- exp(nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,4]+nosurp_GP_P2_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P2_posterior_samp[,(1+78):(24+78)]+nosurp_GP_P2_posterior_samp[,2]+nosurp_GP_P2_posterior_samp[,6]+nosurp_GP_P2_posterior_samp[,(1+30):(24+30)]+nosurp_GP_P2_posterior_samp[,(1+126):(24+126)])-exp(nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,4]+nosurp_GP_P2_posterior_samp[,(1+6):(24+6)]+nosurp_GP_P2_posterior_samp[,(1+78):(24+78)])

for(i in 1:24){
  posterior_emp_MVRR_P0[i,]$mean <- mean(emp_GP_P0_posterior_samp[,i+ncols])
  posterior_emp_MVRR_P0[i,]$SE <- sd(emp_GP_P0_posterior_samp[,i+ncols])
  posterior_emp_MVRR_P0[i,]$upper <- quantile(emp_GP_P0_posterior_samp[,i+ncols],0.975)
  posterior_emp_MVRR_P0[i,]$lower <- quantile(emp_GP_P0_posterior_samp[,i+ncols],0.025)
  posterior_emp_MVRR_P1[i,]$mean <- mean(emp_GP_P1_posterior_samp[,i+ncols])
  posterior_emp_MVRR_P1[i,]$SE <- sd(emp_GP_P1_posterior_samp[,i+ncols])
  posterior_emp_MVRR_P1[i,]$upper <- quantile(emp_GP_P1_posterior_samp[,i+ncols],0.975)
  posterior_emp_MVRR_P1[i,]$lower <- quantile(emp_GP_P1_posterior_samp[,i+ncols],0.025)
  posterior_emp_MVRR_P2[i,]$mean <- mean(emp_GP_P2_posterior_samp[,i+ncols])
  posterior_emp_MVRR_P2[i,]$SE <- sd(emp_GP_P2_posterior_samp[,i+ncols])
  posterior_emp_MVRR_P2[i,]$upper <- quantile(emp_GP_P2_posterior_samp[,i+ncols],0.975)
  posterior_emp_MVRR_P2[i,]$lower <- quantile(emp_GP_P2_posterior_samp[,i+ncols],0.025)
  posterior_emp_NPS_P0[i,]$mean <- mean(emp_GP_P0_posterior_samp[,i+ncols+24])
  posterior_emp_NPS_P0[i,]$SE <- sd(emp_GP_P0_posterior_samp[,i+ncols+24])
  posterior_emp_NPS_P0[i,]$upper <- quantile(emp_GP_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_NPS_P0[i,]$lower <- quantile(emp_GP_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_NPS_P1[i,]$mean <- mean(emp_GP_P1_posterior_samp[,i+ncols+24])
  posterior_emp_NPS_P1[i,]$SE <- sd(emp_GP_P1_posterior_samp[,i+ncols+24])
  posterior_emp_NPS_P1[i,]$upper <- quantile(emp_GP_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_NPS_P1[i,]$lower <- quantile(emp_GP_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_NPS_P2[i,]$mean <- mean(emp_GP_P2_posterior_samp[,i+ncols+24])
  posterior_emp_NPS_P2[i,]$SE <- sd(emp_GP_P2_posterior_samp[,i+ncols+24])
  posterior_emp_NPS_P2[i,]$upper <- quantile(emp_GP_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_NPS_P2[i,]$lower <- quantile(emp_GP_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_NPZ_P0[i,]$mean <- mean(emp_GP_P0_posterior_samp[,i+ncols+48])
  posterior_emp_NPZ_P0[i,]$SE <- sd(emp_GP_P0_posterior_samp[,i+ncols+48])
  posterior_emp_NPZ_P0[i,]$upper <- quantile(emp_GP_P0_posterior_samp[,i+ncols+48],0.975)
  posterior_emp_NPZ_P0[i,]$lower <- quantile(emp_GP_P0_posterior_samp[,i+ncols+48],0.025)
  posterior_emp_NPZ_P1[i,]$mean <- mean(emp_GP_P1_posterior_samp[,i+ncols+48])
  posterior_emp_NPZ_P1[i,]$SE <- sd(emp_GP_P1_posterior_samp[,i+ncols+48])
  posterior_emp_NPZ_P1[i,]$upper <- quantile(emp_GP_P1_posterior_samp[,i+ncols+48],0.975)
  posterior_emp_NPZ_P1[i,]$lower <- quantile(emp_GP_P1_posterior_samp[,i+ncols+48],0.025)
  posterior_emp_NPZ_P2[i,]$mean <- mean(emp_GP_P2_posterior_samp[,i+ncols+48])
  posterior_emp_NPZ_P2[i,]$SE <- sd(emp_GP_P2_posterior_samp[,i+ncols+48])
  posterior_emp_NPZ_P2[i,]$upper <- quantile(emp_GP_P2_posterior_samp[,i+ncols+48],0.975)
  posterior_emp_NPZ_P2[i,]$lower <- quantile(emp_GP_P2_posterior_samp[,i+ncols+48],0.025)
  posterior_emp_unambMVRR_P0[i,]$mean <- mean(emp_GP_P0_posterior_samp[,i+ncols+72])
  posterior_emp_unambMVRR_P0[i,]$SE <- sd(emp_GP_P0_posterior_samp[,i+ncols+72])
  posterior_emp_unambMVRR_P0[i,]$upper <- quantile(emp_GP_P0_posterior_samp[,i+ncols+72],0.975)
  posterior_emp_unambMVRR_P0[i,]$lower <- quantile(emp_GP_P0_posterior_samp[,i+ncols+72],0.025)
  posterior_emp_unambMVRR_P1[i,]$mean <- mean(emp_GP_P1_posterior_samp[,i+ncols+72])
  posterior_emp_unambMVRR_P1[i,]$SE <- sd(emp_GP_P1_posterior_samp[,i+ncols+72])
  posterior_emp_unambMVRR_P1[i,]$upper <- quantile(emp_GP_P1_posterior_samp[,i+ncols+72],0.975)
  posterior_emp_unambMVRR_P1[i,]$lower <- quantile(emp_GP_P1_posterior_samp[,i+ncols+72],0.025)
  posterior_emp_unambMVRR_P2[i,]$mean <- mean(emp_GP_P2_posterior_samp[,i+ncols+72])
  posterior_emp_unambMVRR_P2[i,]$SE <- sd(emp_GP_P2_posterior_samp[,i+ncols+72])
  posterior_emp_unambMVRR_P2[i,]$upper <- quantile(emp_GP_P2_posterior_samp[,i+ncols+72],0.975)
  posterior_emp_unambMVRR_P2[i,]$lower <- quantile(emp_GP_P2_posterior_samp[,i+ncols+72],0.025)
  posterior_emp_unambNPS_P0[i,]$mean <- mean(emp_GP_P0_posterior_samp[,i+ncols+96])
  posterior_emp_unambNPS_P0[i,]$SE <- sd(emp_GP_P0_posterior_samp[,i+ncols+96])
  posterior_emp_unambNPS_P0[i,]$upper <- quantile(emp_GP_P0_posterior_samp[,i+ncols+96],0.975)
  posterior_emp_unambNPS_P0[i,]$lower <- quantile(emp_GP_P0_posterior_samp[,i+ncols+96],0.025)
  posterior_emp_unambNPS_P1[i,]$mean <- mean(emp_GP_P1_posterior_samp[,i+ncols+96])
  posterior_emp_unambNPS_P1[i,]$SE <- sd(emp_GP_P1_posterior_samp[,i+ncols+96])
  posterior_emp_unambNPS_P1[i,]$upper <- quantile(emp_GP_P1_posterior_samp[,i+ncols+96],0.975)
  posterior_emp_unambNPS_P1[i,]$lower <- quantile(emp_GP_P1_posterior_samp[,i+ncols+96],0.025)
  posterior_emp_unambNPS_P2[i,]$mean <- mean(emp_GP_P2_posterior_samp[,i+ncols+96])
  posterior_emp_unambNPS_P2[i,]$SE <- sd(emp_GP_P2_posterior_samp[,i+ncols+96])
  posterior_emp_unambNPS_P2[i,]$upper <- quantile(emp_GP_P2_posterior_samp[,i+ncols+96],0.975)
  posterior_emp_unambNPS_P2[i,]$lower <- quantile(emp_GP_P2_posterior_samp[,i+ncols+96],0.025)
  posterior_emp_unambNPZ_P0[i,]$mean <- mean(emp_GP_P0_posterior_samp[,i+ncols+120])
  posterior_emp_unambNPZ_P0[i,]$SE <- sd(emp_GP_P0_posterior_samp[,i+ncols+120])
  posterior_emp_unambNPZ_P0[i,]$upper <- quantile(emp_GP_P0_posterior_samp[,i+ncols+120],0.975)
  posterior_emp_unambNPZ_P0[i,]$lower <- quantile(emp_GP_P0_posterior_samp[,i+ncols+120],0.025)
  posterior_emp_unambNPZ_P1[i,]$mean <- mean(emp_GP_P1_posterior_samp[,i+ncols+120])
  posterior_emp_unambNPZ_P1[i,]$SE <- sd(emp_GP_P1_posterior_samp[,i+ncols+120])
  posterior_emp_unambNPZ_P1[i,]$upper <- quantile(emp_GP_P1_posterior_samp[,i+ncols+120],0.975)
  posterior_emp_unambNPZ_P1[i,]$lower <- quantile(emp_GP_P1_posterior_samp[,i+ncols+120],0.025)
  posterior_emp_unambNPZ_P2[i,]$mean <- mean(emp_GP_P2_posterior_samp[,i+ncols+120])
  posterior_emp_unambNPZ_P2[i,]$SE <- sd(emp_GP_P2_posterior_samp[,i+ncols+120])
  posterior_emp_unambNPZ_P2[i,]$upper <- quantile(emp_GP_P2_posterior_samp[,i+ncols+120],0.975)
  posterior_emp_unambNPZ_P2[i,]$lower <- quantile(emp_GP_P2_posterior_samp[,i+ncols+120],0.025)
  posterior_lstm_MVRR_P0[i,]$mean <- mean(lstm_GP_P0_posterior_samp[,i+ncols])
  posterior_lstm_MVRR_P0[i,]$SE <- sd(lstm_GP_P0_posterior_samp[,i+ncols])
  posterior_lstm_MVRR_P0[i,]$upper <- quantile(lstm_GP_P0_posterior_samp[,i+ncols],0.975)
  posterior_lstm_MVRR_P0[i,]$lower <- quantile(lstm_GP_P0_posterior_samp[,i+ncols],0.025)
  posterior_lstm_MVRR_P1[i,]$mean <- mean(lstm_GP_P1_posterior_samp[,i+ncols])
  posterior_lstm_MVRR_P1[i,]$SE <- sd(lstm_GP_P1_posterior_samp[,i+ncols])
  posterior_lstm_MVRR_P1[i,]$upper <- quantile(lstm_GP_P1_posterior_samp[,i+ncols],0.975)
  posterior_lstm_MVRR_P1[i,]$lower <- quantile(lstm_GP_P1_posterior_samp[,i+ncols],0.025)
  posterior_lstm_MVRR_P2[i,]$mean <- mean(lstm_GP_P2_posterior_samp[,i+ncols])
  posterior_lstm_MVRR_P2[i,]$SE <- sd(lstm_GP_P2_posterior_samp[,i+ncols])
  posterior_lstm_MVRR_P2[i,]$upper <- quantile(lstm_GP_P2_posterior_samp[,i+ncols],0.975)
  posterior_lstm_MVRR_P2[i,]$lower <- quantile(lstm_GP_P2_posterior_samp[,i+ncols],0.025)
  posterior_lstm_NPS_P0[i,]$mean <- mean(lstm_GP_P0_posterior_samp[,i+ncols+24])
  posterior_lstm_NPS_P0[i,]$SE <- sd(lstm_GP_P0_posterior_samp[,i+ncols+24])
  posterior_lstm_NPS_P0[i,]$upper <- quantile(lstm_GP_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_lstm_NPS_P0[i,]$lower <- quantile(lstm_GP_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_lstm_NPS_P1[i,]$mean <- mean(lstm_GP_P1_posterior_samp[,i+ncols+24])
  posterior_lstm_NPS_P1[i,]$SE <- sd(lstm_GP_P1_posterior_samp[,i+ncols+24])
  posterior_lstm_NPS_P1[i,]$upper <- quantile(lstm_GP_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_lstm_NPS_P1[i,]$lower <- quantile(lstm_GP_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_lstm_NPS_P2[i,]$mean <- mean(lstm_GP_P2_posterior_samp[,i+ncols+24])
  posterior_lstm_NPS_P2[i,]$SE <- sd(lstm_GP_P2_posterior_samp[,i+ncols+24])
  posterior_lstm_NPS_P2[i,]$upper <- quantile(lstm_GP_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_lstm_NPS_P2[i,]$lower <- quantile(lstm_GP_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_lstm_NPZ_P0[i,]$mean <- mean(lstm_GP_P0_posterior_samp[,i+ncols+48])
  posterior_lstm_NPZ_P0[i,]$SE <- sd(lstm_GP_P0_posterior_samp[,i+ncols+48])
  posterior_lstm_NPZ_P0[i,]$upper <- quantile(lstm_GP_P0_posterior_samp[,i+ncols+48],0.975)
  posterior_lstm_NPZ_P0[i,]$lower <- quantile(lstm_GP_P0_posterior_samp[,i+ncols+48],0.025)
  posterior_lstm_NPZ_P1[i,]$mean <- mean(lstm_GP_P1_posterior_samp[,i+ncols+48])
  posterior_lstm_NPZ_P1[i,]$SE <- sd(lstm_GP_P1_posterior_samp[,i+ncols+48])
  posterior_lstm_NPZ_P1[i,]$upper <- quantile(lstm_GP_P1_posterior_samp[,i+ncols+48],0.975)
  posterior_lstm_NPZ_P1[i,]$lower <- quantile(lstm_GP_P1_posterior_samp[,i+ncols+48],0.025)
  posterior_lstm_NPZ_P2[i,]$mean <- mean(lstm_GP_P2_posterior_samp[,i+ncols+48])
  posterior_lstm_NPZ_P2[i,]$SE <- sd(lstm_GP_P2_posterior_samp[,i+ncols+48])
  posterior_lstm_NPZ_P2[i,]$upper <- quantile(lstm_GP_P2_posterior_samp[,i+ncols+48],0.975)
  posterior_lstm_NPZ_P2[i,]$lower <- quantile(lstm_GP_P2_posterior_samp[,i+ncols+48],0.025)
  posterior_gpt2_MVRR_P0[i,]$mean <- mean(gpt2_GP_P0_posterior_samp[,i+ncols])
  posterior_gpt2_MVRR_P0[i,]$SE <- sd(gpt2_GP_P0_posterior_samp[,i+ncols])
  posterior_gpt2_MVRR_P0[i,]$upper <- quantile(gpt2_GP_P0_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_MVRR_P0[i,]$lower <- quantile(gpt2_GP_P0_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_MVRR_P1[i,]$mean <- mean(gpt2_GP_P1_posterior_samp[,i+ncols])
  posterior_gpt2_MVRR_P1[i,]$SE <- sd(gpt2_GP_P1_posterior_samp[,i+ncols])
  posterior_gpt2_MVRR_P1[i,]$upper <- quantile(gpt2_GP_P1_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_MVRR_P1[i,]$lower <- quantile(gpt2_GP_P1_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_MVRR_P2[i,]$mean <- mean(gpt2_GP_P2_posterior_samp[,i+ncols])
  posterior_gpt2_MVRR_P2[i,]$SE <- sd(gpt2_GP_P2_posterior_samp[,i+ncols])
  posterior_gpt2_MVRR_P2[i,]$upper <- quantile(gpt2_GP_P2_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_MVRR_P2[i,]$lower <- quantile(gpt2_GP_P2_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_NPS_P0[i,]$mean <- mean(gpt2_GP_P0_posterior_samp[,i+ncols+24])
  posterior_gpt2_NPS_P0[i,]$SE <- sd(gpt2_GP_P0_posterior_samp[,i+ncols+24])
  posterior_gpt2_NPS_P0[i,]$upper <- quantile(gpt2_GP_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2_NPS_P0[i,]$lower <- quantile(gpt2_GP_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2_NPS_P1[i,]$mean <- mean(gpt2_GP_P1_posterior_samp[,i+ncols+24])
  posterior_gpt2_NPS_P1[i,]$SE <- sd(gpt2_GP_P1_posterior_samp[,i+ncols+24])
  posterior_gpt2_NPS_P1[i,]$upper <- quantile(gpt2_GP_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2_NPS_P1[i,]$lower <- quantile(gpt2_GP_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2_NPS_P2[i,]$mean <- mean(gpt2_GP_P2_posterior_samp[,i+ncols+24])
  posterior_gpt2_NPS_P2[i,]$SE <- sd(gpt2_GP_P2_posterior_samp[,i+ncols+24])
  posterior_gpt2_NPS_P2[i,]$upper <- quantile(gpt2_GP_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_gpt2_NPS_P2[i,]$lower <- quantile(gpt2_GP_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_gpt2_NPZ_P0[i,]$mean <- mean(gpt2_GP_P0_posterior_samp[,i+ncols+48])
  posterior_gpt2_NPZ_P0[i,]$SE <- sd(gpt2_GP_P0_posterior_samp[,i+ncols+48])
  posterior_gpt2_NPZ_P0[i,]$upper <- quantile(gpt2_GP_P0_posterior_samp[,i+ncols+48],0.975)
  posterior_gpt2_NPZ_P0[i,]$lower <- quantile(gpt2_GP_P0_posterior_samp[,i+ncols+48],0.025)
  posterior_gpt2_NPZ_P1[i,]$mean <- mean(gpt2_GP_P1_posterior_samp[,i+ncols+48])
  posterior_gpt2_NPZ_P1[i,]$SE <- sd(gpt2_GP_P1_posterior_samp[,i+ncols+48])
  posterior_gpt2_NPZ_P1[i,]$upper <- quantile(gpt2_GP_P1_posterior_samp[,i+ncols+48],0.975)
  posterior_gpt2_NPZ_P1[i,]$lower <- quantile(gpt2_GP_P1_posterior_samp[,i+ncols+48],0.025)
  posterior_gpt2_NPZ_P2[i,]$mean <- mean(gpt2_GP_P2_posterior_samp[,i+ncols+48])
  posterior_gpt2_NPZ_P2[i,]$SE <- sd(gpt2_GP_P2_posterior_samp[,i+ncols+48])
  posterior_gpt2_NPZ_P2[i,]$upper <- quantile(gpt2_GP_P2_posterior_samp[,i+ncols+48],0.975)
  posterior_gpt2_NPZ_P2[i,]$lower <- quantile(gpt2_GP_P2_posterior_samp[,i+ncols+48],0.025)
  posterior_nosurp_MVRR_P0[i,]$mean <- mean(nosurp_GP_P0_posterior_samp[,i+ncols])
  posterior_nosurp_MVRR_P0[i,]$SE <- sd(nosurp_GP_P0_posterior_samp[,i+ncols])
  posterior_nosurp_MVRR_P0[i,]$upper <- quantile(nosurp_GP_P0_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_MVRR_P0[i,]$lower <- quantile(nosurp_GP_P0_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_MVRR_P1[i,]$mean <- mean(nosurp_GP_P1_posterior_samp[,i+ncols])
  posterior_nosurp_MVRR_P1[i,]$SE <- sd(nosurp_GP_P1_posterior_samp[,i+ncols])
  posterior_nosurp_MVRR_P1[i,]$upper <- quantile(nosurp_GP_P1_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_MVRR_P1[i,]$lower <- quantile(nosurp_GP_P1_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_MVRR_P2[i,]$mean <- mean(nosurp_GP_P2_posterior_samp[,i+ncols])
  posterior_nosurp_MVRR_P2[i,]$SE <- sd(nosurp_GP_P2_posterior_samp[,i+ncols])
  posterior_nosurp_MVRR_P2[i,]$upper <- quantile(nosurp_GP_P2_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_MVRR_P2[i,]$lower <- quantile(nosurp_GP_P2_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_NPS_P0[i,]$mean <- mean(nosurp_GP_P0_posterior_samp[,i+ncols+24])
  posterior_nosurp_NPS_P0[i,]$SE <- sd(nosurp_GP_P0_posterior_samp[,i+ncols+24])
  posterior_nosurp_NPS_P0[i,]$upper <- quantile(nosurp_GP_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_nosurp_NPS_P0[i,]$lower <- quantile(nosurp_GP_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_nosurp_NPS_P1[i,]$mean <- mean(nosurp_GP_P1_posterior_samp[,i+ncols+24])
  posterior_nosurp_NPS_P1[i,]$SE <- sd(nosurp_GP_P1_posterior_samp[,i+ncols+24])
  posterior_nosurp_NPS_P1[i,]$upper <- quantile(nosurp_GP_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_nosurp_NPS_P1[i,]$lower <- quantile(nosurp_GP_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_nosurp_NPS_P2[i,]$mean <- mean(nosurp_GP_P2_posterior_samp[,i+ncols+24])
  posterior_nosurp_NPS_P2[i,]$SE <- sd(nosurp_GP_P2_posterior_samp[,i+ncols+24])
  posterior_nosurp_NPS_P2[i,]$upper <- quantile(nosurp_GP_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_nosurp_NPS_P2[i,]$lower <- quantile(nosurp_GP_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_nosurp_NPZ_P0[i,]$mean <- mean(nosurp_GP_P0_posterior_samp[,i+ncols+48])
  posterior_nosurp_NPZ_P0[i,]$SE <- sd(nosurp_GP_P0_posterior_samp[,i+ncols+48])
  posterior_nosurp_NPZ_P0[i,]$upper <- quantile(nosurp_GP_P0_posterior_samp[,i+ncols+48],0.975)
  posterior_nosurp_NPZ_P0[i,]$lower <- quantile(nosurp_GP_P0_posterior_samp[,i+ncols+48],0.025)
  posterior_nosurp_NPZ_P1[i,]$mean <- mean(nosurp_GP_P1_posterior_samp[,i+ncols+48])
  posterior_nosurp_NPZ_P1[i,]$SE <- sd(nosurp_GP_P1_posterior_samp[,i+ncols+48])
  posterior_nosurp_NPZ_P1[i,]$upper <- quantile(nosurp_GP_P1_posterior_samp[,i+ncols+48],0.975)
  posterior_nosurp_NPZ_P1[i,]$lower <- quantile(nosurp_GP_P1_posterior_samp[,i+ncols+48],0.025)
  posterior_nosurp_NPZ_P2[i,]$mean <- mean(nosurp_GP_P2_posterior_samp[,i+ncols+48])
  posterior_nosurp_NPZ_P2[i,]$SE <- sd(nosurp_GP_P2_posterior_samp[,i+ncols+48])
  posterior_nosurp_NPZ_P2[i,]$upper <- quantile(nosurp_GP_P2_posterior_samp[,i+ncols+48],0.975)
  posterior_nosurp_NPZ_P2[i,]$lower <- quantile(nosurp_GP_P2_posterior_samp[,i+ncols+48],0.025)
}


#P0
sampled_correlations_P0 <- data.frame(Correlation=rep(NA,9000),EOI=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),3000),model=rep(c("lstm","lstm","lstm","gpt2","gpt2","gpt2","nosurp","nosurp","nosurp"),1000),ROI=0)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,72),lstm=rep(NA,72),gpt2=rep(NA,72),nosurp=rep(NA,72),EOI=c(rep("GPE_MVRR",24),rep("GPE_NPS",24),rep("GPE_NPZ",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_GP_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_GP_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_GP_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_GP_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_GP_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_GP_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_GP_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_GP_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+48,1] <- sample(emp_GP_P0_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,2] <- sample(lstm_GP_P0_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,3] <- sample(gpt2_GP_P0_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,4] <- sample(nosurp_GP_P0_posterior_samp[,j+48+ncols],1)
  }
  sampled_correlations_P0[((i-1)*9+1):((i-1)*9+9),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate,
                                                            cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,4])$estimate)
}
#P0

#P1
sampled_correlations_P1 <- data.frame(Correlation=rep(NA,9000),EOI=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),3000),model=rep(c("lstm","lstm","lstm","gpt2","gpt2","gpt2","nosurp","nosurp","nosurp"),1000),ROI=1)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,72),lstm=rep(NA,72),gpt2=rep(NA,72),nosurp=rep(NA,72),EOI=c(rep("GPE_MVRR",24),rep("GPE_NPS",24),rep("GPE_NPZ",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_GP_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_GP_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_GP_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_GP_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_GP_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_GP_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_GP_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_GP_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+48,1] <- sample(emp_GP_P1_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,2] <- sample(lstm_GP_P1_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,3] <- sample(gpt2_GP_P1_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,4] <- sample(nosurp_GP_P1_posterior_samp[,j+48+ncols],1)
  }
  sampled_correlations_P1[((i-1)*9+1):((i-1)*9+9),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,2])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,3])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,4])$estimate)
}
#P1



#P2
sampled_correlations_P2 <- data.frame(Correlation=rep(NA,9000),EOI=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),3000),model=rep(c("lstm","lstm","lstm","gpt2","gpt2","gpt2","nosurp","nosurp","nosurp"),1000),ROI=2)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,72),lstm=rep(NA,72),gpt2=rep(NA,72),nosurp=rep(NA,72),EOI=c(rep("GPE_MVRR",24),rep("GPE_NPS",24),rep("GPE_NPZ",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_GP_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_GP_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_GP_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_GP_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_GP_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_GP_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_GP_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_GP_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+48,1] <- sample(emp_GP_P2_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,2] <- sample(lstm_GP_P2_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,3] <- sample(gpt2_GP_P2_posterior_samp[,j+48+ncols],1)
    posterior_onesampleeachitem[j+48,4] <- sample(nosurp_GP_P2_posterior_samp[,j+48+ncols],1)
  }
  sampled_correlations_P2[((i-1)*9+1):((i-1)*9+9),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,2])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,3])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate,
                                                                         cor.test(posterior_onesampleeachitem[49:72,1],posterior_onesampleeachitem[49:72,4])$estimate)
}
#P2




sampled_correlations_maxregion <- sampled_correlations_P1
for(i in unique(sampled_correlations_maxregion$EOI)){
  for(j in unique(sampled_correlations_maxregion$model)){
    hist(sampled_correlations_maxregion$Correlation[sampled_correlations_maxregion$EOI==i&sampled_correlations_maxregion$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_maxregion,"GP_logadd_prior1/sampled_correlations_P1_GP.rds")


for(i in unique(sampled_correlations_P2$EOI)){
  for(j in unique(sampled_correlations_P2$model)){
    hist(sampled_correlations_P2$Correlation[sampled_correlations_P2$EOI==i&sampled_correlations_P2$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P2,"GP_logadd_prior1/sampled_correlations_P2_GP.rds")

for(i in unique(sampled_correlations_P0$EOI)){
  for(j in unique(sampled_correlations_P0$model)){
    hist(sampled_correlations_P0$Correlation[sampled_correlations_P0$EOI==i&sampled_correlations_P0$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P0,"GP_logadd_prior1/sampled_correlations_P0_GP.rds")



