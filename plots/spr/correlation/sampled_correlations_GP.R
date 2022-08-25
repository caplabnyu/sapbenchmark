library(brms)
library(dplyr)
library(ggplot2)

#emp_GP_P0 <- readRDS("ClassicGP/prior1_fit_P0.rds")
#emp_GP_P1 <- readRDS("ClassicGP/prior1_fit_P1.rds")
#emp_GP_P2 <- readRDS("ClassicGP/prior1_fit_P2.rds")
#lstm_GP_P0 <- readRDS("ClassicGP/brm_predicted_lstm_GP_P0.rds")
#lstm_GP_P1 <- readRDS("ClassicGP/brm_predicted_lstm_GP_P1.rds")
#lstm_GP_P2 <- readRDS("ClassicGP/brm_predicted_lstm_GP_P2.rds")
#gpt2_GP_P0 <- readRDS("ClassicGP/brm_predicted_gpt2_GP_P0.rds")
#gpt2_GP_P1 <- readRDS("ClassicGP/brm_predicted_gpt2_GP_P1.rds")
#gpt2_GP_P2 <- readRDS("ClassicGP/brm_predicted_gpt2_GP_P2.rds")
#nosurp_GP_P0 <- readRDS("ClassicGP/brm_predicted_GP_P0_nosurp.rds")
#nosurp_GP_P1 <- readRDS("ClassicGP/brm_predicted_GP_P1_nosurp.rds")
#nosurp_GP_P2 <- readRDS("ClassicGP/brm_predicted_GP_P2_nosurp.rds")





#posterior_samp <- posterior_samples(emp_GP_P0)
#randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(AMBUAMB|SZM)',colnames(posterior_samp))]
#saveRDS(randomslope_names,"ClassicGP/GP_randomslopesnames.rds")
randomslope_names <- readRDS("ClassicGP/GP_randomslopesnames.rds")

emp_GP_P0_posterior_samp <- posterior_samples(emp_GP_P0, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
lstm_GP_P0_posterior_samp <- posterior_samples(lstm_GP_P0, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
gpt2_GP_P0_posterior_samp <- posterior_samples(gpt2_GP_P0, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
nosurp_GP_P0_posterior_samp <- posterior_samples(nosurp_GP_P0, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
emp_GP_P1_posterior_samp <- posterior_samples(emp_GP_P1, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
lstm_GP_P1_posterior_samp <- posterior_samples(lstm_GP_P1, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
gpt2_GP_P1_posterior_samp <- posterior_samples(gpt2_GP_P1, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
nosurp_GP_P1_posterior_samp <- posterior_samples(nosurp_GP_P1, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
emp_GP_P2_posterior_samp <- posterior_samples(emp_GP_P2, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
lstm_GP_P2_posterior_samp <- posterior_samples(lstm_GP_P2, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
gpt2_GP_P2_posterior_samp <- posterior_samples(gpt2_GP_P2, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))
nosurp_GP_P2_posterior_samp <- posterior_samples(nosurp_GP_P2, fixed=TRUE, pars=
                                                c("b_AMBUAMB","b_SZM1","b_SZM2","b_AMBUAMB:SZM1","b_AMBUAMB:SZM2",randomslope_names))

rm(emp_GP_P0, lstm_GP_P0, gpt2_GP_P0, nosurp_GP_P0, emp_GP_P1, lstm_GP_P1,
gpt2_GP_P1, nosurp_GP_P1, emp_GP_P2, lstm_GP_P2, gpt2_GP_P2, nosurp_GP_P2,posterior_samp)
#saveRDS(emp_GP_P0_posterior_samp,"ClassicGP/emp_GP_P0_posterior_samp.rds")
#saveRDS(lstm_GP_P0_posterior_samp,"ClassicGP/lstm_GP_P0_posterior_samp.rds")
#saveRDS(gpt2_GP_P0_posterior_samp,"ClassicGP/gpt2_GP_P0_posterior_samp.rds")
#saveRDS(nosurp_GP_P0_posterior_samp,"ClassicGP/nosurp_GP_P0_posterior_samp.rds")
#saveRDS(emp_GP_P1_posterior_samp,"ClassicGP/emp_GP_P1_posterior_samp.rds")
#saveRDS(lstm_GP_P1_posterior_samp,"ClassicGP/lstm_GP_P1_posterior_samp.rds")
#saveRDS(gpt2_GP_P1_posterior_samp,"ClassicGP/gpt2_GP_P1_posterior_samp.rds")
#saveRDS(nosurp_GP_P1_posterior_samp,"ClassicGP/nosurp_GP_P1_posterior_samp.rds")
#saveRDS(emp_GP_P2_posterior_samp,"ClassicGP/emp_GP_P2_posterior_samp.rds")
#saveRDS(lstm_GP_P2_posterior_samp,"ClassicGP/lstm_GP_P2_posterior_samp.rds")
#saveRDS(gpt2_GP_P2_posterior_samp,"ClassicGP/gpt2_GP_P2_posterior_samp.rds")
#saveRDS(nosurp_GP_P2_posterior_samp,"ClassicGP/nosurp_GP_P2_posterior_samp.rds")

emp_GP_P0_posterior_samp <- readRDS("ClassicGP/emp_GP_P0_posterior_samp.rds")
lstm_GP_P0_posterior_samp <- readRDS("ClassicGP/lstm_GP_P0_posterior_samp.rds")
gpt2_GP_P0_posterior_samp <- readRDS("ClassicGP/gpt2_GP_P0_posterior_samp.rds")
nosurp_GP_P0_posterior_samp <- readRDS("ClassicGP/nosurp_GP_P0_posterior_samp.rds")
emp_GP_P1_posterior_samp <- readRDS("ClassicGP/emp_GP_P1_posterior_samp.rds")
lstm_GP_P1_posterior_samp <- readRDS("ClassicGP/lstm_GP_P1_posterior_samp.rds")
gpt2_GP_P1_posterior_samp <- readRDS("ClassicGP/gpt2_GP_P1_posterior_samp.rds")
nosurp_GP_P1_posterior_samp <- readRDS("ClassicGP/nosurp_GP_P1_posterior_samp.rds")
emp_GP_P2_posterior_samp <- readRDS("ClassicGP/emp_GP_P2_posterior_samp.rds")
lstm_GP_P2_posterior_samp <- readRDS("ClassicGP/lstm_GP_P2_posterior_samp.rds")
gpt2_GP_P2_posterior_samp <- readRDS("ClassicGP/gpt2_GP_P2_posterior_samp.rds")
nosurp_GP_P2_posterior_samp <- readRDS("ClassicGP/nosurp_GP_P2_posterior_samp.rds")




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



ncols <- ncol(emp_GP_P0_posterior_samp)
emp_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,(1+5):(24+5)]
emp_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,4]+emp_GP_P0_posterior_samp[,(1+5):(24+5)]+emp_GP_P0_posterior_samp[,(1+77):(24+77)]
emp_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- emp_GP_P0_posterior_samp[,1]+emp_GP_P0_posterior_samp[,5]+emp_GP_P0_posterior_samp[,(1+5):(24+5)]+emp_GP_P0_posterior_samp[,(1+101):(24+101)]
emp_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,(1+5):(24+5)]
emp_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,4]+emp_GP_P1_posterior_samp[,(1+5):(24+5)]+emp_GP_P1_posterior_samp[,(1+77):(24+77)]
emp_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- emp_GP_P1_posterior_samp[,1]+emp_GP_P1_posterior_samp[,5]+emp_GP_P1_posterior_samp[,(1+5):(24+5)]+emp_GP_P1_posterior_samp[,(1+101):(24+101)]
emp_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,(1+5):(24+5)]
emp_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,4]+emp_GP_P2_posterior_samp[,(1+5):(24+5)]+emp_GP_P2_posterior_samp[,(1+77):(24+77)]
emp_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- emp_GP_P2_posterior_samp[,1]+emp_GP_P2_posterior_samp[,5]+emp_GP_P2_posterior_samp[,(1+5):(24+5)]+emp_GP_P2_posterior_samp[,(1+101):(24+101)]
lstm_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,(1+5):(24+5)]
lstm_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,4]+lstm_GP_P0_posterior_samp[,(1+5):(24+5)]+lstm_GP_P0_posterior_samp[,(1+77):(24+77)]
lstm_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- lstm_GP_P0_posterior_samp[,1]+lstm_GP_P0_posterior_samp[,5]+lstm_GP_P0_posterior_samp[,(1+5):(24+5)]+lstm_GP_P0_posterior_samp[,(1+101):(24+101)]
lstm_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,(1+5):(24+5)]
lstm_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,4]+lstm_GP_P1_posterior_samp[,(1+5):(24+5)]+lstm_GP_P1_posterior_samp[,(1+77):(24+77)]
lstm_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- lstm_GP_P1_posterior_samp[,1]+lstm_GP_P1_posterior_samp[,5]+lstm_GP_P1_posterior_samp[,(1+5):(24+5)]+lstm_GP_P1_posterior_samp[,(1+101):(24+101)]
lstm_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,(1+5):(24+5)]
lstm_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,4]+lstm_GP_P2_posterior_samp[,(1+5):(24+5)]+lstm_GP_P2_posterior_samp[,(1+77):(24+77)]
lstm_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- lstm_GP_P2_posterior_samp[,1]+lstm_GP_P2_posterior_samp[,5]+lstm_GP_P2_posterior_samp[,(1+5):(24+5)]+lstm_GP_P2_posterior_samp[,(1+101):(24+101)]
gpt2_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,(1+5):(24+5)]
gpt2_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,4]+gpt2_GP_P0_posterior_samp[,(1+5):(24+5)]+gpt2_GP_P0_posterior_samp[,(1+77):(24+77)]
gpt2_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- gpt2_GP_P0_posterior_samp[,1]+gpt2_GP_P0_posterior_samp[,5]+gpt2_GP_P0_posterior_samp[,(1+5):(24+5)]+gpt2_GP_P0_posterior_samp[,(1+101):(24+101)]
gpt2_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,(1+5):(24+5)]
gpt2_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,4]+gpt2_GP_P1_posterior_samp[,(1+5):(24+5)]+gpt2_GP_P1_posterior_samp[,(1+77):(24+77)]
gpt2_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- gpt2_GP_P1_posterior_samp[,1]+gpt2_GP_P1_posterior_samp[,5]+gpt2_GP_P1_posterior_samp[,(1+5):(24+5)]+gpt2_GP_P1_posterior_samp[,(1+101):(24+101)]
gpt2_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,(1+5):(24+5)]
gpt2_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,4]+gpt2_GP_P2_posterior_samp[,(1+5):(24+5)]+gpt2_GP_P2_posterior_samp[,(1+77):(24+77)]
gpt2_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- gpt2_GP_P2_posterior_samp[,1]+gpt2_GP_P2_posterior_samp[,5]+gpt2_GP_P2_posterior_samp[,(1+5):(24+5)]+gpt2_GP_P2_posterior_samp[,(1+101):(24+101)]
nosurp_GP_P0_posterior_samp[,(1+ncols):(24+ncols)] <- nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,(1+5):(24+5)]
nosurp_GP_P0_posterior_samp[,(25+ncols):(48+ncols)] <- nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,4]+nosurp_GP_P0_posterior_samp[,(1+5):(24+5)]+nosurp_GP_P0_posterior_samp[,(1+77):(24+77)]
nosurp_GP_P0_posterior_samp[,(49+ncols):(72+ncols)] <- nosurp_GP_P0_posterior_samp[,1]+nosurp_GP_P0_posterior_samp[,5]+nosurp_GP_P0_posterior_samp[,(1+5):(24+5)]+nosurp_GP_P0_posterior_samp[,(1+101):(24+101)]
nosurp_GP_P1_posterior_samp[,(1+ncols):(24+ncols)] <- nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,(1+5):(24+5)]
nosurp_GP_P1_posterior_samp[,(25+ncols):(48+ncols)] <- nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,4]+nosurp_GP_P1_posterior_samp[,(1+5):(24+5)]+nosurp_GP_P1_posterior_samp[,(1+77):(24+77)]
nosurp_GP_P1_posterior_samp[,(49+ncols):(72+ncols)] <- nosurp_GP_P1_posterior_samp[,1]+nosurp_GP_P1_posterior_samp[,5]+nosurp_GP_P1_posterior_samp[,(1+5):(24+5)]+nosurp_GP_P1_posterior_samp[,(1+101):(24+101)]
nosurp_GP_P2_posterior_samp[,(1+ncols):(24+ncols)] <- nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,(1+5):(24+5)]
nosurp_GP_P2_posterior_samp[,(25+ncols):(48+ncols)] <- nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,4]+nosurp_GP_P2_posterior_samp[,(1+5):(24+5)]+nosurp_GP_P2_posterior_samp[,(1+77):(24+77)]
nosurp_GP_P2_posterior_samp[,(49+ncols):(72+ncols)] <- nosurp_GP_P2_posterior_samp[,1]+nosurp_GP_P2_posterior_samp[,5]+nosurp_GP_P2_posterior_samp[,(1+5):(24+5)]+nosurp_GP_P2_posterior_samp[,(1+101):(24+101)]


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

by_item <- data.frame(item=c(posterior_emp_MVRR_P0$item,posterior_emp_MVRR_P1$item,posterior_emp_MVRR_P2$item,posterior_emp_NPS_P0$item,posterior_emp_NPS_P1$item,posterior_emp_NPS_P2$item,posterior_emp_NPZ_P0$item,posterior_emp_NPZ_P1$item,posterior_emp_NPZ_P2$item),
                      ROI=c(posterior_emp_MVRR_P0$ROI,posterior_emp_MVRR_P1$ROI,posterior_emp_MVRR_P2$ROI,posterior_emp_NPS_P0$ROI,posterior_emp_NPS_P1$ROI,posterior_emp_NPS_P2$ROI,posterior_emp_NPZ_P0$ROI,posterior_emp_NPZ_P1$ROI,posterior_emp_NPZ_P2$ROI),
                      coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=72),
                      mean=c(posterior_emp_MVRR_P0$mean,posterior_emp_MVRR_P1$mean,posterior_emp_MVRR_P2$mean,posterior_emp_NPS_P0$mean,posterior_emp_NPS_P1$mean,posterior_emp_NPS_P2$mean,posterior_emp_NPZ_P0$mean,posterior_emp_NPZ_P1$mean,posterior_emp_NPZ_P2$mean),
                      lower=c(posterior_emp_MVRR_P0$lower,posterior_emp_MVRR_P1$lower,posterior_emp_MVRR_P2$lower,posterior_emp_NPS_P0$lower,posterior_emp_NPS_P1$lower,posterior_emp_NPS_P2$lower,posterior_emp_NPZ_P0$lower,posterior_emp_NPZ_P1$lower,posterior_emp_NPZ_P2$lower),
                      upper=c(posterior_emp_MVRR_P0$upper,posterior_emp_MVRR_P1$upper,posterior_emp_MVRR_P2$upper,posterior_emp_NPS_P0$upper,posterior_emp_NPS_P1$upper,posterior_emp_NPS_P2$upper,posterior_emp_NPZ_P0$upper,posterior_emp_NPZ_P1$upper,posterior_emp_NPZ_P2$upper))
by_item_lstm <- data.frame(item=c(posterior_lstm_MVRR_P0$item,posterior_lstm_MVRR_P1$item,posterior_lstm_MVRR_P2$item,posterior_lstm_NPS_P0$item,posterior_lstm_NPS_P1$item,posterior_lstm_NPS_P2$item,posterior_lstm_NPZ_P0$item,posterior_lstm_NPZ_P1$item,posterior_lstm_NPZ_P2$item),
                           ROI=c(posterior_lstm_MVRR_P0$ROI,posterior_lstm_MVRR_P1$ROI,posterior_lstm_MVRR_P2$ROI,posterior_lstm_NPS_P0$ROI,posterior_lstm_NPS_P1$ROI,posterior_lstm_NPS_P2$ROI,posterior_lstm_NPZ_P0$ROI,posterior_lstm_NPZ_P1$ROI,posterior_lstm_NPZ_P2$ROI),
                           coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=72),
                           mean=c(posterior_lstm_MVRR_P0$mean,posterior_lstm_MVRR_P1$mean,posterior_lstm_MVRR_P2$mean,posterior_lstm_NPS_P0$mean,posterior_lstm_NPS_P1$mean,posterior_lstm_NPS_P2$mean,posterior_lstm_NPZ_P0$mean,posterior_lstm_NPZ_P1$mean,posterior_lstm_NPZ_P2$mean),
                           lower=c(posterior_lstm_MVRR_P0$lower,posterior_lstm_MVRR_P1$lower,posterior_lstm_MVRR_P2$lower,posterior_lstm_NPS_P0$lower,posterior_lstm_NPS_P1$lower,posterior_lstm_NPS_P2$lower,posterior_lstm_NPZ_P0$lower,posterior_lstm_NPZ_P1$lower,posterior_lstm_NPZ_P2$lower),
                           upper=c(posterior_lstm_MVRR_P0$upper,posterior_lstm_MVRR_P1$upper,posterior_lstm_MVRR_P2$upper,posterior_lstm_NPS_P0$upper,posterior_lstm_NPS_P1$upper,posterior_lstm_NPS_P2$upper,posterior_lstm_NPZ_P0$upper,posterior_lstm_NPZ_P1$upper,posterior_lstm_NPZ_P2$upper))
by_item_gpt2 <- data.frame(item=c(posterior_gpt2_MVRR_P0$item,posterior_gpt2_MVRR_P1$item,posterior_gpt2_MVRR_P2$item,posterior_gpt2_NPS_P0$item,posterior_gpt2_NPS_P1$item,posterior_gpt2_NPS_P2$item,posterior_gpt2_NPZ_P0$item,posterior_gpt2_NPZ_P1$item,posterior_gpt2_NPZ_P2$item),
                           ROI=c(posterior_gpt2_MVRR_P0$ROI,posterior_gpt2_MVRR_P1$ROI,posterior_gpt2_MVRR_P2$ROI,posterior_gpt2_NPS_P0$ROI,posterior_gpt2_NPS_P1$ROI,posterior_gpt2_NPS_P2$ROI,posterior_gpt2_NPZ_P0$ROI,posterior_gpt2_NPZ_P1$ROI,posterior_gpt2_NPZ_P2$ROI),
                           coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=72),
                           mean=c(posterior_gpt2_MVRR_P0$mean,posterior_gpt2_MVRR_P1$mean,posterior_gpt2_MVRR_P2$mean,posterior_gpt2_NPS_P0$mean,posterior_gpt2_NPS_P1$mean,posterior_gpt2_NPS_P2$mean,posterior_gpt2_NPZ_P0$mean,posterior_gpt2_NPZ_P1$mean,posterior_gpt2_NPZ_P2$mean),
                           lower=c(posterior_gpt2_MVRR_P0$lower,posterior_gpt2_MVRR_P1$lower,posterior_gpt2_MVRR_P2$lower,posterior_gpt2_NPS_P0$lower,posterior_gpt2_NPS_P1$lower,posterior_gpt2_NPS_P2$lower,posterior_gpt2_NPZ_P0$lower,posterior_gpt2_NPZ_P1$lower,posterior_gpt2_NPZ_P2$lower),
                           upper=c(posterior_gpt2_MVRR_P0$upper,posterior_gpt2_MVRR_P1$upper,posterior_gpt2_MVRR_P2$upper,posterior_gpt2_NPS_P0$upper,posterior_gpt2_NPS_P1$upper,posterior_gpt2_NPS_P2$upper,posterior_gpt2_NPZ_P0$upper,posterior_gpt2_NPZ_P1$upper,posterior_gpt2_NPZ_P2$upper))
by_item_nosurp <- data.frame(item=c(posterior_nosurp_MVRR_P0$item,posterior_nosurp_MVRR_P1$item,posterior_nosurp_MVRR_P2$item,posterior_nosurp_NPS_P0$item,posterior_nosurp_NPS_P1$item,posterior_nosurp_NPS_P2$item,posterior_nosurp_NPZ_P0$item,posterior_nosurp_NPZ_P1$item,posterior_nosurp_NPZ_P2$item),
                             ROI=c(posterior_nosurp_MVRR_P0$ROI,posterior_nosurp_MVRR_P1$ROI,posterior_nosurp_MVRR_P2$ROI,posterior_nosurp_NPS_P0$ROI,posterior_nosurp_NPS_P1$ROI,posterior_nosurp_NPS_P2$ROI,posterior_nosurp_NPZ_P0$ROI,posterior_nosurp_NPZ_P1$ROI,posterior_nosurp_NPZ_P2$ROI),
                             coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=72),
                             mean=c(posterior_nosurp_MVRR_P0$mean,posterior_nosurp_MVRR_P1$mean,posterior_nosurp_MVRR_P2$mean,posterior_nosurp_NPS_P0$mean,posterior_nosurp_NPS_P1$mean,posterior_nosurp_NPS_P2$mean,posterior_nosurp_NPZ_P0$mean,posterior_nosurp_NPZ_P1$mean,posterior_nosurp_NPZ_P2$mean),
                             lower=c(posterior_nosurp_MVRR_P0$lower,posterior_nosurp_MVRR_P1$lower,posterior_nosurp_MVRR_P2$lower,posterior_nosurp_NPS_P0$lower,posterior_nosurp_NPS_P1$lower,posterior_nosurp_NPS_P2$lower,posterior_nosurp_NPZ_P0$lower,posterior_nosurp_NPZ_P1$lower,posterior_nosurp_NPZ_P2$lower),
                             upper=c(posterior_nosurp_MVRR_P0$upper,posterior_nosurp_MVRR_P1$upper,posterior_nosurp_MVRR_P2$upper,posterior_nosurp_NPS_P0$upper,posterior_nosurp_NPS_P1$upper,posterior_nosurp_NPS_P2$upper,posterior_nosurp_NPZ_P0$upper,posterior_nosurp_NPZ_P1$upper,posterior_nosurp_NPZ_P2$upper))
by_item <- arrange(by_item,item,ROI)
by_item_lstm <- arrange(by_item_lstm,item,ROI)
by_item_gpt2 <- arrange(by_item_gpt2,item,ROI)
by_item_nosurp <- arrange(by_item_nosurp,item,ROI)
saveRDS(by_item,"ClassicGP/by_item.rds")
saveRDS(by_item_lstm,"ClassicGP/by_item_lstm.rds")
saveRDS(by_item_gpt2,"ClassicGP/by_item_gpt2.rds")
saveRDS(by_item_nosurp,"ClassicGP/by_item_nosurp.rds")

by_construction <- data.frame(ROI=rep(c(0,1,2),3),coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=3),mean=c(mean(emp_GP_P0_posterior_samp$b_AMBUAMB),mean(emp_GP_P1_posterior_samp$b_AMBUAMB),mean(emp_GP_P2_posterior_samp$b_AMBUAMB),mean(emp_GP_P0_posterior_samp$b_AMBUAMB+emp_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`),mean(emp_GP_P1_posterior_samp$b_AMBUAMB+emp_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`),mean(emp_GP_P2_posterior_samp$b_AMBUAMB+emp_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`),mean(emp_GP_P0_posterior_samp$b_AMBUAMB+emp_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`),mean(emp_GP_P1_posterior_samp$b_AMBUAMB+emp_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`),mean(emp_GP_P2_posterior_samp$b_AMBUAMB+emp_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`)),
                              lower=c(quantile(emp_GP_P0_posterior_samp$b_AMBUAMB,0.025),quantile(emp_GP_P1_posterior_samp$b_AMBUAMB,0.025),quantile(emp_GP_P2_posterior_samp$b_AMBUAMB,0.025),quantile(emp_GP_P0_posterior_samp$b_AMBUAMB+emp_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(emp_GP_P1_posterior_samp$b_AMBUAMB+emp_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(emp_GP_P2_posterior_samp$b_AMBUAMB+emp_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(emp_GP_P0_posterior_samp$b_AMBUAMB+emp_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(emp_GP_P1_posterior_samp$b_AMBUAMB+emp_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(emp_GP_P2_posterior_samp$b_AMBUAMB+emp_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.025)),
                              upper=c(quantile(emp_GP_P0_posterior_samp$b_AMBUAMB,0.975),quantile(emp_GP_P1_posterior_samp$b_AMBUAMB,0.975),quantile(emp_GP_P2_posterior_samp$b_AMBUAMB,0.975),quantile(emp_GP_P0_posterior_samp$b_AMBUAMB+emp_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(emp_GP_P1_posterior_samp$b_AMBUAMB+emp_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(emp_GP_P2_posterior_samp$b_AMBUAMB+emp_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(emp_GP_P0_posterior_samp$b_AMBUAMB+emp_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(emp_GP_P1_posterior_samp$b_AMBUAMB+emp_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(emp_GP_P2_posterior_samp$b_AMBUAMB+emp_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.975)))
by_construction_lstm <- data.frame(ROI=rep(c(0,1,2),3),coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=3),mean=c(mean(lstm_GP_P0_posterior_samp$b_AMBUAMB),mean(lstm_GP_P1_posterior_samp$b_AMBUAMB),mean(lstm_GP_P2_posterior_samp$b_AMBUAMB),mean(lstm_GP_P0_posterior_samp$b_AMBUAMB+lstm_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`),mean(lstm_GP_P1_posterior_samp$b_AMBUAMB+lstm_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`),mean(lstm_GP_P2_posterior_samp$b_AMBUAMB+lstm_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`),mean(lstm_GP_P0_posterior_samp$b_AMBUAMB+lstm_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`),mean(lstm_GP_P1_posterior_samp$b_AMBUAMB+lstm_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`),mean(lstm_GP_P2_posterior_samp$b_AMBUAMB+lstm_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`)),
                                   lower=c(quantile(lstm_GP_P0_posterior_samp$b_AMBUAMB,0.025),quantile(lstm_GP_P1_posterior_samp$b_AMBUAMB,0.025),quantile(lstm_GP_P2_posterior_samp$b_AMBUAMB,0.025),quantile(lstm_GP_P0_posterior_samp$b_AMBUAMB+lstm_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(lstm_GP_P1_posterior_samp$b_AMBUAMB+lstm_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(lstm_GP_P2_posterior_samp$b_AMBUAMB+lstm_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(lstm_GP_P0_posterior_samp$b_AMBUAMB+lstm_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(lstm_GP_P1_posterior_samp$b_AMBUAMB+lstm_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(lstm_GP_P2_posterior_samp$b_AMBUAMB+lstm_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.025)),
                                   upper=c(quantile(lstm_GP_P0_posterior_samp$b_AMBUAMB,0.975),quantile(lstm_GP_P1_posterior_samp$b_AMBUAMB,0.975),quantile(lstm_GP_P2_posterior_samp$b_AMBUAMB,0.975),quantile(lstm_GP_P0_posterior_samp$b_AMBUAMB+lstm_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(lstm_GP_P1_posterior_samp$b_AMBUAMB+lstm_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(lstm_GP_P2_posterior_samp$b_AMBUAMB+lstm_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(lstm_GP_P0_posterior_samp$b_AMBUAMB+lstm_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(lstm_GP_P1_posterior_samp$b_AMBUAMB+lstm_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(lstm_GP_P2_posterior_samp$b_AMBUAMB+lstm_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.975)))
by_construction_gpt2 <- data.frame(ROI=rep(c(0,1,2),3),coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=3),mean=c(mean(gpt2_GP_P0_posterior_samp$b_AMBUAMB),mean(gpt2_GP_P1_posterior_samp$b_AMBUAMB),mean(gpt2_GP_P2_posterior_samp$b_AMBUAMB),mean(gpt2_GP_P0_posterior_samp$b_AMBUAMB+gpt2_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`),mean(gpt2_GP_P1_posterior_samp$b_AMBUAMB+gpt2_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`),mean(gpt2_GP_P2_posterior_samp$b_AMBUAMB+gpt2_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`),mean(gpt2_GP_P0_posterior_samp$b_AMBUAMB+gpt2_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`),mean(gpt2_GP_P1_posterior_samp$b_AMBUAMB+gpt2_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`),mean(gpt2_GP_P2_posterior_samp$b_AMBUAMB+gpt2_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`)),
                                   lower=c(quantile(gpt2_GP_P0_posterior_samp$b_AMBUAMB,0.025),quantile(gpt2_GP_P1_posterior_samp$b_AMBUAMB,0.025),quantile(gpt2_GP_P2_posterior_samp$b_AMBUAMB,0.025),quantile(gpt2_GP_P0_posterior_samp$b_AMBUAMB+gpt2_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(gpt2_GP_P1_posterior_samp$b_AMBUAMB+gpt2_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(gpt2_GP_P2_posterior_samp$b_AMBUAMB+gpt2_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(gpt2_GP_P0_posterior_samp$b_AMBUAMB+gpt2_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(gpt2_GP_P1_posterior_samp$b_AMBUAMB+gpt2_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(gpt2_GP_P2_posterior_samp$b_AMBUAMB+gpt2_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.025)),
                                   upper=c(quantile(gpt2_GP_P0_posterior_samp$b_AMBUAMB,0.975),quantile(gpt2_GP_P1_posterior_samp$b_AMBUAMB,0.975),quantile(gpt2_GP_P2_posterior_samp$b_AMBUAMB,0.975),quantile(gpt2_GP_P0_posterior_samp$b_AMBUAMB+gpt2_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(gpt2_GP_P1_posterior_samp$b_AMBUAMB+gpt2_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(gpt2_GP_P2_posterior_samp$b_AMBUAMB+gpt2_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(gpt2_GP_P0_posterior_samp$b_AMBUAMB+gpt2_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(gpt2_GP_P1_posterior_samp$b_AMBUAMB+gpt2_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(gpt2_GP_P2_posterior_samp$b_AMBUAMB+gpt2_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.975)))
by_construction_nosurp <- data.frame(ROI=rep(c(0,1,2),3),coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),each=3),mean=c(mean(nosurp_GP_P0_posterior_samp$b_AMBUAMB),mean(nosurp_GP_P1_posterior_samp$b_AMBUAMB),mean(nosurp_GP_P2_posterior_samp$b_AMBUAMB),mean(nosurp_GP_P0_posterior_samp$b_AMBUAMB+nosurp_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`),mean(nosurp_GP_P1_posterior_samp$b_AMBUAMB+nosurp_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`),mean(nosurp_GP_P2_posterior_samp$b_AMBUAMB+nosurp_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`),mean(nosurp_GP_P0_posterior_samp$b_AMBUAMB+nosurp_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`),mean(nosurp_GP_P1_posterior_samp$b_AMBUAMB+nosurp_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`),mean(nosurp_GP_P2_posterior_samp$b_AMBUAMB+nosurp_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`)),
                                     lower=c(quantile(nosurp_GP_P0_posterior_samp$b_AMBUAMB,0.025),quantile(nosurp_GP_P1_posterior_samp$b_AMBUAMB,0.025),quantile(nosurp_GP_P2_posterior_samp$b_AMBUAMB,0.025),quantile(nosurp_GP_P0_posterior_samp$b_AMBUAMB+nosurp_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(nosurp_GP_P1_posterior_samp$b_AMBUAMB+nosurp_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(nosurp_GP_P2_posterior_samp$b_AMBUAMB+nosurp_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.025),quantile(nosurp_GP_P0_posterior_samp$b_AMBUAMB+nosurp_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(nosurp_GP_P1_posterior_samp$b_AMBUAMB+nosurp_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.025),quantile(nosurp_GP_P2_posterior_samp$b_AMBUAMB+nosurp_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.025)),
                                     upper=c(quantile(nosurp_GP_P0_posterior_samp$b_AMBUAMB,0.975),quantile(nosurp_GP_P1_posterior_samp$b_AMBUAMB,0.975),quantile(nosurp_GP_P2_posterior_samp$b_AMBUAMB,0.975),quantile(nosurp_GP_P0_posterior_samp$b_AMBUAMB+nosurp_GP_P0_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(nosurp_GP_P1_posterior_samp$b_AMBUAMB+nosurp_GP_P1_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(nosurp_GP_P2_posterior_samp$b_AMBUAMB+nosurp_GP_P2_posterior_samp$`b_AMBUAMB:SZM1`,0.975),quantile(nosurp_GP_P0_posterior_samp$b_AMBUAMB+nosurp_GP_P0_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(nosurp_GP_P1_posterior_samp$b_AMBUAMB+nosurp_GP_P1_posterior_samp$`b_AMBUAMB:SZM2`,0.975),quantile(nosurp_GP_P2_posterior_samp$b_AMBUAMB+nosurp_GP_P2_posterior_samp$`b_AMBUAMB:SZM2`,0.975)))

saveRDS(by_construction,"ClassicGP/by_construction.rds")
saveRDS(by_construction_lstm,"ClassicGP/by_construction_lstm.rds")
saveRDS(by_construction_gpt2,"ClassicGP/by_construction_gpt2.rds")
saveRDS(by_construction_nosurp,"ClassicGP/by_construction_nosurp.rds")


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
sampled_correlations_P2 <- data.frame(Correlation=rep(NA,9000),EOI=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),3000),model=rep(c("lstm","lstm","lstm","gpt2","gpt2","gpt2","nosurp","nosurp","nosurp"),1000),ROI=1)
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
#saveRDS(sampled_correlations_maxregion,"sampled_correlations_maxregion_GP.rds")





for_plotting_GP <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=mean)
colnames(for_plotting_GP) <- c("EOI","model","Correlation")
for_plotting_GP$SE <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=sd)$x
for_plotting_GP$ROI <- 0

temp <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=sd)$x
temp$ROI <- 1
for_plotting_GP <- rbind(for_plotting_GP,temp)
rm(temp)

temp <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=sd)$x
temp$ROI <- 2
for_plotting_GP <- rbind(for_plotting_GP,temp)
rm(temp)


#saveRDS(for_plotting_GP,"for_plotting_GP.rds")

for_plotting_GP_max <- for_plotting_GP[(for_plotting_GP$ROI==1),]


ggplot(for_plotting_GP_max,aes(x=Correlation,y=EOI,fill=model))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))


ggplot(for_plotting_GP,aes(x=Correlation,y=EOI,fill=model))+
  facet_grid(~ROI)+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))




cor.test(posterior_emp_MVRR_P0$mean,posterior_gpt2_MVRR_P0$mean)$estimate
cor.test(posterior_emp_NPS_P0$mean,posterior_gpt2_NPS_P0$mean)$estimate
cor.test(posterior_emp_NPZ_P0$mean,posterior_gpt2_NPZ_P0$mean)$estimate
cor.test(posterior_emp_MVRR_P0$mean,posterior_lstm_MVRR_P0$mean)$estimate
cor.test(posterior_emp_NPS_P0$mean,posterior_lstm_NPS_P0$mean)$estimate
cor.test(posterior_emp_NPZ_P0$mean,posterior_lstm_NPZ_P0$mean)$estimate
cor.test(posterior_emp_MVRR_P0$mean,posterior_nosurp_MVRR_P0$mean)$estimate
cor.test(posterior_emp_NPS_P0$mean,posterior_nosurp_NPS_P0$mean)$estimate
cor.test(posterior_emp_NPZ_P0$mean,posterior_nosurp_NPZ_P0$mean)$estimate
cor.test(posterior_emp_MVRR_P1$mean,posterior_gpt2_MVRR_P1$mean)$estimate
cor.test(posterior_emp_NPS_P1$mean,posterior_gpt2_NPS_P1$mean)$estimate
cor.test(posterior_emp_NPZ_P1$mean,posterior_gpt2_NPZ_P1$mean)$estimate
cor.test(posterior_emp_MVRR_P1$mean,posterior_lstm_MVRR_P1$mean)$estimate
cor.test(posterior_emp_NPS_P1$mean,posterior_lstm_NPS_P1$mean)$estimate
cor.test(posterior_emp_NPZ_P1$mean,posterior_lstm_NPZ_P1$mean)$estimate
cor.test(posterior_emp_MVRR_P1$mean,posterior_nosurp_MVRR_P1$mean)$estimate
cor.test(posterior_emp_NPS_P1$mean,posterior_nosurp_NPS_P1$mean)$estimate
cor.test(posterior_emp_NPZ_P1$mean,posterior_nosurp_NPZ_P1$mean)$estimate




posterior_emp_MVRR_P0$model <- "emp" 
posterior_emp_MVRR_P1$model <- "emp" 
posterior_emp_MVRR_P2$model <- "emp" 
posterior_lstm_MVRR_P0$model <- "lstm" 
posterior_lstm_MVRR_P1$model <- "lstm" 
posterior_lstm_MVRR_P2$model <- "lstm" 
posterior_gpt2_MVRR_P0$model <- "gpt2" 
posterior_gpt2_MVRR_P1$model <- "gpt2" 
posterior_gpt2_MVRR_P2$model <- "gpt2" 
posterior_nosurp_MVRR_P0$model <- "nosurp" 
posterior_nosurp_MVRR_P1$model <- "nosurp" 
posterior_nosurp_MVRR_P2$model <- "nosurp"  
posterior_emp_NPS_P0$model <- "emp" 
posterior_emp_NPS_P1$model <- "emp" 
posterior_emp_NPS_P2$model <- "emp" 
posterior_lstm_NPS_P0$model <- "lstm" 
posterior_lstm_NPS_P1$model <- "lstm" 
posterior_lstm_NPS_P2$model <- "lstm" 
posterior_gpt2_NPS_P0$model <- "gpt2" 
posterior_gpt2_NPS_P1$model <- "gpt2" 
posterior_gpt2_NPS_P2$model <- "gpt2" 
posterior_nosurp_NPS_P0$model <- "nosurp" 
posterior_nosurp_NPS_P1$model <- "nosurp" 
posterior_nosurp_NPS_P2$model <- "nosurp" 
posterior_emp_NPZ_P0$model <- "emp" 
posterior_emp_NPZ_P1$model <- "emp" 
posterior_emp_NPZ_P2$model <- "emp" 
posterior_lstm_NPZ_P0$model <- "lstm" 
posterior_lstm_NPZ_P1$model <- "lstm" 
posterior_lstm_NPZ_P2$model <- "lstm" 
posterior_gpt2_NPZ_P0$model <- "gpt2" 
posterior_gpt2_NPZ_P1$model <- "gpt2" 
posterior_gpt2_NPZ_P2$model <- "gpt2" 
posterior_nosurp_NPZ_P0$model <- "nosurp" 
posterior_nosurp_NPZ_P1$model <- "nosurp" 
posterior_nosurp_NPZ_P2$model <- "nosurp" 

df_pointestimate <- 
  data.frame(emp=c(posterior_emp_MVRR_P1$mean,posterior_emp_NPS_P1$mean,posterior_emp_NPZ_P1$mean),
             lstm=c(posterior_lstm_MVRR_P1$mean,posterior_lstm_NPS_P1$mean,posterior_lstm_NPZ_P1$mean),
             gpt2=c(posterior_gpt2_MVRR_P1$mean,posterior_gpt2_NPS_P1$mean,posterior_gpt2_NPZ_P1$mean),
             nosurp=c(posterior_nosurp_MVRR_P1$mean,posterior_nosurp_NPS_P1$mean,posterior_nosurp_NPZ_P1$mean))
df_pointestimate$EOI <- c(rep("GPE_MVRR",24),rep("GPE_NPS",24),rep("GPE_NPZ",24))
df_pointestimate$item <- rep(1:24,3)
saveRDS(df_pointestimate,"df_pointestimate_GP.rds")
