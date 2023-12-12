library(lme4)
library(tidyr)
library(stringr)
library(posterior)
library(tidybayes)
library(tidyverse)
library(dplyr)
library(brms)

emp_AA_P0 <- readRDS("AA_logadd_prior1/brm_prior1_AA_emp_0_log.rds")
emp_AA_P1 <- readRDS("AA_logadd_prior1/brm_prior1_AA_emp_1_log.rds")
emp_AA_P2 <- readRDS("AA_logadd_prior1/brm_prior1_AA_emp_2_log.rds")
lstm_AA_P0 <- readRDS("AA_logadd_prior1/brm_prior1_pred_lstm_0_logadd.rds")
lstm_AA_P1 <- readRDS("AA_logadd_prior1/brm_prior1_pred_lstm_1_logadd.rds")
lstm_AA_P2 <- readRDS("AA_logadd_prior1/brm_prior1_pred_lstm_2_logadd.rds")
gpt2_AA_P0 <- readRDS("AA_logadd_prior1/brm_prior1_pred_gpt2_0_logadd.rds")
gpt2_AA_P1 <- readRDS("AA_logadd_prior1/brm_prior1_pred_gpt2_1_logadd.rds")
gpt2_AA_P2 <- readRDS("AA_logadd_prior1/brm_prior1_pred_gpt2_2_logadd.rds")
nosurp_AA_P0 <- readRDS("AA_logadd_prior1/brm_prior1_pred_nosurp_0_logadd.rds")
nosurp_AA_P1 <- readRDS("AA_logadd_prior1/brm_prior1_pred_nosurp_1_logadd.rds")
nosurp_AA_P2 <- readRDS("AA_logadd_prior1/brm_prior1_pred_nosurp_2_logadd.rds")





posterior_samp <- posterior_samples(emp_AA_P0)
randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(ambiguity|height|Intercept)',colnames(posterior_samp))]
saveRDS(randomslope_names,"AA_logadd_prior1/AA_randomslopesnames.rds")
randomslope_names <- readRDS("AA_logadd_prior1/AA_randomslopesnames.rds")

emp_AA_P0_posterior_samp <- posterior_samples(emp_AA_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstm_AA_P0_posterior_samp <- posterior_samples(lstm_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2_AA_P0_posterior_samp <- posterior_samples(gpt2_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
nosurp_AA_P0_posterior_samp <- posterior_samples(nosurp_AA_P0, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
emp_AA_P1_posterior_samp <- posterior_samples(emp_AA_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstm_AA_P1_posterior_samp <- posterior_samples(lstm_AA_P1, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2_AA_P1_posterior_samp <- posterior_samples(gpt2_AA_P1, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
nosurp_AA_P1_posterior_samp <- posterior_samples(nosurp_AA_P1, fixed=TRUE, pars=
                                                   c("b_Intercept","b_ambiguity","b_height",randomslope_names))
emp_AA_P2_posterior_samp <- posterior_samples(emp_AA_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_ambiguity","b_height",randomslope_names))
lstm_AA_P2_posterior_samp <- posterior_samples(lstm_AA_P2, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
gpt2_AA_P2_posterior_samp <- posterior_samples(gpt2_AA_P2, fixed=TRUE, pars=
                                                 c("b_Intercept","b_ambiguity","b_height",randomslope_names))
nosurp_AA_P2_posterior_samp <- posterior_samples(nosurp_AA_P2, fixed=TRUE, pars=
                                                   c("b_Intercept","b_ambiguity","b_height",randomslope_names))

rm(emp_AA_P0, lstm_AA_P0, gpt2_AA_P0, nosurp_AA_P0, emp_AA_P1, lstm_AA_P1,
gpt2_AA_P1, nosurp_AA_P1, emp_AA_P2, lstm_AA_P2, gpt2_AA_P2, nosurp_AA_P2,posterior_samp)
saveRDS(emp_AA_P0_posterior_samp,"AA_logadd_prior1/emp_AA_P0_posterior_samp.rds")
saveRDS(lstm_AA_P0_posterior_samp,"AA_logadd_prior1/lstm_AA_P0_posterior_samp.rds")
saveRDS(gpt2_AA_P0_posterior_samp,"AA_logadd_prior1/gpt2_AA_P0_posterior_samp.rds")
saveRDS(nosurp_AA_P0_posterior_samp,"AA_logadd_prior1/nosurp_AA_P0_posterior_samp.rds")
saveRDS(emp_AA_P1_posterior_samp,"AA_logadd_prior1/emp_AA_P1_posterior_samp.rds")
saveRDS(lstm_AA_P1_posterior_samp,"AA_logadd_prior1/lstm_AA_P1_posterior_samp.rds")
saveRDS(gpt2_AA_P1_posterior_samp,"AA_logadd_prior1/gpt2_AA_P1_posterior_samp.rds")
saveRDS(nosurp_AA_P1_posterior_samp,"AA_logadd_prior1/nosurp_AA_P1_posterior_samp.rds")
saveRDS(emp_AA_P2_posterior_samp,"AA_logadd_prior1/emp_AA_P2_posterior_samp.rds")
saveRDS(lstm_AA_P2_posterior_samp,"AA_logadd_prior1/lstm_AA_P2_posterior_samp.rds")
saveRDS(gpt2_AA_P2_posterior_samp,"AA_logadd_prior1/gpt2_AA_P2_posterior_samp.rds")
saveRDS(nosurp_AA_P2_posterior_samp,"AA_logadd_prior1/nosurp_AA_P2_posterior_samp.rds")

emp_AA_P0_posterior_samp <- readRDS("AA_logadd_prior1/emp_AA_P0_posterior_samp.rds")
lstm_AA_P0_posterior_samp <- readRDS("AA_logadd_prior1/lstm_AA_P0_posterior_samp.rds")
gpt2_AA_P0_posterior_samp <- readRDS("AA_logadd_prior1/gpt2_AA_P0_posterior_samp.rds")
nosurp_AA_P0_posterior_samp <- readRDS("AA_logadd_prior1/nosurp_AA_P0_posterior_samp.rds")
emp_AA_P1_posterior_samp <- readRDS("AA_logadd_prior1/emp_AA_P1_posterior_samp.rds")
lstm_AA_P1_posterior_samp <- readRDS("AA_logadd_prior1/lstm_AA_P1_posterior_samp.rds")
gpt2_AA_P1_posterior_samp <- readRDS("AA_logadd_prior1/gpt2_AA_P1_posterior_samp.rds")
nosurp_AA_P1_posterior_samp <- readRDS("AA_logadd_prior1/nosurp_AA_P1_posterior_samp.rds")
emp_AA_P2_posterior_samp <- readRDS("AA_logadd_prior1/emp_AA_P2_posterior_samp.rds")
lstm_AA_P2_posterior_samp <- readRDS("AA_logadd_prior1/lstm_AA_P2_posterior_samp.rds")
gpt2_AA_P2_posterior_samp <- readRDS("AA_logadd_prior1/gpt2_AA_P2_posterior_samp.rds")
nosurp_AA_P2_posterior_samp <- readRDS("AA_logadd_prior1/nosurp_AA_P2_posterior_samp.rds")




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
emp_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_AA_P0_posterior_samp[,1]+emp_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(emp_AA_P0_posterior_samp[,2])+(1/2)*emp_AA_P0_posterior_samp[,3]+(-1/3)*emp_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*emp_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(emp_AA_P0_posterior_samp[,1]+emp_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P0_posterior_samp[,2])+(2/3)*emp_AA_P0_posterior_samp[,(1+27):(24+27)])
emp_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_AA_P0_posterior_samp[,1]+emp_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(emp_AA_P0_posterior_samp[,2])-(1/2)*emp_AA_P0_posterior_samp[,3]+(-1/3)*emp_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*emp_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(emp_AA_P0_posterior_samp[,1]+emp_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P0_posterior_samp[,2])+(2/3)*emp_AA_P0_posterior_samp[,(1+27):(24+27)])
emp_AA_P0_posterior_samp[,(49+ncols):(72+ncols)] <- exp(emp_AA_P0_posterior_samp[,1]+emp_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P0_posterior_samp[,2])+(2/3)*emp_AA_P0_posterior_samp[,(1+27):(24+27)])
emp_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_AA_P1_posterior_samp[,1]+emp_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(emp_AA_P1_posterior_samp[,2])+(1/2)*emp_AA_P1_posterior_samp[,3]+(-1/3)*emp_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*emp_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(emp_AA_P1_posterior_samp[,1]+emp_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P1_posterior_samp[,2])+(2/3)*emp_AA_P1_posterior_samp[,(1+27):(24+27)])
emp_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_AA_P1_posterior_samp[,1]+emp_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(emp_AA_P1_posterior_samp[,2])-(1/2)*emp_AA_P1_posterior_samp[,3]+(-1/3)*emp_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*emp_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(emp_AA_P1_posterior_samp[,1]+emp_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P1_posterior_samp[,2])+(2/3)*emp_AA_P1_posterior_samp[,(1+27):(24+27)])
emp_AA_P1_posterior_samp[,(49+ncols):(72+ncols)] <- exp(emp_AA_P1_posterior_samp[,1]+emp_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P1_posterior_samp[,2])+(2/3)*emp_AA_P1_posterior_samp[,(1+27):(24+27)])
emp_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_AA_P2_posterior_samp[,1]+emp_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(emp_AA_P2_posterior_samp[,2])+(1/2)*emp_AA_P2_posterior_samp[,3]+(-1/3)*emp_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*emp_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(emp_AA_P2_posterior_samp[,1]+emp_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P2_posterior_samp[,2])+(2/3)*emp_AA_P2_posterior_samp[,(1+27):(24+27)])
emp_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_AA_P2_posterior_samp[,1]+emp_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(emp_AA_P2_posterior_samp[,2])-(1/2)*emp_AA_P2_posterior_samp[,3]+(-1/3)*emp_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*emp_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(emp_AA_P2_posterior_samp[,1]+emp_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P2_posterior_samp[,2])+(2/3)*emp_AA_P2_posterior_samp[,(1+27):(24+27)])
emp_AA_P2_posterior_samp[,(49+ncols):(72+ncols)] <- exp(emp_AA_P2_posterior_samp[,1]+emp_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(emp_AA_P2_posterior_samp[,2])+(2/3)*emp_AA_P2_posterior_samp[,(1+27):(24+27)])
lstm_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_AA_P0_posterior_samp[,1]+lstm_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(lstm_AA_P0_posterior_samp[,2])+(1/2)*lstm_AA_P0_posterior_samp[,3]+(-1/3)*lstm_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*lstm_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(lstm_AA_P0_posterior_samp[,1]+lstm_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(lstm_AA_P0_posterior_samp[,2])+(2/3)*lstm_AA_P0_posterior_samp[,(1+27):(24+27)])
lstm_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(lstm_AA_P0_posterior_samp[,1]+lstm_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(lstm_AA_P0_posterior_samp[,2])-(1/2)*lstm_AA_P0_posterior_samp[,3]+(-1/3)*lstm_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*lstm_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(lstm_AA_P0_posterior_samp[,1]+lstm_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(lstm_AA_P0_posterior_samp[,2])+(2/3)*lstm_AA_P0_posterior_samp[,(1+27):(24+27)])
lstm_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_AA_P1_posterior_samp[,1]+lstm_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(lstm_AA_P1_posterior_samp[,2])+(1/2)*lstm_AA_P1_posterior_samp[,3]+(-1/3)*lstm_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*lstm_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(lstm_AA_P1_posterior_samp[,1]+lstm_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(lstm_AA_P1_posterior_samp[,2])+(2/3)*lstm_AA_P1_posterior_samp[,(1+27):(24+27)])
lstm_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(lstm_AA_P1_posterior_samp[,1]+lstm_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(lstm_AA_P1_posterior_samp[,2])-(1/2)*lstm_AA_P1_posterior_samp[,3]+(-1/3)*lstm_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*lstm_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(lstm_AA_P1_posterior_samp[,1]+lstm_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(lstm_AA_P1_posterior_samp[,2])+(2/3)*lstm_AA_P1_posterior_samp[,(1+27):(24+27)])
lstm_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_AA_P2_posterior_samp[,1]+lstm_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(lstm_AA_P2_posterior_samp[,2])+(1/2)*lstm_AA_P2_posterior_samp[,3]+(-1/3)*lstm_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*lstm_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(lstm_AA_P2_posterior_samp[,1]+lstm_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(lstm_AA_P2_posterior_samp[,2])+(2/3)*lstm_AA_P2_posterior_samp[,(1+27):(24+27)])
lstm_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(lstm_AA_P2_posterior_samp[,1]+lstm_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(lstm_AA_P2_posterior_samp[,2])-(1/2)*lstm_AA_P2_posterior_samp[,3]+(-1/3)*lstm_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*lstm_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(lstm_AA_P2_posterior_samp[,1]+lstm_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(lstm_AA_P2_posterior_samp[,2])+(2/3)*lstm_AA_P2_posterior_samp[,(1+27):(24+27)])
gpt2_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_AA_P0_posterior_samp[,1]+gpt2_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(gpt2_AA_P0_posterior_samp[,2])+(1/2)*gpt2_AA_P0_posterior_samp[,3]+(-1/3)*gpt2_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(gpt2_AA_P0_posterior_samp[,1]+gpt2_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(gpt2_AA_P0_posterior_samp[,2])+(2/3)*gpt2_AA_P0_posterior_samp[,(1+27):(24+27)])
gpt2_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(gpt2_AA_P0_posterior_samp[,1]+gpt2_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(gpt2_AA_P0_posterior_samp[,2])-(1/2)*gpt2_AA_P0_posterior_samp[,3]+(-1/3)*gpt2_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(gpt2_AA_P0_posterior_samp[,1]+gpt2_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(gpt2_AA_P0_posterior_samp[,2])+(2/3)*gpt2_AA_P0_posterior_samp[,(1+27):(24+27)])
gpt2_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_AA_P1_posterior_samp[,1]+gpt2_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(gpt2_AA_P1_posterior_samp[,2])+(1/2)*gpt2_AA_P1_posterior_samp[,3]+(-1/3)*gpt2_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(gpt2_AA_P1_posterior_samp[,1]+gpt2_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(gpt2_AA_P1_posterior_samp[,2])+(2/3)*gpt2_AA_P1_posterior_samp[,(1+27):(24+27)])
gpt2_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(gpt2_AA_P1_posterior_samp[,1]+gpt2_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(gpt2_AA_P1_posterior_samp[,2])-(1/2)*gpt2_AA_P1_posterior_samp[,3]+(-1/3)*gpt2_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(gpt2_AA_P1_posterior_samp[,1]+gpt2_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(gpt2_AA_P1_posterior_samp[,2])+(2/3)*gpt2_AA_P1_posterior_samp[,(1+27):(24+27)])
gpt2_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_AA_P2_posterior_samp[,1]+gpt2_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(gpt2_AA_P2_posterior_samp[,2])+(1/2)*gpt2_AA_P2_posterior_samp[,3]+(-1/3)*gpt2_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*gpt2_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(gpt2_AA_P2_posterior_samp[,1]+gpt2_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(gpt2_AA_P2_posterior_samp[,2])+(2/3)*gpt2_AA_P2_posterior_samp[,(1+27):(24+27)])
gpt2_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(gpt2_AA_P2_posterior_samp[,1]+gpt2_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(gpt2_AA_P2_posterior_samp[,2])-(1/2)*gpt2_AA_P2_posterior_samp[,3]+(-1/3)*gpt2_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*gpt2_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(gpt2_AA_P2_posterior_samp[,1]+gpt2_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(gpt2_AA_P2_posterior_samp[,2])+(2/3)*gpt2_AA_P2_posterior_samp[,(1+27):(24+27)])
nosurp_AA_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_AA_P0_posterior_samp[,1]+nosurp_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(nosurp_AA_P0_posterior_samp[,2])+(1/2)*nosurp_AA_P0_posterior_samp[,3]+(-1/3)*nosurp_AA_P0_posterior_samp[,(1+27):(24+27)]+(1/2)*nosurp_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(nosurp_AA_P0_posterior_samp[,1]+nosurp_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(nosurp_AA_P0_posterior_samp[,2])+(2/3)*nosurp_AA_P0_posterior_samp[,(1+27):(24+27)])
nosurp_AA_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(nosurp_AA_P0_posterior_samp[,1]+nosurp_AA_P0_posterior_samp[,(1+3):(24+3)]+(-1/3)*(nosurp_AA_P0_posterior_samp[,2])-(1/2)*nosurp_AA_P0_posterior_samp[,3]+(-1/3)*nosurp_AA_P0_posterior_samp[,(1+27):(24+27)]-(1/2)*nosurp_AA_P0_posterior_samp[,(1+51):(24+51)])-exp(nosurp_AA_P0_posterior_samp[,1]+nosurp_AA_P0_posterior_samp[,(1+3):(24+3)]+(2/3)*(nosurp_AA_P0_posterior_samp[,2])+(2/3)*nosurp_AA_P0_posterior_samp[,(1+27):(24+27)])
nosurp_AA_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_AA_P1_posterior_samp[,1]+nosurp_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(nosurp_AA_P1_posterior_samp[,2])+(1/2)*nosurp_AA_P1_posterior_samp[,3]+(-1/3)*nosurp_AA_P1_posterior_samp[,(1+27):(24+27)]+(1/2)*nosurp_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(nosurp_AA_P1_posterior_samp[,1]+nosurp_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(nosurp_AA_P1_posterior_samp[,2])+(2/3)*nosurp_AA_P1_posterior_samp[,(1+27):(24+27)])
nosurp_AA_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(nosurp_AA_P1_posterior_samp[,1]+nosurp_AA_P1_posterior_samp[,(1+3):(24+3)]+(-1/3)*(nosurp_AA_P1_posterior_samp[,2])-(1/2)*nosurp_AA_P1_posterior_samp[,3]+(-1/3)*nosurp_AA_P1_posterior_samp[,(1+27):(24+27)]-(1/2)*nosurp_AA_P1_posterior_samp[,(1+51):(24+51)])-exp(nosurp_AA_P1_posterior_samp[,1]+nosurp_AA_P1_posterior_samp[,(1+3):(24+3)]+(2/3)*(nosurp_AA_P1_posterior_samp[,2])+(2/3)*nosurp_AA_P1_posterior_samp[,(1+27):(24+27)])
nosurp_AA_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_AA_P2_posterior_samp[,1]+nosurp_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(nosurp_AA_P2_posterior_samp[,2])+(1/2)*nosurp_AA_P2_posterior_samp[,3]+(-1/3)*nosurp_AA_P2_posterior_samp[,(1+27):(24+27)]+(1/2)*nosurp_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(nosurp_AA_P2_posterior_samp[,1]+nosurp_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(nosurp_AA_P2_posterior_samp[,2])+(2/3)*nosurp_AA_P2_posterior_samp[,(1+27):(24+27)])
nosurp_AA_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(nosurp_AA_P2_posterior_samp[,1]+nosurp_AA_P2_posterior_samp[,(1+3):(24+3)]+(-1/3)*(nosurp_AA_P2_posterior_samp[,2])-(1/2)*nosurp_AA_P2_posterior_samp[,3]+(-1/3)*nosurp_AA_P2_posterior_samp[,(1+27):(24+27)]-(1/2)*nosurp_AA_P2_posterior_samp[,(1+51):(24+51)])-exp(nosurp_AA_P2_posterior_samp[,1]+nosurp_AA_P2_posterior_samp[,(1+3):(24+3)]+(2/3)*(nosurp_AA_P2_posterior_samp[,2])+(2/3)*nosurp_AA_P2_posterior_samp[,(1+27):(24+27)])


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

#P0
sampled_correlations_P0 <- data.frame(Correlation=rep(NA,6000),EOI=rep(c("GPE_high","GPE_low"),3000),model=rep(c("lstm","lstm","gpt2","gpt2","nosurp","nosurp"),1000),ROI=0)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,48),lstm=rep(NA,48),gpt2=rep(NA,48),nosurp=rep(NA,48),EOI=c(rep("GPE_high",24),rep("GPE_low",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_AA_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_AA_P0_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_AA_P0_posterior_samp[,j+24+ncols],1)
  }
  sampled_correlations_P0[((i-1)*6+1):((i-1)*6+6),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                            cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate)
}
#P0


#P1
sampled_correlations_P1 <- data.frame(Correlation=rep(NA,6000),EOI=rep(c("GPE_high","GPE_low"),3000),model=rep(c("lstm","lstm","gpt2","gpt2","nosurp","nosurp"),1000),ROI=1)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,48),lstm=rep(NA,48),gpt2=rep(NA,48),nosurp=rep(NA,48),EOI=c(rep("GPE_high",24),rep("GPE_low",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_AA_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_AA_P1_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_AA_P1_posterior_samp[,j+24+ncols],1)
  }
  sampled_correlations_P1[((i-1)*6+1):((i-1)*6+6),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate)
}
#P1



#P2
sampled_correlations_P2 <- data.frame(Correlation=rep(NA,6000),EOI=rep(c("GPE_high","GPE_low"),3000),model=rep(c("lstm","lstm","gpt2","gpt2","nosurp","nosurp"),1000),ROI=2)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,48),lstm=rep(NA,48),gpt2=rep(NA,48),nosurp=rep(NA,48),EOI=c(rep("GPE_high",24),rep("GPE_low",24)))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_AA_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j+24,1] <- sample(emp_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,2] <- sample(lstm_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,3] <- sample(gpt2_AA_P2_posterior_samp[,j+24+ncols],1)
    posterior_onesampleeachitem[j+24,4] <- sample(nosurp_AA_P2_posterior_samp[,j+24+ncols],1)
  }
  sampled_correlations_P2[((i-1)*6+1):((i-1)*6+6),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[25:48,1],posterior_onesampleeachitem[25:48,4])$estimate)
}
#P2




for(i in unique(sampled_correlations_P0$EOI)){
  for(j in unique(sampled_correlations_P0$model)){
    hist(sampled_correlations_P0$Correlation[sampled_correlations_P0$EOI==i&sampled_correlations_P0$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P0,"AA_logadd_prior1/sampled_correlations_P0_AA.rds")

for(i in unique(sampled_correlations_P1$EOI)){
  for(j in unique(sampled_correlations_P1$model)){
    hist(sampled_correlations_P1$Correlation[sampled_correlations_P1$EOI==i&sampled_correlations_P1$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P1,"AA_logadd_prior1/sampled_correlations_P1_AA.rds")

for(i in unique(sampled_correlations_P2$EOI)){
  for(j in unique(sampled_correlations_P2$model)){
    hist(sampled_correlations_P2$Correlation[sampled_correlations_P2$EOI==i&sampled_correlations_P2$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P2,"AA_logadd_prior1/sampled_correlations_P2_AA.rds")



