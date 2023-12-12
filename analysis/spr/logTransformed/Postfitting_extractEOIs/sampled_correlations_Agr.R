library(lme4)
library(tidyr)
library(stringr)
library(posterior)
library(tidybayes)
library(tidyverse)
library(dplyr)
library(brms)

emp_Agr_P0 <- readRDS("Agr_logadd_prior1/brm_Agr_P0_log.rds")
emp_Agr_P1 <- readRDS("Agr_logadd_prior1/brm_Agr_P1_log.rds")
emp_Agr_P2 <- readRDS("Agr_logadd_prior1/brm_Agr_P2_log.rds")
lstm_Agr_P0 <- readRDS("Agr_logadd_prior1/brm_predicted_lstm_Agr_P0_logadd.rds")
lstm_Agr_P1 <- readRDS("Agr_logadd_prior1/brm_predicted_lstm_Agr_P1_logadd.rds")
lstm_Agr_P2 <- readRDS("Agr_logadd_prior1/brm_predicted_lstm_Agr_P2_logadd.rds")
gpt2_Agr_P0 <- readRDS("Agr_logadd_prior1/brm_predicted_gpt2_Agr_P0_logadd.rds")
gpt2_Agr_P1 <- readRDS("Agr_logadd_prior1/brm_predicted_gpt2_Agr_P1_logadd.rds")
gpt2_Agr_P2 <- readRDS("Agr_logadd_prior1/brm_predicted_gpt2_Agr_P2_logadd.rds")
nosurp_Agr_P0 <- readRDS("Agr_logadd_prior1/brm_predicted_nosurp_Agr_P0_logadd.rds")
nosurp_Agr_P1 <- readRDS("Agr_logadd_prior1/brm_predicted_nosurp_Agr_P1_logadd.rds")
nosurp_Agr_P2 <- readRDS("Agr_logadd_prior1/brm_predicted_nosurp_Agr_P2_logadd.rds")



posterior_samp <- posterior_samples(emp_Agr_P0)
randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(pGram|Intercept)',colnames(posterior_samp))]
saveRDS(randomslope_names,"Agr_logadd_prior1/Agr_randomslopesnames.rds")
randomslope_names <- readRDS("Agr_logadd_prior1/Agr_randomslopesnames.rds")

emp_Agr_P0_posterior_samp <- posterior_samples(emp_Agr_P0, fixed=TRUE, pars=
                                              c("b_Intercept","b_pGram.coded",randomslope_names))
emp_Agr_P1_posterior_samp <- posterior_samples(emp_Agr_P1, fixed=TRUE, pars=
                                                 c("b_Intercept","b_pGram.coded",randomslope_names))
emp_Agr_P2_posterior_samp <- posterior_samples(emp_Agr_P2, fixed=TRUE, pars=
                                                 c("b_Intercept","b_pGram.coded",randomslope_names))
lstm_Agr_P0_posterior_samp <- posterior_samples(lstm_Agr_P0, fixed=TRUE, pars=
                                              c("b_Intercept","b_pGram.coded",randomslope_names))
lstm_Agr_P1_posterior_samp <- posterior_samples(lstm_Agr_P1, fixed=TRUE, pars=
                                                  c("b_Intercept","b_pGram.coded",randomslope_names))
lstm_Agr_P2_posterior_samp <- posterior_samples(lstm_Agr_P2, fixed=TRUE, pars=
                                                  c("b_Intercept","b_pGram.coded",randomslope_names))
gpt2_Agr_P0_posterior_samp <- posterior_samples(gpt2_Agr_P0, fixed=TRUE, pars=
                                              c("b_Intercept","b_pGram.coded",randomslope_names))
gpt2_Agr_P1_posterior_samp <- posterior_samples(gpt2_Agr_P1, fixed=TRUE, pars=
                                                  c("b_Intercept","b_pGram.coded",randomslope_names))
gpt2_Agr_P2_posterior_samp <- posterior_samples(gpt2_Agr_P2, fixed=TRUE, pars=
                                                  c("b_Intercept","b_pGram.coded",randomslope_names))
nosurp_Agr_P0_posterior_samp <- posterior_samples(nosurp_Agr_P0, fixed=TRUE, pars=
                                              c("b_Intercept","b_pGram.coded",randomslope_names))
nosurp_Agr_P1_posterior_samp <- posterior_samples(nosurp_Agr_P1, fixed=TRUE, pars=
                                                    c("b_Intercept","b_pGram.coded",randomslope_names))
nosurp_Agr_P2_posterior_samp <- posterior_samples(nosurp_Agr_P2, fixed=TRUE, pars=
                                                    c("b_Intercept","b_pGram.coded",randomslope_names))

rm(emp_Agr_P0,emp_Agr_P1,emp_Agr_P2, lstm_Agr_P0, lstm_Agr_P1, lstm_Agr_P2, gpt2_Agr_P0, gpt2_Agr_P1, gpt2_Agr_P2,
   nosurp_Agr_P0, nosurp_Agr_P1, nosurp_Agr_P2, posterior_samp)
saveRDS(emp_Agr_P0_posterior_samp,"Agr_logadd_prior1/emp_Agr_P0_posterior_samp.rds")
saveRDS(emp_Agr_P1_posterior_samp,"Agr_logadd_prior1/emp_Agr_P1_posterior_samp.rds")
saveRDS(emp_Agr_P2_posterior_samp,"Agr_logadd_prior1/emp_Agr_P2_posterior_samp.rds")
saveRDS(lstm_Agr_P0_posterior_samp,"Agr_logadd_prior1/lstm_Agr_P0_posterior_samp.rds")
saveRDS(lstm_Agr_P1_posterior_samp,"Agr_logadd_prior1/lstm_Agr_P1_posterior_samp.rds")
saveRDS(lstm_Agr_P2_posterior_samp,"Agr_logadd_prior1/lstm_Agr_P2_posterior_samp.rds")
saveRDS(gpt2_Agr_P0_posterior_samp,"Agr_logadd_prior1/gpt2_Agr_P0_posterior_samp.rds")
saveRDS(gpt2_Agr_P1_posterior_samp,"Agr_logadd_prior1/gpt2_Agr_P1_posterior_samp.rds")
saveRDS(gpt2_Agr_P2_posterior_samp,"Agr_logadd_prior1/gpt2_Agr_P2_posterior_samp.rds")
saveRDS(nosurp_Agr_P0_posterior_samp,"Agr_logadd_prior1/nosurp_Agr_P0_posterior_samp.rds")
saveRDS(nosurp_Agr_P1_posterior_samp,"Agr_logadd_prior1/nosurp_Agr_P1_posterior_samp.rds")
saveRDS(nosurp_Agr_P2_posterior_samp,"Agr_logadd_prior1/nosurp_Agr_P2_posterior_samp.rds")


emp_Agr_P0_posterior_samp <- readRDS("Agr_logadd_prior1/emp_Agr_P0_posterior_samp.rds")
emp_Agr_P1_posterior_samp <- readRDS("Agr_logadd_prior1/emp_Agr_P1_posterior_samp.rds")
emp_Agr_P2_posterior_samp <- readRDS("Agr_logadd_prior1/emp_Agr_P2_posterior_samp.rds")
lstm_Agr_P0_posterior_samp <- readRDS("Agr_logadd_prior1/lstm_Agr_P0_posterior_samp.rds")
lstm_Agr_P1_posterior_samp <- readRDS("Agr_logadd_prior1/lstm_Agr_P1_posterior_samp.rds")
lstm_Agr_P2_posterior_samp <- readRDS("Agr_logadd_prior1/lstm_Agr_P2_posterior_samp.rds")
gpt2_Agr_P0_posterior_samp <- readRDS("Agr_logadd_prior1/gpt2_Agr_P0_posterior_samp.rds")
gpt2_Agr_P1_posterior_samp <- readRDS("Agr_logadd_prior1/gpt2_Agr_P1_posterior_samp.rds")
gpt2_Agr_P2_posterior_samp <- readRDS("Agr_logadd_prior1/gpt2_Agr_P2_posterior_samp.rds")
nosurp_Agr_P0_posterior_samp <- readRDS("Agr_logadd_prior1/nosurp_Agr_P0_posterior_samp.rds")
nosurp_Agr_P1_posterior_samp <- readRDS("Agr_logadd_prior1/nosurp_Agr_P1_posterior_samp.rds")
nosurp_Agr_P2_posterior_samp <- readRDS("Agr_logadd_prior1/nosurp_Agr_P2_posterior_samp.rds")





posterior_emp_Agr_P0 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=0)
posterior_emp_Agr_P1 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=1)
posterior_emp_Agr_P2 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=2)
posterior_lstm_Agr_P0 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=0)
posterior_lstm_Agr_P1 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=1)
posterior_lstm_Agr_P2 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=2)
posterior_gpt2_Agr_P0 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=0)
posterior_gpt2_Agr_P1 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=1)
posterior_gpt2_Agr_P2 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=2)
posterior_nosurp_Agr_P0 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=0)
posterior_nosurp_Agr_P1 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=1)
posterior_nosurp_Agr_P2 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agr",ROI=2)


posterior_emp_Agreed_P0 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agreed",ROI=0)
posterior_emp_Agreed_P1 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agreed",ROI=1)
posterior_emp_Agreed_P2 <- data.frame(mean=rep(NA,18),SE=rep(NA,18),upper=rep(NA,18),lower=rep(NA,18),item=c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24),EOI="Agreed",ROI=2)




ncols <- ncol(emp_Agr_P0_posterior_samp)
emp_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- exp(emp_Agr_P0_posterior_samp[,1]+emp_Agr_P0_posterior_samp[,(1+2):(18+2)]+emp_Agr_P0_posterior_samp[,2]+emp_Agr_P0_posterior_samp[,(1+20):(18+20)])-exp(emp_Agr_P0_posterior_samp[,1]+emp_Agr_P0_posterior_samp[,(1+2):(18+2)])
emp_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- exp(emp_Agr_P1_posterior_samp[,1]+emp_Agr_P1_posterior_samp[,(1+2):(18+2)]+emp_Agr_P1_posterior_samp[,2]+emp_Agr_P1_posterior_samp[,(1+20):(18+20)])-exp(emp_Agr_P1_posterior_samp[,1]+emp_Agr_P1_posterior_samp[,(1+2):(18+2)])
emp_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- exp(emp_Agr_P2_posterior_samp[,1]+emp_Agr_P2_posterior_samp[,(1+2):(18+2)]+emp_Agr_P2_posterior_samp[,2]+emp_Agr_P2_posterior_samp[,(1+20):(18+20)])-exp(emp_Agr_P2_posterior_samp[,1]+emp_Agr_P2_posterior_samp[,(1+2):(18+2)])
emp_Agr_P0_posterior_samp[,(19+ncols):(36+ncols)] <- exp(emp_Agr_P0_posterior_samp[,1]+emp_Agr_P0_posterior_samp[,(1+2):(18+2)])
emp_Agr_P1_posterior_samp[,(19+ncols):(36+ncols)] <- exp(emp_Agr_P1_posterior_samp[,1]+emp_Agr_P1_posterior_samp[,(1+2):(18+2)])
emp_Agr_P2_posterior_samp[,(19+ncols):(36+ncols)] <- exp(emp_Agr_P2_posterior_samp[,1]+emp_Agr_P2_posterior_samp[,(1+2):(18+2)])
lstm_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- exp(lstm_Agr_P0_posterior_samp[,1]+lstm_Agr_P0_posterior_samp[,(1+2):(18+2)]+lstm_Agr_P0_posterior_samp[,2]+lstm_Agr_P0_posterior_samp[,(1+20):(18+20)])-exp(lstm_Agr_P0_posterior_samp[,1]+lstm_Agr_P0_posterior_samp[,(1+2):(18+2)])
lstm_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- exp(lstm_Agr_P1_posterior_samp[,1]+lstm_Agr_P1_posterior_samp[,(1+2):(18+2)]+lstm_Agr_P1_posterior_samp[,2]+lstm_Agr_P1_posterior_samp[,(1+20):(18+20)])-exp(lstm_Agr_P1_posterior_samp[,1]+lstm_Agr_P1_posterior_samp[,(1+2):(18+2)])
lstm_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- exp(lstm_Agr_P2_posterior_samp[,1]+lstm_Agr_P2_posterior_samp[,(1+2):(18+2)]+lstm_Agr_P2_posterior_samp[,2]+lstm_Agr_P2_posterior_samp[,(1+20):(18+20)])-exp(lstm_Agr_P2_posterior_samp[,1]+lstm_Agr_P2_posterior_samp[,(1+2):(18+2)])
gpt2_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- exp(gpt2_Agr_P0_posterior_samp[,1]+gpt2_Agr_P0_posterior_samp[,(1+2):(18+2)]+gpt2_Agr_P0_posterior_samp[,2]+gpt2_Agr_P0_posterior_samp[,(1+20):(18+20)])-exp(gpt2_Agr_P0_posterior_samp[,1]+gpt2_Agr_P0_posterior_samp[,(1+2):(18+2)])
gpt2_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- exp(gpt2_Agr_P1_posterior_samp[,1]+gpt2_Agr_P1_posterior_samp[,(1+2):(18+2)]+gpt2_Agr_P1_posterior_samp[,2]+gpt2_Agr_P1_posterior_samp[,(1+20):(18+20)])-exp(gpt2_Agr_P1_posterior_samp[,1]+gpt2_Agr_P1_posterior_samp[,(1+2):(18+2)])
gpt2_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- exp(gpt2_Agr_P2_posterior_samp[,1]+gpt2_Agr_P2_posterior_samp[,(1+2):(18+2)]+gpt2_Agr_P2_posterior_samp[,2]+gpt2_Agr_P2_posterior_samp[,(1+20):(18+20)])-exp(gpt2_Agr_P2_posterior_samp[,1]+gpt2_Agr_P2_posterior_samp[,(1+2):(18+2)])
nosurp_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- exp(nosurp_Agr_P0_posterior_samp[,1]+nosurp_Agr_P0_posterior_samp[,(1+2):(18+2)]+nosurp_Agr_P0_posterior_samp[,2]+nosurp_Agr_P0_posterior_samp[,(1+20):(18+20)])-exp(nosurp_Agr_P0_posterior_samp[,1]+nosurp_Agr_P0_posterior_samp[,(1+2):(18+2)])
nosurp_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- exp(nosurp_Agr_P1_posterior_samp[,1]+nosurp_Agr_P1_posterior_samp[,(1+2):(18+2)]+nosurp_Agr_P1_posterior_samp[,2]+nosurp_Agr_P1_posterior_samp[,(1+20):(18+20)])-exp(nosurp_Agr_P1_posterior_samp[,1]+nosurp_Agr_P1_posterior_samp[,(1+2):(18+2)])
nosurp_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- exp(nosurp_Agr_P2_posterior_samp[,1]+nosurp_Agr_P2_posterior_samp[,(1+2):(18+2)]+nosurp_Agr_P2_posterior_samp[,2]+nosurp_Agr_P2_posterior_samp[,(1+20):(18+20)])-exp(nosurp_Agr_P2_posterior_samp[,1]+nosurp_Agr_P2_posterior_samp[,(1+2):(18+2)])





for(i in 1:18){
  posterior_emp_Agr_P0[i,]$mean <- mean(emp_Agr_P0_posterior_samp[,i+ncols])
  posterior_emp_Agr_P0[i,]$SE <- sd(emp_Agr_P0_posterior_samp[,i+ncols])
  posterior_emp_Agr_P0[i,]$upper <- quantile(emp_Agr_P0_posterior_samp[,i+ncols],0.975)
  posterior_emp_Agr_P0[i,]$lower <- quantile(emp_Agr_P0_posterior_samp[,i+ncols],0.025)
  posterior_emp_Agr_P1[i,]$mean <- mean(emp_Agr_P1_posterior_samp[,i+ncols])
  posterior_emp_Agr_P1[i,]$SE <- sd(emp_Agr_P1_posterior_samp[,i+ncols])
  posterior_emp_Agr_P1[i,]$upper <- quantile(emp_Agr_P1_posterior_samp[,i+ncols],0.975)
  posterior_emp_Agr_P1[i,]$lower <- quantile(emp_Agr_P1_posterior_samp[,i+ncols],0.025)
  posterior_emp_Agr_P2[i,]$mean <- mean(emp_Agr_P2_posterior_samp[,i+ncols])
  posterior_emp_Agr_P2[i,]$SE <- sd(emp_Agr_P2_posterior_samp[,i+ncols])
  posterior_emp_Agr_P2[i,]$upper <- quantile(emp_Agr_P2_posterior_samp[,i+ncols],0.975)
  posterior_emp_Agr_P2[i,]$lower <- quantile(emp_Agr_P2_posterior_samp[,i+ncols],0.025)
  posterior_emp_Agreed_P0[i,]$mean <- mean(emp_Agr_P0_posterior_samp[,i+ncols+18])
  posterior_emp_Agreed_P0[i,]$SE <- sd(emp_Agr_P0_posterior_samp[,i+ncols+18])
  posterior_emp_Agreed_P0[i,]$upper <- quantile(emp_Agr_P0_posterior_samp[,i+ncols+18],0.975)
  posterior_emp_Agreed_P0[i,]$lower <- quantile(emp_Agr_P0_posterior_samp[,i+ncols+18],0.025)
  posterior_emp_Agreed_P1[i,]$mean <- mean(emp_Agr_P1_posterior_samp[,i+ncols+18])
  posterior_emp_Agreed_P1[i,]$SE <- sd(emp_Agr_P1_posterior_samp[,i+ncols+18])
  posterior_emp_Agreed_P1[i,]$upper <- quantile(emp_Agr_P1_posterior_samp[,i+ncols+18],0.975)
  posterior_emp_Agreed_P1[i,]$lower <- quantile(emp_Agr_P1_posterior_samp[,i+ncols+18],0.025)
  posterior_emp_Agreed_P2[i,]$mean <- mean(emp_Agr_P2_posterior_samp[,i+ncols+18])
  posterior_emp_Agreed_P2[i,]$SE <- sd(emp_Agr_P2_posterior_samp[,i+ncols+18])
  posterior_emp_Agreed_P2[i,]$upper <- quantile(emp_Agr_P2_posterior_samp[,i+ncols+18],0.975)
  posterior_emp_Agreed_P2[i,]$lower <- quantile(emp_Agr_P2_posterior_samp[,i+ncols+18],0.025)
  posterior_lstm_Agr_P0[i,]$mean <- mean(lstm_Agr_P0_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P0[i,]$SE <- sd(lstm_Agr_P0_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P0[i,]$upper <- quantile(lstm_Agr_P0_posterior_samp[,i+ncols],0.975)
  posterior_lstm_Agr_P0[i,]$lower <- quantile(lstm_Agr_P0_posterior_samp[,i+ncols],0.025)
  posterior_lstm_Agr_P1[i,]$mean <- mean(lstm_Agr_P1_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P1[i,]$SE <- sd(lstm_Agr_P1_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P1[i,]$upper <- quantile(lstm_Agr_P1_posterior_samp[,i+ncols],0.975)
  posterior_lstm_Agr_P1[i,]$lower <- quantile(lstm_Agr_P1_posterior_samp[,i+ncols],0.025)
  posterior_lstm_Agr_P2[i,]$mean <- mean(lstm_Agr_P2_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P2[i,]$SE <- sd(lstm_Agr_P2_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P2[i,]$upper <- quantile(lstm_Agr_P2_posterior_samp[,i+ncols],0.975)
  posterior_lstm_Agr_P2[i,]$lower <- quantile(lstm_Agr_P2_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_Agr_P0[i,]$mean <- mean(gpt2_Agr_P0_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P0[i,]$SE <- sd(gpt2_Agr_P0_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P0[i,]$upper <- quantile(gpt2_Agr_P0_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_Agr_P0[i,]$lower <- quantile(gpt2_Agr_P0_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_Agr_P1[i,]$mean <- mean(gpt2_Agr_P1_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P1[i,]$SE <- sd(gpt2_Agr_P1_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P1[i,]$upper <- quantile(gpt2_Agr_P1_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_Agr_P1[i,]$lower <- quantile(gpt2_Agr_P1_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_Agr_P2[i,]$mean <- mean(gpt2_Agr_P2_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P2[i,]$SE <- sd(gpt2_Agr_P2_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P2[i,]$upper <- quantile(gpt2_Agr_P2_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_Agr_P2[i,]$lower <- quantile(gpt2_Agr_P2_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_Agr_P0[i,]$mean <- mean(nosurp_Agr_P0_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P0[i,]$SE <- sd(nosurp_Agr_P0_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P0[i,]$upper <- quantile(nosurp_Agr_P0_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_Agr_P0[i,]$lower <- quantile(nosurp_Agr_P0_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_Agr_P1[i,]$mean <- mean(nosurp_Agr_P1_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P1[i,]$SE <- sd(nosurp_Agr_P1_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P1[i,]$upper <- quantile(nosurp_Agr_P1_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_Agr_P1[i,]$lower <- quantile(nosurp_Agr_P1_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_Agr_P2[i,]$mean <- mean(nosurp_Agr_P2_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P2[i,]$SE <- sd(nosurp_Agr_P2_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P2[i,]$upper <- quantile(nosurp_Agr_P2_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_Agr_P2[i,]$lower <- quantile(nosurp_Agr_P2_posterior_samp[,i+ncols],0.025)
}


#P0
sampled_correlations_P0 <- data.frame(Correlation=rep(NA,3000),EOI=rep("Agr",3000),model=rep(c("lstm","gpt2","nosurp"),1000),ROI=0)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,18),lstm=rep(NA,18),gpt2=rep(NA,18),nosurp=rep(NA,18),EOI=rep("Agr",18))
  for(j in 1:18){
    posterior_onesampleeachitem[j,1] <- sample(emp_Agr_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_Agr_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_Agr_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_Agr_P0_posterior_samp[,j+ncols],1)
  }
  sampled_correlations_P0[((i-1)*3+1):((i-1)*3+3),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,4])$estimate)
}
#P0


#P1
sampled_correlations_P1 <- data.frame(Correlation=rep(NA,3000),EOI=rep("Agr",3000),model=rep(c("lstm","gpt2","nosurp"),1000),ROI=1)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,18),lstm=rep(NA,18),gpt2=rep(NA,18),nosurp=rep(NA,18),EOI=rep("Agr",18))
  for(j in 1:18){
    posterior_onesampleeachitem[j,1] <- sample(emp_Agr_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_Agr_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_Agr_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_Agr_P1_posterior_samp[,j+ncols],1)
  }
  sampled_correlations_P1[((i-1)*3+1):((i-1)*3+3),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,4])$estimate)
}
#P1


#P2
sampled_correlations_P2 <- data.frame(Correlation=rep(NA,3000),EOI=rep("Agr",3000),model=rep(c("lstm","gpt2","nosurp"),1000),ROI=2)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,18),lstm=rep(NA,18),gpt2=rep(NA,18),nosurp=rep(NA,18),EOI=rep("Agr",18))
  for(j in 1:18){
    posterior_onesampleeachitem[j,1] <- sample(emp_Agr_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_Agr_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_Agr_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_Agr_P2_posterior_samp[,j+ncols],1)
  }
  sampled_correlations_P2[((i-1)*3+1):((i-1)*3+3),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:18,1],posterior_onesampleeachitem[1:18,4])$estimate)
}
#P2




for(i in unique(sampled_correlations_P0$EOI)){
  for(j in unique(sampled_correlations_P0$model)){
    hist(sampled_correlations_P0$Correlation[sampled_correlations_P0$EOI==i&sampled_correlations_P0$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P0$Correlation,"Agr_logadd_prior1/sampled_correlations_P0_Agr.rds")
sampled_correlations_maxregion <- sampled_correlations_P1
for(i in unique(sampled_correlations_maxregion$EOI)){
  for(j in unique(sampled_correlations_maxregion$model)){
    hist(sampled_correlations_maxregion$Correlation[sampled_correlations_maxregion$EOI==i&sampled_correlations_maxregion$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_maxregion,"Agr_logadd_prior1/sampled_correlations_P1_Agr.rds")
for(i in unique(sampled_correlations_P2$EOI)){
  for(j in unique(sampled_correlations_P2$model)){
    hist(sampled_correlations_P2$Correlation[sampled_correlations_P2$EOI==i&sampled_correlations_P2$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P2$Correlation,"Agr_logadd_prior1/sampled_correlations_P2_Agr.rds")

