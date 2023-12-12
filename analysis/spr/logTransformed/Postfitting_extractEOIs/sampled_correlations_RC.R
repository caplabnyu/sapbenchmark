library(lme4)
library(tidyr)
library(stringr)
library(posterior)
library(tidybayes)
library(tidyverse)
library(dplyr)
library(brms)


emp_RC_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_emp_prior1.rds")
emp_RC_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_emp_prior1.rds")
emp_RC_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_emp_prior1.rds")
lstm_RC_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_pred_lstm_prior1_logadd.rds")
lstm_RC_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_pred_lstm_prior1_logadd.rds")
lstm_RC_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_pred_lstm_prior1_logadd.rds")
gpt2_RC_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_pred_gpt2_prior1_logadd.rds")
gpt2_RC_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_pred_gpt2_prior1_logadd.rds")
gpt2_RC_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_pred_gpt2_prior1_logadd.rds")
nosurp_RC_P0 <- readRDS("RC_logadd_prior1/fit_verb_bayes_pred_nosurp_prior1_logadd.rds")
nosurp_RC_P1 <- readRDS("RC_logadd_prior1/fit_det_bayes_pred_nosurp_prior1_logadd.rds")
nosurp_RC_P2 <- readRDS("RC_logadd_prior1/fit_noun_bayes_pred_nosurp_prior1_logadd.rds")



posterior_samp <- posterior_samples(emp_RC_P0)
randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(Type_num|Intercept)',colnames(posterior_samp))]
saveRDS(randomslope_names,"RC_logadd_prior1/RC_randomslopesnames.rds")
randomslope_names <- readRDS("RC_logadd_prior1/RC_randomslopesnames.rds")

emp_RC_P0_posterior_samp <- posterior_samples(emp_RC_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
lstm_RC_P0_posterior_samp <- posterior_samples(lstm_RC_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
gpt2_RC_P0_posterior_samp <- posterior_samples(gpt2_RC_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
nosurp_RC_P0_posterior_samp <- posterior_samples(nosurp_RC_P0, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
emp_RC_P1_posterior_samp <- posterior_samples(emp_RC_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
lstm_RC_P1_posterior_samp <- posterior_samples(lstm_RC_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
gpt2_RC_P1_posterior_samp <- posterior_samples(gpt2_RC_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
nosurp_RC_P1_posterior_samp <- posterior_samples(nosurp_RC_P1, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
emp_RC_P2_posterior_samp <- posterior_samples(emp_RC_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
lstm_RC_P2_posterior_samp <- posterior_samples(lstm_RC_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
gpt2_RC_P2_posterior_samp <- posterior_samples(gpt2_RC_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))
nosurp_RC_P2_posterior_samp <- posterior_samples(nosurp_RC_P2, fixed=TRUE, pars=
                                                c("b_Intercept","b_Type_num",randomslope_names))

rm(emp_RC_P0, lstm_RC_P0, gpt2_RC_P0, nosurp_RC_P0, emp_RC_P1, lstm_RC_P1,
gpt2_RC_P1, nosurp_RC_P1, emp_RC_P2, lstm_RC_P2, gpt2_RC_P2, nosurp_RC_P2,posterior_samp)
saveRDS(emp_RC_P0_posterior_samp,"RC_logadd_prior1/emp_RC_P0_posterior_samp.rds")
saveRDS(lstm_RC_P0_posterior_samp,"RC_logadd_prior1/lstm_RC_P0_posterior_samp.rds")
saveRDS(gpt2_RC_P0_posterior_samp,"RC_logadd_prior1/gpt2_RC_P0_posterior_samp.rds")
saveRDS(nosurp_RC_P0_posterior_samp,"RC_logadd_prior1/nosurp_RC_P0_posterior_samp.rds")
saveRDS(emp_RC_P1_posterior_samp,"RC_logadd_prior1/emp_RC_P1_posterior_samp.rds")
saveRDS(lstm_RC_P1_posterior_samp,"RC_logadd_prior1/lstm_RC_P1_posterior_samp.rds")
saveRDS(gpt2_RC_P1_posterior_samp,"RC_logadd_prior1/gpt2_RC_P1_posterior_samp.rds")
saveRDS(nosurp_RC_P1_posterior_samp,"RC_logadd_prior1/nosurp_RC_P1_posterior_samp.rds")
saveRDS(emp_RC_P2_posterior_samp,"RC_logadd_prior1/emp_RC_P2_posterior_samp.rds")
saveRDS(lstm_RC_P2_posterior_samp,"RC_logadd_prior1/lstm_RC_P2_posterior_samp.rds")
saveRDS(gpt2_RC_P2_posterior_samp,"RC_logadd_prior1/gpt2_RC_P2_posterior_samp.rds")
saveRDS(nosurp_RC_P2_posterior_samp,"RC_logadd_prior1/nosurp_RC_P2_posterior_samp.rds")

emp_RC_P0_posterior_samp <- readRDS("RC_logadd_prior1/emp_RC_P0_posterior_samp.rds")
lstm_RC_P0_posterior_samp <- readRDS("RC_logadd_prior1/lstm_RC_P0_posterior_samp.rds")
gpt2_RC_P0_posterior_samp <- readRDS("RC_logadd_prior1/gpt2_RC_P0_posterior_samp.rds")
nosurp_RC_P0_posterior_samp <- readRDS("RC_logadd_prior1/nosurp_RC_P0_posterior_samp.rds")
emp_RC_P1_posterior_samp <- readRDS("RC_logadd_prior1/emp_RC_P1_posterior_samp.rds")
lstm_RC_P1_posterior_samp <- readRDS("RC_logadd_prior1/lstm_RC_P1_posterior_samp.rds")
gpt2_RC_P1_posterior_samp <- readRDS("RC_logadd_prior1/gpt2_RC_P1_posterior_samp.rds")
nosurp_RC_P1_posterior_samp <- readRDS("RC_logadd_prior1/nosurp_RC_P1_posterior_samp.rds")
emp_RC_P2_posterior_samp <- readRDS("RC_logadd_prior1/emp_RC_P2_posterior_samp.rds")
lstm_RC_P2_posterior_samp <- readRDS("RC_logadd_prior1/lstm_RC_P2_posterior_samp.rds")
gpt2_RC_P2_posterior_samp <- readRDS("RC_logadd_prior1/gpt2_RC_P2_posterior_samp.rds")
nosurp_RC_P2_posterior_samp <- readRDS("RC_logadd_prior1/nosurp_RC_P2_posterior_samp.rds")




posterior_emp_RC_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=0)
posterior_emp_RC_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=1)
posterior_emp_RC_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=2)
posterior_lstm_RC_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=0)
posterior_lstm_RC_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=1)
posterior_lstm_RC_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=2)
posterior_gpt2_RC_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=0)
posterior_gpt2_RC_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=1)
posterior_gpt2_RC_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=2)
posterior_nosurp_RC_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=0)
posterior_nosurp_RC_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=1)
posterior_nosurp_RC_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="RC",ROI=2)

posterior_emp_SRC_P0 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="SRC",ROI=0)
posterior_emp_SRC_P1 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="SRC",ROI=1)
posterior_emp_SRC_P2 <- data.frame(mean=rep(NA,24),SE=rep(NA,24),upper=rep(NA,24),lower=rep(NA,24),item=25:48,EOI="SRC",ROI=2)


ncols <- ncol(emp_RC_P0_posterior_samp)
emp_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_RC_P0_posterior_samp[,1]+emp_RC_P0_posterior_samp[,(1+2):(24+2)]+emp_RC_P0_posterior_samp[,2]+emp_RC_P0_posterior_samp[,(1+26):(24+26)])-exp(emp_RC_P0_posterior_samp[,1]+emp_RC_P0_posterior_samp[,(1+2):(24+2)])
emp_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_RC_P1_posterior_samp[,1]+emp_RC_P1_posterior_samp[,(1+2):(24+2)]+emp_RC_P1_posterior_samp[,2]+emp_RC_P1_posterior_samp[,(1+26):(24+26)])-exp(emp_RC_P1_posterior_samp[,1]+emp_RC_P1_posterior_samp[,(1+2):(24+2)])
emp_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(emp_RC_P2_posterior_samp[,1]+emp_RC_P2_posterior_samp[,(1+2):(24+2)]+emp_RC_P2_posterior_samp[,2]+emp_RC_P2_posterior_samp[,(1+26):(24+26)])-exp(emp_RC_P2_posterior_samp[,1]+emp_RC_P2_posterior_samp[,(1+2):(24+2)])
emp_RC_P0_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_RC_P0_posterior_samp[,1]+emp_RC_P0_posterior_samp[,(1+2):(24+2)])
emp_RC_P1_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_RC_P1_posterior_samp[,1]+emp_RC_P1_posterior_samp[,(1+2):(24+2)])
emp_RC_P2_posterior_samp[,(25+ncols):(48+ncols)] <- exp(emp_RC_P2_posterior_samp[,1]+emp_RC_P2_posterior_samp[,(1+2):(24+2)])
lstm_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_RC_P0_posterior_samp[,1]+lstm_RC_P0_posterior_samp[,(1+2):(24+2)]+lstm_RC_P0_posterior_samp[,2]+lstm_RC_P0_posterior_samp[,(1+26):(24+26)])-exp(lstm_RC_P0_posterior_samp[,1]+lstm_RC_P0_posterior_samp[,(1+2):(24+2)])
lstm_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_RC_P1_posterior_samp[,1]+lstm_RC_P1_posterior_samp[,(1+2):(24+2)]+lstm_RC_P1_posterior_samp[,2]+lstm_RC_P1_posterior_samp[,(1+26):(24+26)])-exp(lstm_RC_P1_posterior_samp[,1]+lstm_RC_P1_posterior_samp[,(1+2):(24+2)])
lstm_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(lstm_RC_P2_posterior_samp[,1]+lstm_RC_P2_posterior_samp[,(1+2):(24+2)]+lstm_RC_P2_posterior_samp[,2]+lstm_RC_P2_posterior_samp[,(1+26):(24+26)])-exp(lstm_RC_P2_posterior_samp[,1]+lstm_RC_P2_posterior_samp[,(1+2):(24+2)])
gpt2_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_RC_P0_posterior_samp[,1]+gpt2_RC_P0_posterior_samp[,(1+2):(24+2)]+gpt2_RC_P0_posterior_samp[,2]+gpt2_RC_P0_posterior_samp[,(1+26):(24+26)])-exp(gpt2_RC_P0_posterior_samp[,1]+gpt2_RC_P0_posterior_samp[,(1+2):(24+2)])
gpt2_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_RC_P1_posterior_samp[,1]+gpt2_RC_P1_posterior_samp[,(1+2):(24+2)]+gpt2_RC_P1_posterior_samp[,2]+gpt2_RC_P1_posterior_samp[,(1+26):(24+26)])-exp(gpt2_RC_P1_posterior_samp[,1]+gpt2_RC_P1_posterior_samp[,(1+2):(24+2)])
gpt2_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(gpt2_RC_P2_posterior_samp[,1]+gpt2_RC_P2_posterior_samp[,(1+2):(24+2)]+gpt2_RC_P2_posterior_samp[,2]+gpt2_RC_P2_posterior_samp[,(1+26):(24+26)])-exp(gpt2_RC_P2_posterior_samp[,1]+gpt2_RC_P2_posterior_samp[,(1+2):(24+2)])
nosurp_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_RC_P0_posterior_samp[,1]+nosurp_RC_P0_posterior_samp[,(1+2):(24+2)]+nosurp_RC_P0_posterior_samp[,2]+nosurp_RC_P0_posterior_samp[,(1+26):(24+26)])-exp(nosurp_RC_P0_posterior_samp[,1]+nosurp_RC_P0_posterior_samp[,(1+2):(24+2)])
nosurp_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_RC_P1_posterior_samp[,1]+nosurp_RC_P1_posterior_samp[,(1+2):(24+2)]+nosurp_RC_P1_posterior_samp[,2]+nosurp_RC_P1_posterior_samp[,(1+26):(24+26)])-exp(nosurp_RC_P1_posterior_samp[,1]+nosurp_RC_P1_posterior_samp[,(1+2):(24+2)])
nosurp_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- exp(nosurp_RC_P2_posterior_samp[,1]+nosurp_RC_P2_posterior_samp[,(1+2):(24+2)]+nosurp_RC_P2_posterior_samp[,2]+nosurp_RC_P2_posterior_samp[,(1+26):(24+26)])-exp(nosurp_RC_P2_posterior_samp[,1]+nosurp_RC_P2_posterior_samp[,(1+2):(24+2)])





for(i in 1:24){
  posterior_emp_RC_P0[i,]$mean <- mean(emp_RC_P0_posterior_samp[,i+ncols])
  posterior_emp_RC_P0[i,]$SE <- sd(emp_RC_P0_posterior_samp[,i+ncols])
  posterior_emp_RC_P0[i,]$upper <- quantile(emp_RC_P0_posterior_samp[,i+ncols],0.975)
  posterior_emp_RC_P0[i,]$lower <- quantile(emp_RC_P0_posterior_samp[,i+ncols],0.025)
  posterior_emp_RC_P1[i,]$mean <- mean(emp_RC_P1_posterior_samp[,i+ncols])
  posterior_emp_RC_P1[i,]$SE <- sd(emp_RC_P1_posterior_samp[,i+ncols])
  posterior_emp_RC_P1[i,]$upper <- quantile(emp_RC_P1_posterior_samp[,i+ncols],0.975)
  posterior_emp_RC_P1[i,]$lower <- quantile(emp_RC_P1_posterior_samp[,i+ncols],0.025)
  posterior_emp_RC_P2[i,]$mean <- mean(emp_RC_P2_posterior_samp[,i+ncols])
  posterior_emp_RC_P2[i,]$SE <- sd(emp_RC_P2_posterior_samp[,i+ncols])
  posterior_emp_RC_P2[i,]$upper <- quantile(emp_RC_P2_posterior_samp[,i+ncols],0.975)
  posterior_emp_RC_P2[i,]$lower <- quantile(emp_RC_P2_posterior_samp[,i+ncols],0.025)
  posterior_emp_SRC_P0[i,]$mean <- mean(emp_RC_P0_posterior_samp[,i+ncols+24])
  posterior_emp_SRC_P0[i,]$SE <- sd(emp_RC_P0_posterior_samp[,i+ncols+24])
  posterior_emp_SRC_P0[i,]$upper <- quantile(emp_RC_P0_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_SRC_P0[i,]$lower <- quantile(emp_RC_P0_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_SRC_P1[i,]$mean <- mean(emp_RC_P1_posterior_samp[,i+ncols+24])
  posterior_emp_SRC_P1[i,]$SE <- sd(emp_RC_P1_posterior_samp[,i+ncols+24])
  posterior_emp_SRC_P1[i,]$upper <- quantile(emp_RC_P1_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_SRC_P1[i,]$lower <- quantile(emp_RC_P1_posterior_samp[,i+ncols+24],0.025)
  posterior_emp_SRC_P2[i,]$mean <- mean(emp_RC_P2_posterior_samp[,i+ncols+24])
  posterior_emp_SRC_P2[i,]$SE <- sd(emp_RC_P2_posterior_samp[,i+ncols+24])
  posterior_emp_SRC_P2[i,]$upper <- quantile(emp_RC_P2_posterior_samp[,i+ncols+24],0.975)
  posterior_emp_SRC_P2[i,]$lower <- quantile(emp_RC_P2_posterior_samp[,i+ncols+24],0.025)
  posterior_lstm_RC_P0[i,]$mean <- mean(lstm_RC_P0_posterior_samp[,i+ncols])
  posterior_lstm_RC_P0[i,]$SE <- sd(lstm_RC_P0_posterior_samp[,i+ncols])
  posterior_lstm_RC_P0[i,]$upper <- quantile(lstm_RC_P0_posterior_samp[,i+ncols],0.975)
  posterior_lstm_RC_P0[i,]$lower <- quantile(lstm_RC_P0_posterior_samp[,i+ncols],0.025)
  posterior_lstm_RC_P1[i,]$mean <- mean(lstm_RC_P1_posterior_samp[,i+ncols])
  posterior_lstm_RC_P1[i,]$SE <- sd(lstm_RC_P1_posterior_samp[,i+ncols])
  posterior_lstm_RC_P1[i,]$upper <- quantile(lstm_RC_P1_posterior_samp[,i+ncols],0.975)
  posterior_lstm_RC_P1[i,]$lower <- quantile(lstm_RC_P1_posterior_samp[,i+ncols],0.025)
  posterior_lstm_RC_P2[i,]$mean <- mean(lstm_RC_P2_posterior_samp[,i+ncols])
  posterior_lstm_RC_P2[i,]$SE <- sd(lstm_RC_P2_posterior_samp[,i+ncols])
  posterior_lstm_RC_P2[i,]$upper <- quantile(lstm_RC_P2_posterior_samp[,i+ncols],0.975)
  posterior_lstm_RC_P2[i,]$lower <- quantile(lstm_RC_P2_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_RC_P0[i,]$mean <- mean(gpt2_RC_P0_posterior_samp[,i+ncols])
  posterior_gpt2_RC_P0[i,]$SE <- sd(gpt2_RC_P0_posterior_samp[,i+ncols])
  posterior_gpt2_RC_P0[i,]$upper <- quantile(gpt2_RC_P0_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_RC_P0[i,]$lower <- quantile(gpt2_RC_P0_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_RC_P1[i,]$mean <- mean(gpt2_RC_P1_posterior_samp[,i+ncols])
  posterior_gpt2_RC_P1[i,]$SE <- sd(gpt2_RC_P1_posterior_samp[,i+ncols])
  posterior_gpt2_RC_P1[i,]$upper <- quantile(gpt2_RC_P1_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_RC_P1[i,]$lower <- quantile(gpt2_RC_P1_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_RC_P2[i,]$mean <- mean(gpt2_RC_P2_posterior_samp[,i+ncols])
  posterior_gpt2_RC_P2[i,]$SE <- sd(gpt2_RC_P2_posterior_samp[,i+ncols])
  posterior_gpt2_RC_P2[i,]$upper <- quantile(gpt2_RC_P2_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_RC_P2[i,]$lower <- quantile(gpt2_RC_P2_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_RC_P0[i,]$mean <- mean(nosurp_RC_P0_posterior_samp[,i+ncols])
  posterior_nosurp_RC_P0[i,]$SE <- sd(nosurp_RC_P0_posterior_samp[,i+ncols])
  posterior_nosurp_RC_P0[i,]$upper <- quantile(nosurp_RC_P0_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_RC_P0[i,]$lower <- quantile(nosurp_RC_P0_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_RC_P1[i,]$mean <- mean(nosurp_RC_P1_posterior_samp[,i+ncols])
  posterior_nosurp_RC_P1[i,]$SE <- sd(nosurp_RC_P1_posterior_samp[,i+ncols])
  posterior_nosurp_RC_P1[i,]$upper <- quantile(nosurp_RC_P1_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_RC_P1[i,]$lower <- quantile(nosurp_RC_P1_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_RC_P2[i,]$mean <- mean(nosurp_RC_P2_posterior_samp[,i+ncols])
  posterior_nosurp_RC_P2[i,]$SE <- sd(nosurp_RC_P2_posterior_samp[,i+ncols])
  posterior_nosurp_RC_P2[i,]$upper <- quantile(nosurp_RC_P2_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_RC_P2[i,]$lower <- quantile(nosurp_RC_P2_posterior_samp[,i+ncols],0.025)
}


#P0
sampled_correlations_P0 <- data.frame(Correlation=rep(NA,3000),EOI=rep("RC",3000),model=rep(c("lstm","gpt2","nosurp"),1000),ROI=0)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,24),lstm=rep(NA,24),gpt2=rep(NA,24),nosurp=rep(NA,24),EOI=rep("RC",24))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_RC_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_RC_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_RC_P0_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_RC_P0_posterior_samp[,j+ncols],1)
    }
  sampled_correlations_P0[((i-1)*3+1):((i-1)*3+3),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                            cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate)
}
#P0


#P1
sampled_correlations_P1 <- data.frame(Correlation=rep(NA,3000),EOI=rep("RC",3000),model=rep(c("lstm","gpt2","nosurp"),1000),ROI=1)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,24),lstm=rep(NA,24),gpt2=rep(NA,24),nosurp=rep(NA,24),EOI=rep("RC",24))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_RC_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_RC_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_RC_P1_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_RC_P1_posterior_samp[,j+ncols],1)
  }
  sampled_correlations_P1[((i-1)*3+1):((i-1)*3+3),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate)
}
#P1


#P2
sampled_correlations_P2 <- data.frame(Correlation=rep(NA,3000),EOI=rep("RC",3000),model=rep(c("lstm","gpt2","nosurp"),1000),ROI=2)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,24),lstm=rep(NA,24),gpt2=rep(NA,24),nosurp=rep(NA,24),EOI=rep("RC",24))
  for(j in 1:24){
    posterior_onesampleeachitem[j,1] <- sample(emp_RC_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_RC_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_RC_P2_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_RC_P2_posterior_samp[,j+ncols],1)
  }
  sampled_correlations_P2[((i-1)*3+1):((i-1)*3+3),'Correlation'] <- c(cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,2])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,3])$estimate,
                                                                      cor.test(posterior_onesampleeachitem[1:24,1],posterior_onesampleeachitem[1:24,4])$estimate)
}
#P2


sampled_correlations_maxregion <- sampled_correlations_P0
for(i in unique(sampled_correlations_maxregion$EOI)){
  for(j in unique(sampled_correlations_maxregion$model)){
    hist(sampled_correlations_maxregion$Correlation[sampled_correlations_maxregion$EOI==i&sampled_correlations_maxregion$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
#saveRDS(sampled_correlations_maxregion,"sampled_correlations_maxregion_RC.rds")


#P2 is the spillover1 when critical=determiner
for(i in unique(sampled_correlations_P2$EOI)){
  for(j in unique(sampled_correlations_P2$model)){
    hist(sampled_correlations_P2$Correlation[sampled_correlations_P2$EOI==i&sampled_correlations_P2$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P2,"RC_logadd_prior1/sampled_correlations_P2_RC.rds")
for(i in unique(sampled_correlations_P1$EOI)){
  for(j in unique(sampled_correlations_P1$model)){
    hist(sampled_correlations_P1$Correlation[sampled_correlations_P1$EOI==i&sampled_correlations_P1$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P1,"RC_logadd_prior1/sampled_correlations_P1_RC.rds")
for(i in unique(sampled_correlations_P0$EOI)){
  for(j in unique(sampled_correlations_P0$model)){
    hist(sampled_correlations_P0$Correlation[sampled_correlations_P0$EOI==i&sampled_correlations_P0$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
saveRDS(sampled_correlations_P0,"RC_logadd_prior1/sampled_correlations_P0_RC.rds")


