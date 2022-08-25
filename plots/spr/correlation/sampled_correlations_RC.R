library(brms)
library(dplyr)
library(ggplot2)

#emp_RC_P0 <- readRDS("RelativeClause/fit_verb_bayes_prior1.rds")
#emp_RC_P1 <- readRDS("RelativeClause/fit_det_bayes_prior1.rds")
#emp_RC_P2 <- readRDS("RelativeClause/fit_noun_bayes_prior1.rds")
#lstm_RC_P0 <- readRDS("RelativeClause/fit_verb_bayes_pred_lstm_prior1.rds")
#lstm_RC_P1 <- readRDS("RelativeClause/fit_det_bayes_pred_lstm_prior1.rds")
#lstm_RC_P2 <- readRDS("RelativeClause/fit_noun_bayes_pred_lstm_prior1.rds")
#gpt2_RC_P0 <- readRDS("RelativeClause/fit_verb_bayes_pred_gpt2_prior1.rds")
#gpt2_RC_P1 <- readRDS("RelativeClause/fit_det_bayes_pred_gpt2_prior1.rds")
#gpt2_RC_P2 <- readRDS("RelativeClause/fit_noun_bayes_pred_gpt2_prior1.rds")
#nosurp_RC_P0 <- readRDS("RelativeClause/fit_verb_bayes_pred_nosurp_prior1.rds")
#nosurp_RC_P1 <- readRDS("RelativeClause/fit_det_bayes_pred_nosurp_prior1.rds")
#nosurp_RC_P2 <- readRDS("RelativeClause/fit_noun_bayes_pred_nosurp_prior1.rds")



#posterior_samp <- posterior_samples(emp_RC_P0)
#randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(Type_num)',colnames(posterior_samp))]
#saveRDS(randomslope_names,"RelativeClause/RC_randomslopesnames.rds")
randomslope_names <- readRDS("RelativeClause/RC_randomslopesnames.rds")

emp_RC_P0_posterior_samp <- posterior_samples(emp_RC_P0, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
lstm_RC_P0_posterior_samp <- posterior_samples(lstm_RC_P0, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
gpt2_RC_P0_posterior_samp <- posterior_samples(gpt2_RC_P0, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
nosurp_RC_P0_posterior_samp <- posterior_samples(nosurp_RC_P0, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
emp_RC_P1_posterior_samp <- posterior_samples(emp_RC_P1, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
lstm_RC_P1_posterior_samp <- posterior_samples(lstm_RC_P1, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
gpt2_RC_P1_posterior_samp <- posterior_samples(gpt2_RC_P1, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
nosurp_RC_P1_posterior_samp <- posterior_samples(nosurp_RC_P1, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
emp_RC_P2_posterior_samp <- posterior_samples(emp_RC_P2, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
lstm_RC_P2_posterior_samp <- posterior_samples(lstm_RC_P2, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
gpt2_RC_P2_posterior_samp <- posterior_samples(gpt2_RC_P2, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))
nosurp_RC_P2_posterior_samp <- posterior_samples(nosurp_RC_P2, fixed=TRUE, pars=
                                                c("b_Type_num",randomslope_names))

rm(emp_RC_P0, lstm_RC_P0, gpt2_RC_P0, nosurp_RC_P0, emp_RC_P1, lstm_RC_P1,
gpt2_RC_P1, nosurp_RC_P1, emp_RC_P2, lstm_RC_P2, gpt2_RC_P2, nosurp_RC_P2,posterior_samp)
#saveRDS(emp_RC_P0_posterior_samp,"RelativeClause/emp_RC_P0_posterior_samp.rds")
#saveRDS(lstm_RC_P0_posterior_samp,"RelativeClause/lstm_RC_P0_posterior_samp.rds")
#saveRDS(gpt2_RC_P0_posterior_samp,"RelativeClause/gpt2_RC_P0_posterior_samp.rds")
#saveRDS(nosurp_RC_P0_posterior_samp,"RelativeClause/nosurp_RC_P0_posterior_samp.rds")
#saveRDS(emp_RC_P1_posterior_samp,"RelativeClause/emp_RC_P1_posterior_samp.rds")
#saveRDS(lstm_RC_P1_posterior_samp,"RelativeClause/lstm_RC_P1_posterior_samp.rds")
#saveRDS(gpt2_RC_P1_posterior_samp,"RelativeClause/gpt2_RC_P1_posterior_samp.rds")
#saveRDS(nosurp_RC_P1_posterior_samp,"RelativeClause/nosurp_RC_P1_posterior_samp.rds")
#saveRDS(emp_RC_P2_posterior_samp,"RelativeClause/emp_RC_P2_posterior_samp.rds")
#saveRDS(lstm_RC_P2_posterior_samp,"RelativeClause/lstm_RC_P2_posterior_samp.rds")
#saveRDS(gpt2_RC_P2_posterior_samp,"RelativeClause/gpt2_RC_P2_posterior_samp.rds")
#saveRDS(nosurp_RC_P2_posterior_samp,"RelativeClause/nosurp_RC_P2_posterior_samp.rds")

emp_RC_P0_posterior_samp <- readRDS("RelativeClause/emp_RC_P0_posterior_samp.rds")
lstm_RC_P0_posterior_samp <- readRDS("RelativeClause/lstm_RC_P0_posterior_samp.rds")
gpt2_RC_P0_posterior_samp <- readRDS("RelativeClause/gpt2_RC_P0_posterior_samp.rds")
nosurp_RC_P0_posterior_samp <- readRDS("RelativeClause/nosurp_RC_P0_posterior_samp.rds")
emp_RC_P1_posterior_samp <- readRDS("RelativeClause/emp_RC_P1_posterior_samp.rds")
lstm_RC_P1_posterior_samp <- readRDS("RelativeClause/lstm_RC_P1_posterior_samp.rds")
gpt2_RC_P1_posterior_samp <- readRDS("RelativeClause/gpt2_RC_P1_posterior_samp.rds")
nosurp_RC_P1_posterior_samp <- readRDS("RelativeClause/nosurp_RC_P1_posterior_samp.rds")
emp_RC_P2_posterior_samp <- readRDS("RelativeClause/emp_RC_P2_posterior_samp.rds")
lstm_RC_P2_posterior_samp <- readRDS("RelativeClause/lstm_RC_P2_posterior_samp.rds")
gpt2_RC_P2_posterior_samp <- readRDS("RelativeClause/gpt2_RC_P2_posterior_samp.rds")
nosurp_RC_P2_posterior_samp <- readRDS("RelativeClause/nosurp_RC_P2_posterior_samp.rds")




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



ncols <- ncol(emp_RC_P0_posterior_samp)
emp_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- emp_RC_P0_posterior_samp[,1]+emp_RC_P0_posterior_samp[,(1+1):(24+1)]
emp_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- emp_RC_P1_posterior_samp[,1]+emp_RC_P1_posterior_samp[,(1+1):(24+1)]
emp_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- emp_RC_P2_posterior_samp[,1]+emp_RC_P2_posterior_samp[,(1+1):(24+1)]
lstm_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- lstm_RC_P0_posterior_samp[,1]+lstm_RC_P0_posterior_samp[,(1+1):(24+1)]
lstm_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- lstm_RC_P1_posterior_samp[,1]+lstm_RC_P1_posterior_samp[,(1+1):(24+1)]
lstm_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- lstm_RC_P2_posterior_samp[,1]+lstm_RC_P2_posterior_samp[,(1+1):(24+1)]
gpt2_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- gpt2_RC_P0_posterior_samp[,1]+gpt2_RC_P0_posterior_samp[,(1+1):(24+1)]
gpt2_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- gpt2_RC_P1_posterior_samp[,1]+gpt2_RC_P1_posterior_samp[,(1+1):(24+1)]
gpt2_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- gpt2_RC_P2_posterior_samp[,1]+gpt2_RC_P2_posterior_samp[,(1+1):(24+1)]
nosurp_RC_P0_posterior_samp[,(1+ncols):(24+ncols)] <- nosurp_RC_P0_posterior_samp[,1]+nosurp_RC_P0_posterior_samp[,(1+1):(24+1)]
nosurp_RC_P1_posterior_samp[,(1+ncols):(24+ncols)] <- nosurp_RC_P1_posterior_samp[,1]+nosurp_RC_P1_posterior_samp[,(1+1):(24+1)]
nosurp_RC_P2_posterior_samp[,(1+ncols):(24+ncols)] <- nosurp_RC_P2_posterior_samp[,1]+nosurp_RC_P2_posterior_samp[,(1+1):(24+1)]


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


by_item <- data.frame(item=c(posterior_emp_RC_P0$item,posterior_emp_RC_P1$item,posterior_emp_RC_P2$item),
                      ROI=c(posterior_emp_RC_P0$ROI,posterior_emp_RC_P1$ROI,posterior_emp_RC_P2$ROI),
                      coef="RC",
                      mean=c(posterior_emp_RC_P0$mean,posterior_emp_RC_P1$mean,posterior_emp_RC_P2$mean),
                      lower=c(posterior_emp_RC_P0$lower,posterior_emp_RC_P1$lower,posterior_emp_RC_P2$lower),
                      upper=c(posterior_emp_RC_P0$upper,posterior_emp_RC_P1$upper,posterior_emp_RC_P2$upper))
by_item_lstm <- data.frame(item=c(posterior_lstm_RC_P0$item,posterior_lstm_RC_P1$item,posterior_lstm_RC_P2$item),
                           ROI=c(posterior_lstm_RC_P0$ROI,posterior_lstm_RC_P1$ROI,posterior_lstm_RC_P2$ROI),
                           coef="RC",
                           mean=c(posterior_lstm_RC_P0$mean,posterior_lstm_RC_P1$mean,posterior_lstm_RC_P2$mean),
                           lower=c(posterior_lstm_RC_P0$lower,posterior_lstm_RC_P1$lower,posterior_lstm_RC_P2$lower),
                           upper=c(posterior_lstm_RC_P0$upper,posterior_lstm_RC_P1$upper,posterior_lstm_RC_P2$upper))
by_item_gpt2 <- data.frame(item=c(posterior_gpt2_RC_P0$item,posterior_gpt2_RC_P1$item,posterior_gpt2_RC_P2$item),
                           ROI=c(posterior_gpt2_RC_P0$ROI,posterior_gpt2_RC_P1$ROI,posterior_gpt2_RC_P2$ROI),
                           coef="RC",
                           mean=c(posterior_gpt2_RC_P0$mean,posterior_gpt2_RC_P1$mean,posterior_gpt2_RC_P2$mean),
                           lower=c(posterior_gpt2_RC_P0$lower,posterior_gpt2_RC_P1$lower,posterior_gpt2_RC_P2$lower),
                           upper=c(posterior_gpt2_RC_P0$upper,posterior_gpt2_RC_P1$upper,posterior_gpt2_RC_P2$upper))
by_item_nosurp <- data.frame(item=c(posterior_nosurp_RC_P0$item,posterior_nosurp_RC_P1$item,posterior_nosurp_RC_P2$item),
                             ROI=c(posterior_nosurp_RC_P0$ROI,posterior_nosurp_RC_P1$ROI,posterior_nosurp_RC_P2$ROI),
                             coef="RC",
                             mean=c(posterior_nosurp_RC_P0$mean,posterior_nosurp_RC_P1$mean,posterior_nosurp_RC_P2$mean),
                             lower=c(posterior_nosurp_RC_P0$lower,posterior_nosurp_RC_P1$lower,posterior_nosurp_RC_P2$lower),
                             upper=c(posterior_nosurp_RC_P0$upper,posterior_nosurp_RC_P1$upper,posterior_nosurp_RC_P2$upper))
saveRDS(by_item,"RelativeClause/by_item.rds")
saveRDS(by_item_lstm,"RelativeClause/by_item_lstm.rds")
saveRDS(by_item_gpt2,"RelativeClause/by_item_gpt2.rds")
saveRDS(by_item_nosurp,"RelativeClause/by_item_nosurp.rds")

by_construction <- data.frame(ROI=c(0,1,2),coef="RC",mean=c(mean(emp_RC_P0_posterior_samp$b_Type_num),mean(emp_RC_P1_posterior_samp$b_Type_num),mean(emp_RC_P2_posterior_samp$b_Type_num)),
                              lower=c(quantile(emp_RC_P0_posterior_samp$b_Type_num,0.025),quantile(emp_RC_P1_posterior_samp$b_Type_num,0.025),quantile(emp_RC_P2_posterior_samp$b_Type_num,0.025)),
                              upper=c(quantile(emp_RC_P0_posterior_samp$b_Type_num,0.975),quantile(emp_RC_P1_posterior_samp$b_Type_num,0.975),quantile(emp_RC_P2_posterior_samp$b_Type_num,0.975)))
by_construction_lstm <- data.frame(ROI=c(0,1,2),coef="RC",mean=c(mean(lstm_RC_P0_posterior_samp$b_Type_num),mean(lstm_RC_P1_posterior_samp$b_Type_num),mean(lstm_RC_P2_posterior_samp$b_Type_num)),
                                   lower=c(quantile(lstm_RC_P0_posterior_samp$b_Type_num,0.025),quantile(lstm_RC_P1_posterior_samp$b_Type_num,0.025),quantile(lstm_RC_P2_posterior_samp$b_Type_num,0.025)),
                                   upper=c(quantile(lstm_RC_P0_posterior_samp$b_Type_num,0.975),quantile(lstm_RC_P1_posterior_samp$b_Type_num,0.975),quantile(lstm_RC_P2_posterior_samp$b_Type_num,0.975)))
by_construction_gpt2 <- data.frame(ROI=c(0,1,2),coef="RC",mean=c(mean(gpt2_RC_P0_posterior_samp$b_Type_num),mean(gpt2_RC_P1_posterior_samp$b_Type_num),mean(gpt2_RC_P2_posterior_samp$b_Type_num)),
                                   lower=c(quantile(gpt2_RC_P0_posterior_samp$b_Type_num,0.025),quantile(gpt2_RC_P1_posterior_samp$b_Type_num,0.025),quantile(gpt2_RC_P2_posterior_samp$b_Type_num,0.025)),
                                   upper=c(quantile(gpt2_RC_P0_posterior_samp$b_Type_num,0.975),quantile(gpt2_RC_P1_posterior_samp$b_Type_num,0.975),quantile(gpt2_RC_P2_posterior_samp$b_Type_num,0.975)))
by_construction_nosurp <- data.frame(ROI=c(0,1,2),coef="RC",mean=c(mean(nosurp_RC_P0_posterior_samp$b_Type_num),mean(nosurp_RC_P1_posterior_samp$b_Type_num),mean(nosurp_RC_P2_posterior_samp$b_Type_num)),
                                     lower=c(quantile(nosurp_RC_P0_posterior_samp$b_Type_num,0.025),quantile(nosurp_RC_P1_posterior_samp$b_Type_num,0.025),quantile(nosurp_RC_P2_posterior_samp$b_Type_num,0.025)),
                                     upper=c(quantile(nosurp_RC_P0_posterior_samp$b_Type_num,0.975),quantile(nosurp_RC_P1_posterior_samp$b_Type_num,0.975),quantile(nosurp_RC_P2_posterior_samp$b_Type_num,0.975)))

saveRDS(by_construction,"RelativeClause/by_construction.rds")
saveRDS(by_construction_lstm,"RelativeClause/by_construction_lstm.rds")
saveRDS(by_construction_gpt2,"RelativeClause/by_construction_gpt2.rds")
saveRDS(by_construction_nosurp,"RelativeClause/by_construction_nosurp.rds")

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






for_plotting_RC <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=mean)
colnames(for_plotting_RC) <- c("EOI","model","Correlation")
for_plotting_RC$SE <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=sd)$x
for_plotting_RC$ROI <- 0

temp <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=sd)$x
temp$ROI <- 1
for_plotting_RC <- rbind(for_plotting_RC,temp)
rm(temp)

temp <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=sd)$x
temp$ROI <- 2
for_plotting_RC <- rbind(for_plotting_RC,temp)
rm(temp)

#saveRDS(for_plotting_RC,"for_plotting_RC.rds")

for_plotting_RC_max <- for_plotting_RC[(for_plotting_RC$ROI==1),]


ggplot(for_plotting_RC_max,aes(x=Correlation,y=EOI,fill=model))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))

ggplot(for_plotting_RC,aes(x=Correlation,y=EOI,fill=model))+
  facet_grid(~ROI)+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))



cor.test(posterior_emp_RC_P0$mean,posterior_gpt2_RC_P0$mean)$estimate
cor.test(posterior_emp_RC_P0$mean,posterior_lstm_RC_P0$mean)$estimate
cor.test(posterior_emp_RC_P0$mean,posterior_nosurp_RC_P0$mean)$estimate
cor.test(posterior_emp_RC_P1$mean,posterior_gpt2_RC_P1$mean)$estimate
cor.test(posterior_emp_RC_P1$mean,posterior_lstm_RC_P1$mean)$estimate
cor.test(posterior_emp_RC_P1$mean,posterior_nosurp_RC_P1$mean)$estimate
cor.test(posterior_emp_RC_P2$mean,posterior_gpt2_RC_P2$mean)$estimate
cor.test(posterior_emp_RC_P2$mean,posterior_lstm_RC_P2$mean)$estimate
cor.test(posterior_emp_RC_P2$mean,posterior_nosurp_RC_P2$mean)$estimate



posterior_emp_RC_P0$model <- "emp" 
posterior_emp_RC_P1$model <- "emp" 
posterior_emp_RC_P2$model <- "emp" 
posterior_lstm_RC_P0$model <- "lstm" 
posterior_lstm_RC_P1$model <- "lstm" 
posterior_lstm_RC_P2$model <- "lstm"
posterior_gpt2_RC_P0$model <- "gpt2" 
posterior_gpt2_RC_P1$model <- "gpt2" 
posterior_gpt2_RC_P2$model <- "gpt2" 
posterior_nosurp_RC_P0$model <- "nosurp" 
posterior_nosurp_RC_P1$model <- "nosurp" 
posterior_nosurp_RC_P2$model <- "nosurp"

df_pointestimate <- 
  data.frame(emp=c(posterior_emp_RC_P0$mean,posterior_emp_RC_P1$mean,posterior_emp_RC_P2$mean),
             lstm=c(posterior_lstm_RC_P0$mean,posterior_lstm_RC_P1$mean,posterior_lstm_RC_P2$mean),
             gpt2=c(posterior_gpt2_RC_P0$mean,posterior_lstm_RC_P1$mean,posterior_lstm_RC_P2$mean),
             nosurp=c(posterior_nosurp_RC_P0$mean,posterior_lstm_RC_P1$mean,posterior_lstm_RC_P2$mean),
             EOI="RC",
             item=rep(25:48,3),
             ROI=rep(c(0,1,2),each=24))
saveRDS(df_pointestimate,"df_pointestimate_RC.rds")
