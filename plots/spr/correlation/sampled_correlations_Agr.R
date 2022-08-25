library(brms)
library(dplyr)
library(ggplot2)

#emp_Agr_P0 <- readRDS("Agreement/brm_RT_Agr_P0.rds")
#emp_Agr_P1 <- readRDS("Agreement/brm_RT_Agr_P1.rds")
#emp_Agr_P2 <- readRDS("Agreement/brm_RT_Agr_P2.rds")
#lstm_Agr_P0 <- readRDS("Agreement/brm_predicted_lstm_Agr_P0.rds")
#lstm_Agr_P1 <- readRDS("Agreement/brm_predicted_lstm_Agr_P1.rds")
#lstm_Agr_P2 <- readRDS("Agreement/brm_predicted_lstm_Agr_P2.rds")
#gpt2_Agr_P0 <- readRDS("Agreement/brm_predicted_gpt2_Agr_P0.rds")
#gpt2_Agr_P1 <- readRDS("Agreement/brm_predicted_gpt2_Agr_P1.rds")
#gpt2_Agr_P2 <- readRDS("Agreement/brm_predicted_gpt2_Agr_P2.rds")
#nosurp_Agr_P0 <- readRDS("Agreement/brm_predicted_nosurp_Agr_P0.rds")
#nosurp_Agr_P1 <- readRDS("Agreement/brm_predicted_nosurp_Agr_P1.rds")
#nosurp_Agr_P2 <- readRDS("Agreement/brm_predicted_nosurp_Agr_P2.rds")



#posterior_samp <- posterior_samples(emp_Agr_P0)
#randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(pGram)',colnames(posterior_samp))]
#saveRDS(randomslope_names,"Agreement/Agr_randomslopesnames.rds")
randomslope_names <- readRDS("Agreement/Agr_randomslopesnames.rds")

emp_Agr_P0_posterior_samp <- posterior_samples(emp_Agr_P0, fixed=TRUE, pars=
                                              c("b_pGram.coded",randomslope_names))
emp_Agr_P1_posterior_samp <- posterior_samples(emp_Agr_P1, fixed=TRUE, pars=
                                                 c("b_pGram.coded",randomslope_names))
emp_Agr_P2_posterior_samp <- posterior_samples(emp_Agr_P2, fixed=TRUE, pars=
                                                 c("b_pGram.coded",randomslope_names))
lstm_Agr_P0_posterior_samp <- posterior_samples(lstm_Agr_P0, fixed=TRUE, pars=
                                              c("b_pGram.coded",randomslope_names))
lstm_Agr_P1_posterior_samp <- posterior_samples(lstm_Agr_P1, fixed=TRUE, pars=
                                                  c("b_pGram.coded",randomslope_names))
lstm_Agr_P2_posterior_samp <- posterior_samples(lstm_Agr_P2, fixed=TRUE, pars=
                                                  c("b_pGram.coded",randomslope_names))
gpt2_Agr_P0_posterior_samp <- posterior_samples(gpt2_Agr_P0, fixed=TRUE, pars=
                                              c("b_pGram.coded",randomslope_names))
gpt2_Agr_P1_posterior_samp <- posterior_samples(gpt2_Agr_P1, fixed=TRUE, pars=
                                                  c("b_pGram.coded",randomslope_names))
gpt2_Agr_P2_posterior_samp <- posterior_samples(gpt2_Agr_P2, fixed=TRUE, pars=
                                                  c("b_pGram.coded",randomslope_names))
nosurp_Agr_P0_posterior_samp <- posterior_samples(nosurp_Agr_P0, fixed=TRUE, pars=
                                              c("b_pGram.coded",randomslope_names))
nosurp_Agr_P1_posterior_samp <- posterior_samples(nosurp_Agr_P1, fixed=TRUE, pars=
                                                    c("b_pGram.coded",randomslope_names))
nosurp_Agr_P2_posterior_samp <- posterior_samples(nosurp_Agr_P2, fixed=TRUE, pars=
                                                    c("b_pGram.coded",randomslope_names))

rm(emp_Agr_P0,emp_Agr_P1,emp_Agr_P2, lstm_Agr_P0, lstm_Agr_P1, lstm_Agr_P2, gpt2_Agr_P0, gpt2_Agr_P1, gpt2_Agr_P2,
   nosurp_Agr_P0, nosurp_Agr_P1, nosurp_Agr_P2, posterior_samp)
#saveRDS(emp_Agr_P0_posterior_samp,"Agreement/emp_Agr_P0_posterior_samp.rds")
#saveRDS(emp_Agr_P1_posterior_samp,"Agreement/emp_Agr_P1_posterior_samp.rds")
#saveRDS(emp_Agr_P2_posterior_samp,"Agreement/emp_Agr_P2_posterior_samp.rds")
#saveRDS(lstm_Agr_P0_posterior_samp,"Agreement/lstm_Agr_P0_posterior_samp.rds")
#saveRDS(lstm_Agr_P1_posterior_samp,"Agreement/lstm_Agr_P1_posterior_samp.rds")
#saveRDS(lstm_Agr_P2_posterior_samp,"Agreement/lstm_Agr_P2_posterior_samp.rds")
#saveRDS(gpt2_Agr_P0_posterior_samp,"Agreement/gpt2_Agr_P0_posterior_samp.rds")
#saveRDS(gpt2_Agr_P1_posterior_samp,"Agreement/gpt2_Agr_P1_posterior_samp.rds")
#saveRDS(gpt2_Agr_P2_posterior_samp,"Agreement/gpt2_Agr_P2_posterior_samp.rds")
#saveRDS(nosurp_Agr_P0_posterior_samp,"Agreement/nosurp_Agr_P0_posterior_samp.rds")
#saveRDS(nosurp_Agr_P1_posterior_samp,"Agreement/nosurp_Agr_P1_posterior_samp.rds")
#saveRDS(nosurp_Agr_P2_posterior_samp,"Agreement/nosurp_Agr_P2_posterior_samp.rds")


emp_Agr_P0_posterior_samp <- readRDS("Agreement/emp_Agr_P0_posterior_samp.rds")
emp_Agr_P1_posterior_samp <- readRDS("Agreement/emp_Agr_P1_posterior_samp.rds")
emp_Agr_P2_posterior_samp <- readRDS("Agreement/emp_Agr_P2_posterior_samp.rds")
lstm_Agr_P0_posterior_samp <- readRDS("Agreement/lstm_Agr_P0_posterior_samp.rds")
lstm_Agr_P1_posterior_samp <- readRDS("Agreement/lstm_Agr_P1_posterior_samp.rds")
lstm_Agr_P2_posterior_samp <- readRDS("Agreement/lstm_Agr_P2_posterior_samp.rds")
gpt2_Agr_P0_posterior_samp <- readRDS("Agreement/gpt2_Agr_P0_posterior_samp.rds")
gpt2_Agr_P1_posterior_samp <- readRDS("Agreement/gpt2_Agr_P1_posterior_samp.rds")
gpt2_Agr_P2_posterior_samp <- readRDS("Agreement/gpt2_Agr_P2_posterior_samp.rds")
nosurp_Agr_P0_posterior_samp <- readRDS("Agreement/nosurp_Agr_P0_posterior_samp.rds")
nosurp_Agr_P1_posterior_samp <- readRDS("Agreement/nosurp_Agr_P1_posterior_samp.rds")
nosurp_Agr_P2_posterior_samp <- readRDS("Agreement/nosurp_Agr_P2_posterior_samp.rds")

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



ncols <- ncol(emp_Agr_P0_posterior_samp)
emp_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- emp_Agr_P0_posterior_samp[,1]+emp_Agr_P0_posterior_samp[,(1+1):(18+1)]
emp_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- emp_Agr_P1_posterior_samp[,1]+emp_Agr_P1_posterior_samp[,(1+1):(18+1)]
emp_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- emp_Agr_P2_posterior_samp[,1]+emp_Agr_P2_posterior_samp[,(1+1):(18+1)]
lstm_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- lstm_Agr_P0_posterior_samp[,1]+lstm_Agr_P0_posterior_samp[,(1+1):(18+1)]
lstm_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- lstm_Agr_P1_posterior_samp[,1]+lstm_Agr_P1_posterior_samp[,(1+1):(18+1)]
lstm_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- lstm_Agr_P2_posterior_samp[,1]+lstm_Agr_P2_posterior_samp[,(1+1):(18+1)]
gpt2_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- gpt2_Agr_P0_posterior_samp[,1]+gpt2_Agr_P0_posterior_samp[,(1+1):(18+1)]
gpt2_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- gpt2_Agr_P1_posterior_samp[,1]+gpt2_Agr_P1_posterior_samp[,(1+1):(18+1)]
gpt2_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- gpt2_Agr_P2_posterior_samp[,1]+gpt2_Agr_P2_posterior_samp[,(1+1):(18+1)]
nosurp_Agr_P0_posterior_samp[,(1+ncols):(18+ncols)] <- nosurp_Agr_P0_posterior_samp[,1]+nosurp_Agr_P0_posterior_samp[,(1+1):(18+1)]
nosurp_Agr_P1_posterior_samp[,(1+ncols):(18+ncols)] <- nosurp_Agr_P1_posterior_samp[,1]+nosurp_Agr_P1_posterior_samp[,(1+1):(18+1)]
nosurp_Agr_P2_posterior_samp[,(1+ncols):(18+ncols)] <- nosurp_Agr_P2_posterior_samp[,1]+nosurp_Agr_P2_posterior_samp[,(1+1):(18+1)]





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

by_item <- data.frame(item=c(posterior_emp_Agr_P0$item,posterior_emp_Agr_P1$item,posterior_emp_Agr_P2$item),
                      ROI=c(posterior_emp_Agr_P0$ROI,posterior_emp_Agr_P1$ROI,posterior_emp_Agr_P2$ROI),
                      coef="Agr",
                      mean=c(posterior_emp_Agr_P0$mean,posterior_emp_Agr_P1$mean,posterior_emp_Agr_P2$mean),
                      lower=c(posterior_emp_Agr_P0$lower,posterior_emp_Agr_P1$lower,posterior_emp_Agr_P2$lower),
                      upper=c(posterior_emp_Agr_P0$upper,posterior_emp_Agr_P1$upper,posterior_emp_Agr_P2$upper))
by_item_lstm <- data.frame(item=c(posterior_lstm_Agr_P0$item,posterior_lstm_Agr_P1$item,posterior_lstm_Agr_P2$item),
                           ROI=c(posterior_lstm_Agr_P0$ROI,posterior_lstm_Agr_P1$ROI,posterior_lstm_Agr_P2$ROI),
                           coef="Agr",
                           mean=c(posterior_lstm_Agr_P0$mean,posterior_lstm_Agr_P1$mean,posterior_lstm_Agr_P2$mean),
                           lower=c(posterior_lstm_Agr_P0$lower,posterior_lstm_Agr_P1$lower,posterior_lstm_Agr_P2$lower),
                           upper=c(posterior_lstm_Agr_P0$upper,posterior_lstm_Agr_P1$upper,posterior_lstm_Agr_P2$upper))
by_item_gpt2 <- data.frame(item=c(posterior_gpt2_Agr_P0$item,posterior_gpt2_Agr_P1$item,posterior_gpt2_Agr_P2$item),
                           ROI=c(posterior_gpt2_Agr_P0$ROI,posterior_gpt2_Agr_P1$ROI,posterior_gpt2_Agr_P2$ROI),
                           coef="Agr",
                           mean=c(posterior_gpt2_Agr_P0$mean,posterior_gpt2_Agr_P1$mean,posterior_gpt2_Agr_P2$mean),
                           lower=c(posterior_gpt2_Agr_P0$lower,posterior_gpt2_Agr_P1$lower,posterior_gpt2_Agr_P2$lower),
                           upper=c(posterior_gpt2_Agr_P0$upper,posterior_gpt2_Agr_P1$upper,posterior_gpt2_Agr_P2$upper))
by_item_nosurp <- data.frame(item=c(posterior_nosurp_Agr_P0$item,posterior_nosurp_Agr_P1$item,posterior_nosurp_Agr_P2$item),
                             ROI=c(posterior_nosurp_Agr_P0$ROI,posterior_nosurp_Agr_P1$ROI,posterior_nosurp_Agr_P2$ROI),
                             coef="Agr",
                             mean=c(posterior_nosurp_Agr_P0$mean,posterior_nosurp_Agr_P1$mean,posterior_nosurp_Agr_P2$mean),
                             lower=c(posterior_nosurp_Agr_P0$lower,posterior_nosurp_Agr_P1$lower,posterior_nosurp_Agr_P2$lower),
                             upper=c(posterior_nosurp_Agr_P0$upper,posterior_nosurp_Agr_P1$upper,posterior_nosurp_Agr_P2$upper))
saveRDS(by_item,"Agreement/by_item.rds")
saveRDS(by_item_lstm,"Agreement/by_item_lstm.rds")
saveRDS(by_item_gpt2,"Agreement/by_item_gpt2.rds")
saveRDS(by_item_nosurp,"Agreement/by_item_nosurp.rds")

by_construction <- data.frame(ROI=c(0,1,2),coef="Agr",mean=c(mean(emp_Agr_P0_posterior_samp$b_pGram.coded),mean(emp_Agr_P1_posterior_samp$b_pGram.coded),mean(emp_Agr_P2_posterior_samp$b_pGram.coded)),
                              lower=c(quantile(emp_Agr_P0_posterior_samp$b_pGram.coded,0.025),quantile(emp_Agr_P1_posterior_samp$b_pGram.coded,0.025),quantile(emp_Agr_P2_posterior_samp$b_pGram.coded,0.025)),
                              upper=c(quantile(emp_Agr_P0_posterior_samp$b_pGram.coded,0.975),quantile(emp_Agr_P1_posterior_samp$b_pGram.coded,0.975),quantile(emp_Agr_P2_posterior_samp$b_pGram.coded,0.975)))
by_construction_lstm <- data.frame(ROI=c(0,1,2),coef="Agr",mean=c(mean(lstm_Agr_P0_posterior_samp$b_pGram.coded),mean(lstm_Agr_P1_posterior_samp$b_pGram.coded),mean(lstm_Agr_P2_posterior_samp$b_pGram.coded)),
                                   lower=c(quantile(lstm_Agr_P0_posterior_samp$b_pGram.coded,0.025),quantile(lstm_Agr_P1_posterior_samp$b_pGram.coded,0.025),quantile(lstm_Agr_P2_posterior_samp$b_pGram.coded,0.025)),
                                   upper=c(quantile(lstm_Agr_P0_posterior_samp$b_pGram.coded,0.975),quantile(lstm_Agr_P1_posterior_samp$b_pGram.coded,0.975),quantile(lstm_Agr_P2_posterior_samp$b_pGram.coded,0.975)))
by_construction_gpt2 <- data.frame(ROI=c(0,1,2),coef="Agr",mean=c(mean(gpt2_Agr_P0_posterior_samp$b_pGram.coded),mean(gpt2_Agr_P1_posterior_samp$b_pGram.coded),mean(gpt2_Agr_P2_posterior_samp$b_pGram.coded)),
                                   lower=c(quantile(gpt2_Agr_P0_posterior_samp$b_pGram.coded,0.025),quantile(gpt2_Agr_P1_posterior_samp$b_pGram.coded,0.025),quantile(gpt2_Agr_P2_posterior_samp$b_pGram.coded,0.025)),
                                   upper=c(quantile(gpt2_Agr_P0_posterior_samp$b_pGram.coded,0.975),quantile(gpt2_Agr_P1_posterior_samp$b_pGram.coded,0.975),quantile(gpt2_Agr_P2_posterior_samp$b_pGram.coded,0.975)))
by_construction_nosurp <- data.frame(ROI=c(0,1,2),coef="Agr",mean=c(mean(nosurp_Agr_P0_posterior_samp$b_pGram.coded),mean(nosurp_Agr_P1_posterior_samp$b_pGram.coded),mean(nosurp_Agr_P2_posterior_samp$b_pGram.coded)),
                                     lower=c(quantile(nosurp_Agr_P0_posterior_samp$b_pGram.coded,0.025),quantile(nosurp_Agr_P1_posterior_samp$b_pGram.coded,0.025),quantile(nosurp_Agr_P2_posterior_samp$b_pGram.coded,0.025)),
                                     upper=c(quantile(nosurp_Agr_P0_posterior_samp$b_pGram.coded,0.975),quantile(nosurp_Agr_P1_posterior_samp$b_pGram.coded,0.975),quantile(nosurp_Agr_P2_posterior_samp$b_pGram.coded,0.975)))

saveRDS(by_construction,"Agreement/by_construction.rds")
saveRDS(by_construction_lstm,"Agreement/by_construction_lstm.rds")
saveRDS(by_construction_gpt2,"Agreement/by_construction_gpt2.rds")
saveRDS(by_construction_nosurp,"Agreement/by_construction_nosurp.rds")





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


sampled_correlations_maxregion <- sampled_correlations_P1
for(i in unique(sampled_correlations_maxregion$EOI)){
  for(j in unique(sampled_correlations_maxregion$model)){
    hist(sampled_correlations_maxregion$Correlation[sampled_correlations_maxregion$EOI==i&sampled_correlations_maxregion$model==j],main=paste("EOI=",i,", model=",j),xlab="posterior_correlations")
  }
}
#saveRDS(sampled_correlations_maxregion,"sampled_correlations_maxregion_Agr.rds")



for_plotting_Agr <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=mean)
colnames(for_plotting_Agr) <- c("EOI","model","Correlation")
for_plotting_Agr$SE <- aggregate(sampled_correlations_P0$Correlation,by=list(sampled_correlations_P0$EOI,sampled_correlations_P0$model),FUN=sd)$x
for_plotting_Agr$ROI <- 0

temp <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P1$Correlation,by=list(sampled_correlations_P1$EOI,sampled_correlations_P1$model),FUN=sd)$x
temp$ROI <- 1
for_plotting_Agr <- rbind(for_plotting_Agr,temp)
rm(temp)

temp <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=mean)
colnames(temp) <- c("EOI","model","Correlation")
temp$SE <- aggregate(sampled_correlations_P2$Correlation,by=list(sampled_correlations_P2$EOI,sampled_correlations_P2$model),FUN=sd)$x
temp$ROI <- 2
for_plotting_Agr <- rbind(for_plotting_Agr,temp)
rm(temp)

#saveRDS(for_plotting_Agr,"for_plotting_Agr.rds")


for_plotting_Agr_max <- for_plotting_Agr[for_plotting_Agr$ROI==1,]

ggplot(for_plotting_Agr_max,aes(x=Correlation,y=EOI,fill=model))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))


ggplot(for_plotting_Agr,aes(x=Correlation,y=EOI,fill=model))+
  facet_grid(~ROI)+
  geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(xmin=Correlation - (1.96*SE), 
                    xmax=Correlation + (1.96*SE)),
                width=.5,position=position_dodge(1))





cor.test(posterior_emp_Agr_P0$mean,posterior_gpt2_Agr_P0$mean)$estimate
cor.test(posterior_emp_Agr_P0$mean,posterior_lstm_Agr_P0$mean)$estimate
cor.test(posterior_emp_Agr_P0$mean,posterior_nosurp_Agr_P0$mean)$estimate
cor.test(posterior_emp_Agr_P1$mean,posterior_gpt2_Agr_P1$mean)$estimate
cor.test(posterior_emp_Agr_P1$mean,posterior_lstm_Agr_P1$mean)$estimate
cor.test(posterior_emp_Agr_P1$mean,posterior_nosurp_Agr_P1$mean)$estimate
cor.test(posterior_emp_Agr_P2$mean,posterior_gpt2_Agr_P2$mean)$estimate
cor.test(posterior_emp_Agr_P2$mean,posterior_lstm_Agr_P2$mean)$estimate
cor.test(posterior_emp_Agr_P2$mean,posterior_nosurp_Agr_P2$mean)$estimate



posterior_emp_Agr_P0$model <- "emp" 
posterior_emp_Agr_P1$model <- "emp" 
posterior_emp_Agr_P2$model <- "emp" 
posterior_lstm_Agr_P0$model <- "lstm" 
posterior_lstm_Agr_P1$model <- "lstm" 
posterior_lstm_Agr_P2$model <- "lstm" 
posterior_gpt2_Agr_P0$model <- "gpt2" 
posterior_gpt2_Agr_P1$model <- "gpt2" 
posterior_gpt2_Agr_P2$model <- "gpt2" 
posterior_nosurp_Agr_P0$model <- "nosurp" 
posterior_nosurp_Agr_P1$model <- "nosurp" 
posterior_nosurp_Agr_P2$model <- "nosurp" 
df_pointestimate <- 
  data.frame(emp=posterior_emp_Agr_P1$mean,
             lstm=posterior_lstm_Agr_P1$mean,
             gpt2=posterior_gpt2_Agr_P1$mean,
             nosurp=posterior_nosurp_Agr_P1$mean)
df_pointestimate$EOI <- "Agr"
df_pointestimate$item <- c(1,3,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24)
#saveRDS(df_pointestimate,"df_pointestimate_Agr.rds")
