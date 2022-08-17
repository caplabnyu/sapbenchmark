library(brms)
library(dplyr)
library(ggplot2)

#emp_Agr <- readRDS("Agreement/brm_RT_Agr.rds")
#lstm_Agr <- readRDS("Agreement/brm_predicted_lstm_Agr.rds")
#gpt2_Agr <- readRDS("Agreement/brm_predicted_gpt2_Agr.rds")
#nosurp_Agr <- readRDS("Agreement/brm_predicted_Agr_nosurp.rds")



#posterior_samp <- posterior_samples(emp_Agr)
#randomslope_names <- colnames(posterior_samp)[grepl('r_item.+(pGram|position)',colnames(posterior_samp))]
#saveRDS(randomslope_names,"Agreement/Agr_randomslopesnames.rds")
randomslope_names <- readRDS("Agreement/Agr_randomslopesnames.rds")

emp_Agr_posterior_samp <- posterior_samples(emp_Agr, fixed=TRUE, pars=
                                              c("b_pGram.coded","b_position.coded.1","b_position.coded.2","b_pGram.coded:position.coded.1","b_pGram.coded:position.coded.2",randomslope_names))
lstm_Agr_posterior_samp <- posterior_samples(lstm_Agr, fixed=TRUE, pars=
                                              c("b_pGram.coded","b_position.coded.1","b_position.coded.2","b_pGram.coded:position.coded.1","b_pGram.coded:position.coded.2",randomslope_names))
gpt2_Agr_posterior_samp <- posterior_samples(gpt2_Agr, fixed=TRUE, pars=
                                              c("b_pGram.coded","b_position.coded.1","b_position.coded.2","b_pGram.coded:position.coded.1","b_pGram.coded:position.coded.2",randomslope_names))
nosurp_Agr_posterior_samp <- posterior_samples(nosurp_Agr, fixed=TRUE, pars=
                                              c("b_pGram.coded","b_position.coded.1","b_position.coded.2","b_pGram.coded:position.coded.1","b_pGram.coded:position.coded.2",randomslope_names))

rm(emp_Agr, lstm_Agr, gpt2_Agr, nosurp_Agr,posterior_samp)
#saveRDS(emp_Agr_posterior_samp,"Agreement/emp_Agr_posterior_samp.rds")
#saveRDS(lstm_Agr_posterior_samp,"Agreement/lstm_Agr_posterior_samp.rds")
#saveRDS(gpt2_Agr_posterior_samp,"Agreement/gpt2_Agr_posterior_samp.rds")
#saveRDS(nosurp_Agr_posterior_samp,"Agreement/nosurp_Agr_posterior_samp.rds")


emp_Agr_posterior_samp <- readRDS("Agreement/emp_Agr_posterior_samp.rds")
lstm_Agr_posterior_samp <- readRDS("Agreement/lstm_Agr_posterior_samp.rds")
gpt2_Agr_posterior_samp <- readRDS("Agreement/gpt2_Agr_posterior_samp.rds")
nosurp_Agr_posterior_samp <- readRDS("Agreement/nosurp_Agr_posterior_samp.rds")


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



ncols <- ncol(emp_Agr_posterior_samp)
emp_Agr_posterior_samp[,(1+ncols):(18+ncols)] <- emp_Agr_posterior_samp[,1]+(1/2)*emp_Agr_posterior_samp[,4]+emp_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*emp_Agr_posterior_samp[,(1+59):(18+59)]
emp_Agr_posterior_samp[,(19+ncols):(36+ncols)] <- emp_Agr_posterior_samp[,1]+(1/2)*emp_Agr_posterior_samp[,5]+emp_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*emp_Agr_posterior_samp[,(1+77):(18+77)]
emp_Agr_posterior_samp[,(37+ncols):(54+ncols)] <- emp_Agr_posterior_samp[,1]-(1/2)*emp_Agr_posterior_samp[,4]-(1/2)*emp_Agr_posterior_samp[,5]+emp_Agr_posterior_samp[,(1+5):(18+5)]-(1/2)*emp_Agr_posterior_samp[,(1+59):(18+59)]-(1/2)*emp_Agr_posterior_samp[,(1+77):(18+77)]
lstm_Agr_posterior_samp[,(1+ncols):(18+ncols)] <- lstm_Agr_posterior_samp[,1]+(1/2)*lstm_Agr_posterior_samp[,4]+lstm_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*lstm_Agr_posterior_samp[,(1+59):(18+59)]
lstm_Agr_posterior_samp[,(19+ncols):(36+ncols)] <- lstm_Agr_posterior_samp[,1]+(1/2)*lstm_Agr_posterior_samp[,5]+lstm_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*lstm_Agr_posterior_samp[,(1+77):(18+77)]
lstm_Agr_posterior_samp[,(37+ncols):(54+ncols)] <- lstm_Agr_posterior_samp[,1]-(1/2)*lstm_Agr_posterior_samp[,4]-(1/2)*lstm_Agr_posterior_samp[,5]+lstm_Agr_posterior_samp[,(1+5):(18+5)]-(1/2)*lstm_Agr_posterior_samp[,(1+59):(18+59)]-(1/2)*lstm_Agr_posterior_samp[,(1+77):(18+77)]
gpt2_Agr_posterior_samp[,(1+ncols):(18+ncols)] <- gpt2_Agr_posterior_samp[,1]+(1/2)*gpt2_Agr_posterior_samp[,4]+gpt2_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*gpt2_Agr_posterior_samp[,(1+59):(18+59)]
gpt2_Agr_posterior_samp[,(19+ncols):(36+ncols)] <- gpt2_Agr_posterior_samp[,1]+(1/2)*gpt2_Agr_posterior_samp[,5]+gpt2_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*gpt2_Agr_posterior_samp[,(1+77):(18+77)]
gpt2_Agr_posterior_samp[,(37+ncols):(54+ncols)] <- gpt2_Agr_posterior_samp[,1]-(1/2)*gpt2_Agr_posterior_samp[,4]-(1/2)*gpt2_Agr_posterior_samp[,5]+gpt2_Agr_posterior_samp[,(1+5):(18+5)]-(1/2)*gpt2_Agr_posterior_samp[,(1+59):(18+59)]-(1/2)*gpt2_Agr_posterior_samp[,(1+77):(18+77)]
nosurp_Agr_posterior_samp[,(1+ncols):(18+ncols)] <- nosurp_Agr_posterior_samp[,1]+(1/2)*nosurp_Agr_posterior_samp[,4]+nosurp_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*nosurp_Agr_posterior_samp[,(1+59):(18+59)]
nosurp_Agr_posterior_samp[,(19+ncols):(36+ncols)] <- nosurp_Agr_posterior_samp[,1]+(1/2)*nosurp_Agr_posterior_samp[,5]+nosurp_Agr_posterior_samp[,(1+5):(18+5)]+(1/2)*nosurp_Agr_posterior_samp[,(1+77):(18+77)]
nosurp_Agr_posterior_samp[,(37+ncols):(54+ncols)] <- nosurp_Agr_posterior_samp[,1]-(1/2)*nosurp_Agr_posterior_samp[,4]-(1/2)*nosurp_Agr_posterior_samp[,5]+nosurp_Agr_posterior_samp[,(1+5):(18+5)]-(1/2)*nosurp_Agr_posterior_samp[,(1+59):(18+59)]-(1/2)*nosurp_Agr_posterior_samp[,(1+77):(18+77)]

for(i in 1:18){
  posterior_emp_Agr_P0[i,]$mean <- mean(emp_Agr_posterior_samp[,i+ncols])
  posterior_emp_Agr_P0[i,]$SE <- sd(emp_Agr_posterior_samp[,i+ncols])
  posterior_emp_Agr_P0[i,]$upper <- quantile(emp_Agr_posterior_samp[,i+ncols],0.975)
  posterior_emp_Agr_P0[i,]$lower <- quantile(emp_Agr_posterior_samp[,i+ncols],0.025)
  posterior_emp_Agr_P1[i,]$mean <- mean(emp_Agr_posterior_samp[,i+ncols+18])
  posterior_emp_Agr_P1[i,]$SE <- sd(emp_Agr_posterior_samp[,i+ncols+18])
  posterior_emp_Agr_P1[i,]$upper <- quantile(emp_Agr_posterior_samp[,i+ncols+18],0.975)
  posterior_emp_Agr_P1[i,]$lower <- quantile(emp_Agr_posterior_samp[,i+ncols+18],0.025)
  posterior_emp_Agr_P2[i,]$mean <- mean(emp_Agr_posterior_samp[,i+ncols+36])
  posterior_emp_Agr_P2[i,]$SE <- sd(emp_Agr_posterior_samp[,i+ncols+36])
  posterior_emp_Agr_P2[i,]$upper <- quantile(emp_Agr_posterior_samp[,i+ncols+36],0.975)
  posterior_emp_Agr_P2[i,]$lower <- quantile(emp_Agr_posterior_samp[,i+ncols+36],0.025)
  posterior_lstm_Agr_P0[i,]$mean <- mean(lstm_Agr_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P0[i,]$SE <- sd(lstm_Agr_posterior_samp[,i+ncols])
  posterior_lstm_Agr_P0[i,]$upper <- quantile(lstm_Agr_posterior_samp[,i+ncols],0.975)
  posterior_lstm_Agr_P0[i,]$lower <- quantile(lstm_Agr_posterior_samp[,i+ncols],0.025)
  posterior_lstm_Agr_P1[i,]$mean <- mean(lstm_Agr_posterior_samp[,i+ncols+18])
  posterior_lstm_Agr_P1[i,]$SE <- sd(lstm_Agr_posterior_samp[,i+ncols+18])
  posterior_lstm_Agr_P1[i,]$upper <- quantile(lstm_Agr_posterior_samp[,i+ncols+18],0.975)
  posterior_lstm_Agr_P1[i,]$lower <- quantile(lstm_Agr_posterior_samp[,i+ncols+18],0.025)
  posterior_lstm_Agr_P2[i,]$mean <- mean(lstm_Agr_posterior_samp[,i+ncols+36])
  posterior_lstm_Agr_P2[i,]$SE <- sd(lstm_Agr_posterior_samp[,i+ncols+36])
  posterior_lstm_Agr_P2[i,]$upper <- quantile(lstm_Agr_posterior_samp[,i+ncols+36],0.975)
  posterior_lstm_Agr_P2[i,]$lower <- quantile(lstm_Agr_posterior_samp[,i+ncols+36],0.025)
  posterior_gpt2_Agr_P0[i,]$mean <- mean(gpt2_Agr_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P0[i,]$SE <- sd(gpt2_Agr_posterior_samp[,i+ncols])
  posterior_gpt2_Agr_P0[i,]$upper <- quantile(gpt2_Agr_posterior_samp[,i+ncols],0.975)
  posterior_gpt2_Agr_P0[i,]$lower <- quantile(gpt2_Agr_posterior_samp[,i+ncols],0.025)
  posterior_gpt2_Agr_P1[i,]$mean <- mean(gpt2_Agr_posterior_samp[,i+ncols+18])
  posterior_gpt2_Agr_P1[i,]$SE <- sd(gpt2_Agr_posterior_samp[,i+ncols+18])
  posterior_gpt2_Agr_P1[i,]$upper <- quantile(gpt2_Agr_posterior_samp[,i+ncols+18],0.975)
  posterior_gpt2_Agr_P1[i,]$lower <- quantile(gpt2_Agr_posterior_samp[,i+ncols+18],0.025)
  posterior_gpt2_Agr_P2[i,]$mean <- mean(gpt2_Agr_posterior_samp[,i+ncols+36])
  posterior_gpt2_Agr_P2[i,]$SE <- sd(gpt2_Agr_posterior_samp[,i+ncols+36])
  posterior_gpt2_Agr_P2[i,]$upper <- quantile(gpt2_Agr_posterior_samp[,i+ncols+36],0.975)
  posterior_gpt2_Agr_P2[i,]$lower <- quantile(gpt2_Agr_posterior_samp[,i+ncols+36],0.025)
  posterior_nosurp_Agr_P0[i,]$mean <- mean(nosurp_Agr_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P0[i,]$SE <- sd(nosurp_Agr_posterior_samp[,i+ncols])
  posterior_nosurp_Agr_P0[i,]$upper <- quantile(nosurp_Agr_posterior_samp[,i+ncols],0.975)
  posterior_nosurp_Agr_P0[i,]$lower <- quantile(nosurp_Agr_posterior_samp[,i+ncols],0.025)
  posterior_nosurp_Agr_P1[i,]$mean <- mean(nosurp_Agr_posterior_samp[,i+ncols+18])
  posterior_nosurp_Agr_P1[i,]$SE <- sd(nosurp_Agr_posterior_samp[,i+ncols+18])
  posterior_nosurp_Agr_P1[i,]$upper <- quantile(nosurp_Agr_posterior_samp[,i+ncols+18],0.975)
  posterior_nosurp_Agr_P1[i,]$lower <- quantile(nosurp_Agr_posterior_samp[,i+ncols+18],0.025)
  posterior_nosurp_Agr_P2[i,]$mean <- mean(nosurp_Agr_posterior_samp[,i+ncols+36])
  posterior_nosurp_Agr_P2[i,]$SE <- sd(nosurp_Agr_posterior_samp[,i+ncols+36])
  posterior_nosurp_Agr_P2[i,]$upper <- quantile(nosurp_Agr_posterior_samp[,i+ncols+36],0.975)
  posterior_nosurp_Agr_P2[i,]$lower <- quantile(nosurp_Agr_posterior_samp[,i+ncols+36],0.025)
}



#P0
sampled_correlations_P0 <- data.frame(Correlation=rep(NA,3000),EOI=rep("Agr",3000),model=rep(c("lstm","gpt2","nosurp"),1000),ROI=0)
for(i in 1:1000){
  posterior_onesampleeachitem <- data.frame(emp=rep(NA,18),lstm=rep(NA,18),gpt2=rep(NA,18),nosurp=rep(NA,18),EOI=rep("Agr",18))
  for(j in 1:18){
    posterior_onesampleeachitem[j,1] <- sample(emp_Agr_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_Agr_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_Agr_posterior_samp[,j+ncols],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_Agr_posterior_samp[,j+ncols],1)
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
    posterior_onesampleeachitem[j,1] <- sample(emp_Agr_posterior_samp[,j+ncols+18],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_Agr_posterior_samp[,j+ncols+18],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_Agr_posterior_samp[,j+ncols+18],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_Agr_posterior_samp[,j+ncols+18],1)
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
    posterior_onesampleeachitem[j,1] <- sample(emp_Agr_posterior_samp[,j+ncols+36],1)
    posterior_onesampleeachitem[j,2] <- sample(lstm_Agr_posterior_samp[,j+ncols+36],1)
    posterior_onesampleeachitem[j,3] <- sample(gpt2_Agr_posterior_samp[,j+ncols+36],1)
    posterior_onesampleeachitem[j,4] <- sample(nosurp_Agr_posterior_samp[,j+ncols+36],1)
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

for_plotting_Agr$maxregion <- rep(c(FALSE,TRUE,FALSE),each=3)

for_plotting_Agr_max <- for_plotting_Agr[for_plotting_Agr$maxregion==TRUE,]


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
saveRDS(df_pointestimate,"df_pointestimate_Agr.rds")
