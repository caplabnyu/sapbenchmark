library(dplyr)
library(lme4)
library(ggplot2)
library(brms)
library(reshape2)
library(egg)
library(gridExtra)
library(grid)
source("../../util.R")

rt.data <- load_data("ClassicGP")
rt.data$SZM1 <- ifelse(
  rt.data$CONSTRUCTION=="NPS",1,0
)
rt.data$SZM2 <- ifelse(
  rt.data$CONSTRUCTION=="NPZ",1,0
)
corr_each <- vector()
size_each <- vector()
type <- vector()
for(Size in 2000){
  for(shuffle_participant in 1:5){
    subsample <- rt.data[rt.data$participant%in%sample(unique(rt.data$participant),Size,replace=F)&rt.data$ROI==1,]
    subsample <- arrange(subsample,participant,Type,item)
    for(shuffle_trial in 1:3){
      counter=0
      timeout=0
      while(counter==0){
        half_name <- sample(unique(subsample$participant),(Size/2),replace=F)
        subsample$splitgroup <- ifelse(subsample$participant%in%half_name,"first","second")
        lmer_firsthalf <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||participant),data=subsample[subsample$splitgroup=="first",],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        lmer_secondhalf <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||participant),data=subsample[subsample$splitgroup=="second",],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        random_variance <- c(summary(lmer_firsthalf)$varcor[7]$item[[1]],summary(lmer_firsthalf)$varcor[8]$item.1[[1]],summary(lmer_firsthalf)$varcor[11]$item.4[[1]],
                             summary(lmer_secondhalf)$varcor[7]$item[[1]],summary(lmer_secondhalf)$varcor[8]$item.1[[1]],summary(lmer_secondhalf)$varcor[11]$item.4[[1]])
        if(!any(random_variance==0)){
          counter=1
        }else{timeout=timeout+1}
      }
      print(timeout)
      fixed_AMBUAMB_1st <-fixef(lmer_firsthalf)[2]
      fixed_AMBUAMB_2nd <-fixef(lmer_secondhalf)[2]
      fixed_interaction1_1st <-fixef(lmer_firsthalf)[5]
      fixed_interaction1_2nd <-fixef(lmer_secondhalf)[5]
      fixed_interaction2_1st <-fixef(lmer_firsthalf)[6]
      fixed_interaction2_2nd <-fixef(lmer_secondhalf)[6]
      rand_AMBUAMB_1st <- ranef(lmer_firsthalf)[['item']]$AMBUAMB
      rand_AMBUAMB_2nd <- ranef(lmer_secondhalf)[['item']]$AMBUAMB
      rand_interaction1_1st <- ranef(lmer_firsthalf)[['item']]$`AMBUAMB:SZM1`
      rand_interaction1_2nd <- ranef(lmer_secondhalf)[['item']]$`AMBUAMB:SZM1`
      rand_interaction2_1st <- ranef(lmer_firsthalf)[['item']]$`AMBUAMB:SZM2`
      rand_interaction2_2nd <- ranef(lmer_secondhalf)[['item']]$`AMBUAMB:SZM2`
      corr_each <- c(corr_each,cor.test(fixed_AMBUAMB_1st+rand_AMBUAMB_1st,fixed_AMBUAMB_2nd+rand_AMBUAMB_2nd)$estimate,
                     cor.test(fixed_AMBUAMB_1st+fixed_interaction1_1st+rand_AMBUAMB_1st+rand_interaction1_1st,fixed_AMBUAMB_2nd+fixed_interaction1_2nd+rand_AMBUAMB_2nd+rand_interaction1_2nd)$estimate,
                     cor.test(fixed_AMBUAMB_1st+fixed_interaction2_1st+rand_AMBUAMB_1st+rand_interaction2_1st,fixed_AMBUAMB_2nd+fixed_interaction2_2nd+rand_AMBUAMB_2nd+rand_interaction2_2nd)$estimate)
      size_each <- c(size_each,Size,Size,Size)
      type <- c(type,"MVRR","NPS","NPZ")
    }
  }
  results <- data.frame(corr_each=corr_each,size_each=size_each,type=type) 
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=mean))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=sd))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=is.na))
  saveRDS(results,"split_half_ceiling_results_GP_simple_P1.rds")
}