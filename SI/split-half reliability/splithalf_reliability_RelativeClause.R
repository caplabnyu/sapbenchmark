library(dplyr)
library(lme4)
library(ggplot2)
library(brms)
library(reshape2)
library(egg)
library(gridExtra)
library(grid)
source("../../util.R")
rt.data <- load_data("RelativeClause") 

filler.data <- load_data("Fillers") 

position_fit_lmer_nocor <- lmer(RT ~ scale(WordPosition) + (1 + scale(WordPosition) || participant), data=filler.data)
summary(position_fit_lmer_nocor)

rt.data$wordpos_predrt <- predict(position_fit_lmer_nocor, rt.data)

rt.data$corrected_rt <- rt.data$RT - rt.data$wordpos_predrt


rt.data$Type <-  factor(rt.data$Type, levels = c('RC_Subj', 'RC_Obj'))
rt.data$Type_num  <-  ifelse(rt.data$Type == 'RC_Subj', 0, 1)

corr_each <- vector()
size_each <- vector()
type <- vector()
for(Size in 2000){
  for(shuffle_participant in 1:5){
    subsample <- rt.data[rt.data$participant%in%sample(unique(rt.data$participant),Size,replace=F)&rt.data$ROI==0,]
    subsample <- arrange(subsample,participant,Type,item)
    for(shuffle_trial in 1:3){
      counter=0
      timeout=0
      while(counter==0){
        half_name <- sample(unique(subsample$participant),(Size/2),replace=F)
        subsample$splitgroup <- ifelse(subsample$participant%in%half_name,"first","second")
        lmer_firsthalf <- lmer(corrected_rt ~ Type_num+(1+Type_num||item)+(1+Type_num||participant),data=subsample[subsample$splitgroup=="first",],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        lmer_secondhalf <- lmer(corrected_rt ~ Type_num+(1+Type_num||item)+(1+Type_num||participant),data=subsample[subsample$splitgroup=="second",],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        random_variance <- c(summary(lmer_firsthalf)$varcor[3]$item[[1]],
                             summary(lmer_secondhalf)$varcor[3]$item[[1]])
        if(!any(random_variance==0)){
          counter=1
        }else{timeout=timeout+1}
      }
      print(timeout)
      fixed_Type_num_1st <-fixef(lmer_firsthalf)[2]
      fixed_Type_num_2nd <-fixef(lmer_secondhalf)[2]
      rand_Type_num_1st <- ranef(lmer_firsthalf)[['item']]$Type_num
      rand_Type_num_2nd <- ranef(lmer_secondhalf)[['item']]$Type_num
      corr_each <- c(corr_each,cor.test(fixed_Type_num_1st+rand_Type_num_1st,fixed_Type_num_2nd+rand_Type_num_2nd)$estimate)
      size_each <- c(size_each,Size)
      type <- c(type,"RC")
    }
  }
  results <- data.frame(corr_each=corr_each,size_each=size_each,type=type) 
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=mean))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=sd))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=is.na))
  saveRDS(results,"split_half_ceiling_results_RC_simple_P0.rds")
}
