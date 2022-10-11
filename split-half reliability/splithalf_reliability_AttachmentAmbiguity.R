library(dplyr)
library(lme4)
library(ggplot2)
library(brms)
library(reshape2)
library(egg)
library(gridExtra)
library(grid)

source("../../one/two/three/util.R")
rt.data <- load_data("AttachmentAmbiguity")

rt.data$ambiguity <- ifelse(rt.data$AMBIG=="Amb", 2/3,-1/3)
rt.data$height <-  case_when(rt.data$Type == "AttachMulti" ~0,
                             rt.data$Type=="AttachLow" ~ -1/2,
                             rt.data$Type=="AttachHigh" ~ 1/2)
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
        lmer_firsthalf <- lmer(RT ~ ambiguity+height+(1+ambiguity+height||item)+(1+ambiguity+height||participant),data=subsample[subsample$splitgroup=="first",],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        lmer_secondhalf <- lmer(RT ~ ambiguity+height+(1+ambiguity+height||item)+(1+ambiguity+height||participant),data=subsample[subsample$splitgroup=="second",],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        random_variance <- c(summary(lmer_firsthalf)$varcor[4]$item[[1]],summary(lmer_firsthalf)$varcor[5]$item.1[[1]],
                             summary(lmer_secondhalf)$varcor[4]$item[[1]],summary(lmer_secondhalf)$varcor[5]$item.1[[1]])
        if(!any(random_variance==0)){
          counter=1
        }else{timeout=timeout+1}
      }
      print(timeout)
      fixed_ambiguity_1st <-fixef(lmer_firsthalf)[2]
      fixed_ambiguity_2nd <-fixef(lmer_secondhalf)[2]
      fixed_height_1st <-fixef(lmer_firsthalf)[3]
      fixed_height_2nd <-fixef(lmer_secondhalf)[3]
      rand_ambiguity_1st <- ranef(lmer_firsthalf)[['item']]$ambiguity
      rand_ambiguity_2nd <- ranef(lmer_secondhalf)[['item']]$ambiguity
      rand_height_1st <- ranef(lmer_firsthalf)[['item']]$height
      rand_height_2nd <- ranef(lmer_secondhalf)[['item']]$height
      corr_each <- c(corr_each,cor.test((-1)*fixed_ambiguity_1st-(1/2)*fixed_height_1st-rand_ambiguity_1st-(1/2)*rand_height_1st,
                                        (-1)*fixed_ambiguity_2nd-(1/2)*fixed_height_2nd-rand_ambiguity_2nd-(1/2)*rand_height_2nd)$estimate,
                     cor.test((-1)*fixed_ambiguity_1st+(1/2)*fixed_height_1st-rand_ambiguity_1st+(1/2)*rand_height_1st,
                              (-1)*fixed_ambiguity_2nd+(1/2)*fixed_height_2nd-rand_ambiguity_2nd+(1/2)*rand_height_2nd)$estimate)
      size_each <- c(size_each,Size,Size)
      type <- c(type,"GPE_low","GPE_high")
    }
  }
  results <- data.frame(corr_each=corr_each,size_each=size_each,type=type) 
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=mean))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=sd))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=is.na))
  saveRDS(results,"split_half_ceiling_results_AA_simple_P1.rds")
}