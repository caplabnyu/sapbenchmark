library(dplyr)
library(lme4)
library(ggplot2)
library(brms)
library(reshape2)
library(egg)
library(gridExtra)
library(grid)
library(tidyr)
source("../../util.R")
rt.data <- load_data("Agreement")
rt.data$Type <- as.character(rt.data$Type) # Just in case it's automatically read as a factor
rt.data$Type[rt.data$Type == "AGREE"] <- "AGREE_G"
rt.data <- rt.data %>% separate(Type, c("Type", "pGram"), sep="_")
rt.data$pGram[rt.data$pGram == "UAMB"] <- "G"
rt.data$pGram[rt.data$pGram == "AMB"] <- "U"
rt.data$pGram[rt.data$pGram == "UNG"] <- "U"
rt.data$pGram <- as.factor(rt.data$pGram)
rt.data$Type <- as.factor(rt.data$Type)
rt.data <- rt.data[rt.data$ROI%in%c(0,1,2),]
rt.data$position <- droplevels(as.factor(rt.data$ROI))
contrasts(rt.data$position) <- contr.sum(3)/2
contrasts(rt.data$position)
rt.data$pGram.coded <- recode(rt.data$pGram, "U" = 1, "G" = 0)
rt.data$Type.coded <- recode(rt.data$Type, "AGREE" = 0, "NPZ" = 1)
rt.data$position.coded.1 <- recode(rt.data$position, "0"=0.5, "1"=0, "2"=-0.5)
rt.data$position.coded.2 <- recode(rt.data$position, "0"=0, "1"=0.5, "2"=-0.5)
corr_each <- vector()
size_each <- vector()
type <- vector()
for(Size in 2000){
  for(shuffle_participant in 1:5){
    subsample <- rt.data[rt.data$participant%in%sample(unique(rt.data$participant),Size,replace=F)&rt.data$item%in%unique(rt.data$item[rt.data$Type=="AGREE"]),]
    subsample <- arrange(subsample,participant,Type,item)
    for(shuffle_trial in 1:3){
      counter=0
      timeout=0
      while(counter==0){
        half_name <- sample(unique(subsample$participant),(Size/2),replace=F)
        subsample$splitgroup <- ifelse(subsample$participant%in%half_name,"first","second")
        lmer_firsthalf <- lmer(RT ~ pGram.coded + (1+pGram.coded|| participant) + (1 + pGram.coded|| item),data=subsample[subsample$splitgroup=="first"&subsample$Type=="AGREE"&subsample$position==1,],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        lmer_secondhalf <- lmer(RT ~ pGram.coded  + (1+pGram.coded|| participant) + (1 + pGram.coded|| item),data=subsample[subsample$splitgroup=="second"&subsample$Type=="AGREE"&subsample$position==1,],control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
        random_variance <- c(summary(lmer_firsthalf)$varcor[4]$item[[1]],
                             summary(lmer_secondhalf)$varcor[4]$item[[1]])
        if(!any(random_variance==0)){
          counter=1
        }else{timeout=timeout+1}
      }
      print(timeout)
      fixed_pGram.coded_1st <-fixef(lmer_firsthalf)[2]
      fixed_pGram.coded_2nd <-fixef(lmer_secondhalf)[2]
      rand_pGram.coded_1st <-ranef(lmer_firsthalf)[['item']]$pGram.coded
      rand_pGram.coded_2nd <-ranef(lmer_secondhalf)[['item']]$pGram.coded
      corr_each <- c(corr_each,cor.test(fixed_pGram.coded_1st+rand_pGram.coded_1st,fixed_pGram.coded_2nd+rand_pGram.coded_2nd)$estimate)
      size_each <- c(size_each,Size)
      type <- c(type,"ARG_P1")
    }
  }
  results <- data.frame(corr_each=corr_each,size_each=size_each,type=type)
  print(results)
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=mean,na.rm=T))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=sd))
  print(aggregate(results$corr_each,by=list(results$size_each,results$type),FUN=is.na))
  saveRDS(results,"split_half_ceiling_results_Agreement_simple_P1.rds")
}
