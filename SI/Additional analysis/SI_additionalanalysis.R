library(dplyr)
library(rlist)
source("../../analysis/shared/util.R")
rt.data <- load_data("ClassicGP")
by_item <- readRDS("../../plots/spr/ClassicGP/by_item.rds")
ClassicGP_plausibility <- readRDS("plausibility_norming.rds")
by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"ClassicGP")
by_item_surprisalmerged <- left_join(by_item_surprisalmerged,ClassicGP_plausibility[,c('item','coef','meanrating_whole','meanrating_local','meanrating_ultimate')],by=c('item','coef'))
verb_bias <- read.csv("verbbias.csv",header=T)
by_item_surprisalmerged <- left_join(by_item_surprisalmerged,verb_bias[,2:5])
by_item_surprisalmerged$coef <- ifelse(by_item_surprisalmerged$coef=="GPE_MVRR","MV/RRC", ifelse(
  by_item_surprisalmerged$coef=="GPE_NPS","DO/Sent","Trans/Intrans"
))


par(mfcol=c(1,3))
lstmxgpt2 <- list()
for(j in unique(by_item_surprisalmerged$coef)){
    print(paste("coef=",j))
    x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model=="lstm"]
    y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model=="gpt2"]
    if(any(y!=x)){print("items not aligned!!!")
    }else{
      lstm = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model=="lstm"]
      gpt2 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model=="gpt2"]
      LM = lm(lstm~gpt2)
      print(cor.test(lstm,gpt2)$estimate)
      lstmxgpt2 <- list.append(lstmxgpt2,LM)
      print(summary(LM))}
    print("---------------------------------------------------------")
    plot(lstm~gpt2,pch=16,main=paste(j,"Corr=",round(cor.test(lstm,gpt2)$estimate,2)),xlab="Surp diff (GPT-2)",ylab="Surp diff (LSTM)",xlim=range(by_item_surprisalmerged$surprisal_diff),ylim=range(by_item_surprisalmerged$surprisal_diff),cex=1.5,cex.lab=1.2)
}


par(mfcol=c(2,3))
surp_only_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x1))
        print(cor.test(y,x1)$estimate)
        surp_only_lm <- list.append(surp_only_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
      if(k%in%c("lstm","gpt2")){
        plot(y~x1,pch=16,main=paste(j,"Corr=",round(cor.test(y,x1)$estimate,2)),xlab=paste("Surp diff at disambiguating verb,\n LM=",k),ylab="Garden path effect size",xlim=range(by_item_surprisalmerged$surprisal_diff),ylim=range(by_item_surprisalmerged$mean),cex=1.5,cex.lab=1.2)
      }
    }
  }
}
par(mfcol=c(1,3))
plaus_only_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x2))
        print(cor.test(y,x2)$estimate)
        plaus_only_lm <- list.append(plaus_only_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
      if(k%in%c("lstm")){
        plot(y~x2,pch=16,main=paste(j,"Corr=",round(cor.test(y,x2)$estimate,2)),xlab="Plausibility of local phrase",ylab="Garden path effect size",xlim=range(by_item_surprisalmerged$meanrating_local),ylim=range(by_item_surprisalmerged$mean),cex=1.5,cex.lab=1.2)
      }
    }
  }
}

par(mfcol=c(1,3))
cloze_only_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x3))
        print(cor.test(y,x3)$estimate)
        cloze_only_lm <- list.append(cloze_only_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
      if(k%in%c("lstm")){
        plot(y~x3,pch=16,main=paste(j,"Corr=",round(cor.test(y,x3)$estimate,2)),xlab="Sent/Intrans/ReducedRelative bias (cloze)",ylab="Garden path effect size",xlim=range(by_item_surprisalmerged$cloze_SZMbias,na.rm=T),ylim=range(by_item_surprisalmerged$mean),cex=1.5,cex.lab=1.2)
      }
    }
  }
}
par(mfcol=c(1,3))
corpus_only_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x4))
        print(cor.test(y,x4)$estimate)
        corpus_only_lm <- list.append(corpus_only_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
      if(k%in%c("lstm")){
        plot(y~x4,pch=16,main=paste(j,"Corr=",round(cor.test(y,x4)$estimate,2)),xlab="Sent/Intrans/ReducedRelative bias (COCA)",ylab="Garden path effect size",xlim=range(by_item_surprisalmerged$COCA_SZMbias,na.rm=T),ylim=range(by_item_surprisalmerged$mean),cex=1.5,cex.lab=1.2)
      }
    }
  }
}
par(mfcol=c(1,3))
ultimateplaus_only_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        if(j=="GPE_NPS"){
        }else{
          y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
          x5 = by_item_surprisalmerged$meanrating_ultimate[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
          LM = lm(y~scale(x5))
          print(cor.test(y,x5)$estimate)
          ultimateplaus_only_lm <- list.append(ultimateplaus_only_lm,LM)
          print(summary(LM))
          print("---------------------------------------------------------")}
        if(k=="lstm"&j!="GPE_NPS"){
          plot(y~x5,pch=16,main=paste(j,"Corr=",round(cor.test(y,x5)$estimate,2)),xlab="Plausibility of ultimate interpretation",ylab="Garden path effect size",xlim=range(by_item_surprisalmerged$meanrating_ultimate,na.rm=T),ylim=range(by_item_surprisalmerged$mean),cex=1.5,cex.lab=1.2)
        }
      }
    }
  }
}
surp_plus_plaus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x1)+scale(x2))
        surp_plus_plaus_lm <- list.append(surp_plus_plaus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
for(i in 1:length(surp_only_lm)){
  print(anova(surp_only_lm[i][[1]],surp_plus_plaus_lm[i][[1]]))
}
for(i in 1:length(plaus_only_lm)){
  print(anova(plaus_only_lm[i][[1]],surp_plus_plaus_lm[i][[1]]))
}
surp_corr_plaus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(x1~x2)
        print(cor.test(x1,x2)$estimate)
        surp_corr_plaus_lm <- list.append(surp_corr_plaus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
surp_corr_cloze_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(x1~x3)
        print(cor.test(x1,x3)$estimate)
        surp_corr_cloze_lm <- list.append(surp_corr_cloze_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
plaus_corr_cloze_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(x2~x3)
        print(cor.test(x2,x3)$estimate)
        plaus_corr_cloze_lm <- list.append(plaus_corr_cloze_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
cloze_corr_corpus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(x3~x4)
        print(cor.test(x3,x4)$estimate)
        cloze_corr_corpus_lm <- list.append(cloze_corr_corpus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
plaus_corr_corpus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(x2~x4)
        print(cor.test(x2,x4)$estimate)
        plaus_corr_corpus_lm <- list.append(plaus_corr_corpus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
surp_corr_corpus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(x1~x4)
        print(cor.test(x1,x4)$estimate)
        surp_corr_corpus_lm <- list.append(surp_corr_corpus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
surp_plus_cloze_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x1)+scale(x3))
        surp_plus_cloze_lm <- list.append(surp_plus_cloze_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
surp_plus_corpus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x1)+scale(x4))
        surp_plus_corpus_lm <- list.append(surp_plus_corpus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
plaus_plus_cloze_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x2)+scale(x3))
        plaus_plus_cloze_lm <- list.append(plaus_plus_cloze_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
plaus_plus_corpus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x2)+scale(x4))
        plaus_plus_corpus_lm <- list.append(plaus_plus_corpus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
cloze_plus_corpus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x3)+scale(x4))
        cloze_plus_corpus_lm <- list.append(cloze_plus_corpus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
surp_plus_plaus_plus_cloze_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x1)+scale(x2)+scale(x3))
        surp_plus_plaus_plus_cloze_lm <- list.append(surp_plus_plaus_plus_cloze_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}


surp_plus_plaus_plus_cloze_plus_corpus_lm <- list()
for(i in 1){
  for(j in unique(by_item_surprisalmerged$coef)){
    for(k in unique(by_item_surprisalmerged$model)){
      print(paste("ROI=",i))
      print(paste("coef=",j))
      print(paste("model=",k))
      y = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      x = by_item_surprisalmerged$item[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
      if(any(y!=x)){print("items not aligned!!!")
      }else{
        y = by_item_surprisalmerged$mean[by_item_surprisalmerged$ROI==i&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x1 = by_item_surprisalmerged$surprisal_diff[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x2 = by_item_surprisalmerged$meanrating_local[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x3 = by_item_surprisalmerged$cloze_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        x4 = by_item_surprisalmerged$COCA_SZMbias[by_item_surprisalmerged$ROI==0&by_item_surprisalmerged$coef==j&by_item_surprisalmerged$model==k]
        LM = lm(y~scale(x1)+scale(x2)+scale(x3)+scale(x4))
        surp_plus_plaus_plus_cloze_plus_corpus_lm <- list.append(surp_plus_plaus_plus_cloze_plus_corpus_lm,LM)
        print(summary(LM))}
      print("---------------------------------------------------------")
    }
  }
}
