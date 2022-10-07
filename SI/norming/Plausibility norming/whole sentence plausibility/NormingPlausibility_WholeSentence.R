options(digits=15) #for exact RT
library(dplyr)
library(ggplot2)
library(writexl)
library(ordinal)

mycols = c("Time","MD5","Controller","itemrownumb","Element","Type","Group","PennElementType","PennElementName","Parameter","Value","EventTime","Sentence","List","item","Comments")
results=read.csv("NormPlaus_WholeSentence_results.csv",header = 0, sep = ",", comment.char = "#", stringsAsFactors = F,col.names = mycols)
unique(results$MD5) #N=64
worker_id <- results[results$Parameter=="prolific","Value"]
worker_id <- as.data.frame(worker_id)
worker_id$MD5 <- unique(results$MD5)
unique(worker_id$worker_id)
results <- full_join(results,worker_id)
#antibot = questions for detecting robots
antibot <- results[results$Type=="debrief"&grepl("debrief",results$Parameter),c("Value","Parameter","worker_id")]
maintrials <- results[!results$Type%in%c("consent","intro1","intro2","debrief"),]

jud_index <- as.numeric()
for(i in 1:nrow(maintrials)){
  if(i%%3==2){
    jud_index <- c(i, jud_index)
  }
}
judgment <- maintrials[jud_index,]
judgment$Value <- as.numeric(judgment$Value)
maintrials <- maintrials[-jud_index,]
rm(jud_index)
RT <- as.numeric()
for(i in 1:nrow(maintrials)){
  if(i%%2==1){
    RT <- c(maintrials[i+1,'EventTime']-maintrials[i,'EventTime'],RT)
  }
}
judgment$RT <- RT
arrange(aggregate(judgment$RT, by=list(judgment$worker_id),FUN=mean),x)
aggregate(judgment$RT, by=list(judgment$Type),FUN=mean)
plot(judgment$RT, judgment$Value)
judgment[judgment$RT>=200000,] <- NA
arrange(aggregate(judgment$RT, by=list(judgment$worker_id),FUN=mean),x)
#outliers
ol <- 3*sd(judgment$RT,na.rm=T)+mean(judgment$RT,na.rm=T)
nrow(judgment[judgment$RT>=ol,])
#judgment[judgment$RT>=ol,]$RT <- NA
plot(judgment$RT[judgment$RT<ol], judgment$Value[judgment$RT<ol])

Plausible <- judgment[startsWith(judgment$Type,"Plau"),]
Plausibleitem_mean <- aggregate(Plausible$Value,by=list(Plausible$Type,Plausible$Sentence,Plausible$item),FUN=mean)
colnames(Plausibleitem_mean)[4] <- "meanrating"
Plausibleitem_mean <- full_join(Plausibleitem_mean,aggregate(Plausible$Value,by=list(Plausible$Type,Plausible$Sentence,Plausible$item),FUN=length))
colnames(Plausibleitem_mean)[5] <- "numbofpeople"
Plausibleitem_mean <- full_join(Plausibleitem_mean,aggregate(Plausible$Value,by=list(Plausible$Type,Plausible$Sentence,Plausible$item),FUN=sd))
colnames(Plausibleitem_mean)[6] <- "sd"
Plausibleitem_mean <- full_join(Plausibleitem_mean,aggregate(Plausible$RT,by=list(Plausible$Type,Plausible$Sentence,Plausible$item),FUN=mean))
colnames(Plausibleitem_mean)[c(1,2,3,7)] <- c("Type","Sentence","ItemNumber","RT")
Plausibleitem_mean <- arrange(Plausibleitem_mean,ItemNumber, Type)
cor.test(Plausible$RT,Plausible$Value)
write.csv(Plausibleitem_mean,"plausible.csv")

Implausible <- judgment[startsWith(judgment$Type,"Implau"),]
Implausibleitem_mean <- aggregate(Implausible$Value,by=list(Implausible$Type,Implausible$Sentence,Implausible$item),FUN=mean)
colnames(Implausibleitem_mean)[4] <- "meanrating"
Implausibleitem_mean <- full_join(Implausibleitem_mean,aggregate(Implausible$Value,by=list(Implausible$Type,Implausible$Sentence,Implausible$item),FUN=length))
colnames(Implausibleitem_mean)[5] <- "numbofpeople"
Implausibleitem_mean <- full_join(Implausibleitem_mean,aggregate(Implausible$Value,by=list(Implausible$Type,Implausible$Sentence,Implausible$item),FUN=sd))
colnames(Implausibleitem_mean)[6] <- "sd"
Implausibleitem_mean <- full_join(Implausibleitem_mean,aggregate(Implausible$RT,by=list(Implausible$Type,Implausible$Sentence,Implausible$item),FUN=mean))
colnames(Implausibleitem_mean)[c(1,2,3,7)] <- c("Type","Sentence","ItemNumber","RT")
Implausibleitem_mean <- arrange(Implausibleitem_mean,ItemNumber, Type)
write.csv(Implausibleitem_mean,"implausible.csv")

agreement <- judgment[endsWith(judgment$Type,"NPZZ"),]
agreementitem_mean <- aggregate(agreement$Value,by=list(agreement$Type,agreement$Sentence,agreement$item),FUN=mean)
colnames(agreementitem_mean)[4] <- "meanrating"
agreementitem_mean <- full_join(agreementitem_mean,aggregate(agreement$Value,by=list(agreement$Type,agreement$Sentence,agreement$item),FUN=length))
colnames(agreementitem_mean)[5] <- "numbofpeople"
agreementitem_mean <- full_join(agreementitem_mean,aggregate(agreement$Value,by=list(agreement$Type,agreement$Sentence,agreement$item),FUN=sd))
colnames(agreementitem_mean)[6] <- "sd"
agreementitem_mean <- full_join(agreementitem_mean,aggregate(agreement$RT,by=list(agreement$Type,agreement$Sentence,agreement$item),FUN=mean))
colnames(agreementitem_mean)[c(1,2,3,7)] <- c("Type","Sentence","ItemNumber","RT")
agreementitem_mean <- arrange(agreementitem_mean,ItemNumber,Type)
agreementitem_mean <- rbind(agreementitem_mean,Implausibleitem_mean[Implausibleitem_mean$Type=="ImplauNPZ",])
write.csv(agreementitem_mean,"agreement.csv")

Plausible$Value <- as.character(Plausible$Value)
Plausible$contrast1 <- ifelse(Plausible$Type=="PlauNPS",1,0)
Plausible$contrast2<- ifelse(Plausible$Type=="PlauNPZ",1,0)
#contrast1 NPS-MVRR; contrast2 NPZ-MVRR
summary(clmm(as.factor(Value)~contrast1+contrast2+(1+contrast1+contrast2|worker_id),data=Plausible,link="probit"))
aggregate(Plausibleitem_mean$meanrating,by=list(Plausibleitem_mean$Type),FUN=mean)

NPZ <- judgment[judgment$Type%in%c("PlauNPZ","SingNPZZ","PluralNPZZ"),]
NPZ$contrast1 <- ifelse(grepl("Plau",NPZ$Type),1,0)
NPZ$contrast2 <- ifelse(grepl("Plural",NPZ$Type),1,0)
NPZ$Value <- as.character(NPZ$Value)
summary(clmm(as.factor(Value)~contrast1+contrast2+(1+contrast1+contrast2|worker_id),data=NPZ,link="probit"))
aggregate(agreementitem_mean$meanrating,by=list(agreementitem_mean$Type),FUN=mean)


mean_pairwise <- rbind(Plausibleitem_mean,Implausibleitem_mean)
ggplot(mean_pairwise[grepl("NPS",mean_pairwise$Type),], aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')
ggplot(mean_pairwise[grepl("NPZ",mean_pairwise$Type),], aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')
ggplot(mean_pairwise[grepl("MVRR",mean_pairwise$Type),], aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')
ggplot(agreementitem_mean, aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')
ggplot(mean_pairwise[grepl("NPS",mean_pairwise$Type),], aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')


itemwisemean <- aggregate(judgment$Value,by=list(judgment$Type,judgment$Sentence,judgment$item),FUN=mean)
colnames(itemwisemean)[4] <- "meanrating"
itemwisemean <- full_join(itemwisemean,aggregate(judgment$Value,by=list(judgment$Type,judgment$Sentence,judgment$item),FUN=length))
colnames(itemwisemean)[5] <- "numbofpeople"
itemwisemean <- full_join(itemwisemean,aggregate(judgment$Value,by=list(judgment$Type,judgment$Sentence,judgment$item),FUN=sd))
colnames(itemwisemean)[c(1,2,3,6)] <- c("Type","Sentence","item","sd")
itemwisemean$SE <- itemwisemean$sd/sqrt(itemwisemean$numbofpeople)
ggplot(itemwisemean[grepl("NPS",itemwisemean$Type),], aes(x=Item, y=meanrating, colour=Type)) + 
  geom_errorbar(aes(ymin=meanrating-SE, ymax=meanrating+SE), width=.1) +
  geom_point()
ggplot(itemwisemean[grepl("auNPZ",itemwisemean$Type),], aes(x=Item, y=meanrating, colour=Type)) + 
  geom_errorbar(aes(ymin=meanrating-SE, ymax=meanrating+SE), width=.1) +
  geom_point()
ggplot(itemwisemean[grepl("MVRR",itemwisemean$Type),], aes(x=Item, y=meanrating, colour=Type)) + 
  geom_errorbar(aes(ymin=meanrating-SE, ymax=meanrating+SE), width=.1) +
  geom_point()
ggplot(itemwisemean[grepl("NPZZ",itemwisemean$Type)|itemwisemean$Type=="ImplauNPZ",], aes(x=Item, y=meanrating, colour=Type)) + 
  geom_errorbar(aes(ymin=meanrating-SE, ymax=meanrating+SE), width=.1) +
  geom_point()

a <- judgment %>% group_by(worker_id) %>% mutate(Zvalue = scale(Value))
item_mean_z <- arrange(aggregate(a$Zvalue,by=list(a$Type,a$Sentence),FUN=mean),Group.1,V1)
colnames(item_mean_z) <- c("Type","Sentence","z-score")
write.csv(left_join(itemwisemean[!grepl("Impl",itemwisemean$Type),],item_mean_z[-(1:72),]),"item_mean_wholesentenceplaus.csv",row.names=F)


#check if the very low rating is due to some outlier subjects
bysubject_mean_g <- arrange(aggregate(judgment$Value, by=list(judgment$worker_id),FUN=mean),x)
conservative_raters_g <- bysubject_mean_g$Group.1[bysubject_mean_g$x<=4]
ratings_by_the_conservatives_g <- judgment[judgment$worker_id%in%conservative_raters_g,]

bysubject_mean_p <- arrange(aggregate(judgment$Value[!grepl("Implau",judgment$Type)], by=list(judgment$worker_id[!grepl("Implau",judgment$Type)]),FUN=mean),x)
conservative_raters_p <- bysubject_mean_p$Group.1[bysubject_mean_p$x<=5]
ratings_by_the_conservatives_p <- judgment[judgment$worker_id%in%conservative_raters_p,]
ratings_by_the_conservatives_p <- arrange(ratings_by_the_conservatives_p,worker_id,Type)


bad_items_Plau <- Plausibleitem_mean$Sentence[Plausibleitem_mean$meanrating<=5]
bad_items_agreement <- agreementitem_mean$Sentence[agreementitem_mean$meanrating<=5&agreementitem_mean$Type!="ImplauNPZ"]

for(i in bad_items_Plau){
  print(i)
  print(sum(Plausible$worker_id[Plausible$Sentence%in%i]%in%conservative_raters_p)/length(Plausible$worker_id[Plausible$Sentence%in%i]))}

for(i in bad_items_agreement){
  print(i)
  print(sum(agreement$worker_id[agreement$Sentence%in%i]%in%conservative_raters_p)/length(agreement$worker_id[agreement$Sentence%in%i]))}