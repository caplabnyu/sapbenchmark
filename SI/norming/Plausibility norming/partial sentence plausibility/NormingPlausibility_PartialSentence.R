options(digits=15) #for exact RT
library(dplyr)
library(plyr)
library(ggplot2)
library(writexl)
library(ordinal)

mycols = c("Time","MD5","Controller","itemrownumb","Element","Type","Group","PennElementType","PennElementName","Parameter","Value","EventTime","Sentence","List","item","Localphrase","Comments")
results=read.csv("NormPlaus_PartialSentence_results.csv",header = 0, sep = ",", comment.char = "#", stringsAsFactors = F,col.names = mycols)
antibot <- results[results$Type=="debrief"&grepl("debrief",results$Parameter),c("MD5","Value","Parameter")]
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
aggregate(judgment$RT, by=list(judgment$MD5,judgment$List),FUN=length)
aggregate(judgment$RT, by=list(judgment$MD5),FUN=mean)
aggregate(judgment$Value, by=list(judgment$MD5),FUN=mean)
aggregate(judgment$RT, by=list(judgment$Type),FUN=mean,na.rm=T)
plot(judgment$RT, judgment$Value)
judgment[judgment$RT>=300000,]$Value <- NA
ol <- 3*sd(judgment$RT,na.rm=T)+mean(judgment$RT,na.rm=T)
nrow(judgment[judgment$RT>=ol,])
#judgment[judgment$RT>=ol,]$RT <- NA
plot(judgment$RT[judgment$RT<ol], judgment$Value[judgment$RT<ol])

judgment$COND <- ifelse(startsWith(judgment$Type,"implau"),"implau","plau")

aggregate(judgment$Value,by=list(judgment$Type),FUN=mean,na.rm=T)
filler <- judgment[startsWith(judgment$Type,"implau"),]
critical <- judgment[!startsWith(judgment$Type,"implau"),]
aggregate(critical$Value,by=list(critical$Sentence,critical$item),FUN=mean,na.rm=T)


ultimate <- judgment[startsWith(judgment$Type,"ultimate"),]
aggregate(ultimate$Value,by=list(ultimate$Type),FUN=mean,na.rm=T)
byitem_ultimate_NPZ <- aggregate(ultimate$Value[ultimate$Type=="ultimate_NPZ"],by=list(ultimate$Sentence[ultimate$Type=="ultimate_NPZ"],ultimate$item[ultimate$Type=="ultimate_NPZ"]),FUN=mean,na.rm=T)
byitem_ultimate_NPZ$sd <- (aggregate(ultimate$Value[ultimate$Type=="ultimate_NPZ"],by=list(ultimate$Sentence[ultimate$Type=="ultimate_NPZ"],ultimate$item[ultimate$Type=="ultimate_NPZ"]),FUN=sd,na.rm=T))$x
byitem_ultimate_pres_NPZ <- aggregate(ultimate$Value[ultimate$Type=="ultimate_pres_NPZ"],by=list(ultimate$Sentence[ultimate$Type=="ultimate_pres_NPZ"],ultimate$item[ultimate$Type=="ultimate_pres_NPZ"]),FUN=mean,na.rm=T)
byitem_ultimate_pres_NPZ$sd <- (aggregate(ultimate$Value[ultimate$Type=="ultimate_pres_NPZ"],by=list(ultimate$Sentence[ultimate$Type=="ultimate_pres_NPZ"],ultimate$item[ultimate$Type=="ultimate_pres_NPZ"]),FUN=sd,na.rm=T))$x
byitem_ultimate_MVRR <- aggregate(ultimate$Value[ultimate$Type=="ultimate_MVRR"],by=list(ultimate$Sentence[ultimate$Type=="ultimate_MVRR"],ultimate$item[ultimate$Type=="ultimate_MVRR"]),FUN=mean,na.rm=T)
byitem_ultimate_MVRR$sd <- (aggregate(ultimate$Value[ultimate$Type=="ultimate_MVRR"],by=list(ultimate$Sentence[ultimate$Type=="ultimate_MVRR"],ultimate$item[ultimate$Type=="ultimate_MVRR"]),FUN=sd,na.rm=T))$x
byitem_ultimate_NPZ$Type <- "NPZ"
byitem_ultimate_pres_NPZ$Type <- "pres_NPZ"
byitem_ultimate_MVRR$Type <- "MVRR"
colnames(byitem_ultimate_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_ultimate_pres_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_ultimate_MVRR) <- c("Sentence","item","meanrating","sd","Type")
byitem_ultimate_MVRR$meanrating <- as.numeric(byitem_ultimate_MVRR$meanrating)
byitem_ultimate_MVRR$sd <- as.numeric(byitem_ultimate_MVRR$sd)
byitem_ultimate_MVRR$item <- as.numeric(byitem_ultimate_MVRR$item)
byitem_ultimate_MVRR <- arrange(byitem_ultimate_MVRR,item)
itemwisemean_ultimate <- rbind(byitem_ultimate_NPZ,byitem_ultimate_pres_NPZ,byitem_ultimate_MVRR)
ggplot(itemwisemean_ultimate, aes(x=item, y=meanrating, colour=Type)) + 
  geom_errorbar(aes(ymin=meanrating-1.96*(sd/(sqrt(4))),ymax=meanrating+1.96*(sd/(sqrt(4)))), width=.1)+
  geom_point(size=3)+labs(title="ultimate")
ggplot(itemwisemean_ultimate, aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')+
  scale_fill_discrete(name="ultimate interpretation",labels = c("MVRR=5.80", "NPZ_past=6.07", "NPZ_present=5.71"))




mainclause <- judgment[startsWith(judgment$Type,"mainclause"),]
byitem_mainclause_NPZ <- aggregate(mainclause$Value[mainclause$Type=="mainclause_NPZ"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_NPZ"],mainclause$item[mainclause$Type=="mainclause_NPZ"]),FUN=mean,na.rm=T)
byitem_mainclause_NPZ$sd <- (aggregate(mainclause$Value[mainclause$Type=="mainclause_NPZ"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_NPZ"],mainclause$item[mainclause$Type=="mainclause_NPZ"]),FUN=sd,na.rm=T))$x
byitem_mainclause_singular_NPZ <- aggregate(mainclause$Value[mainclause$Type=="mainclause_singular_NPZ"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_singular_NPZ"],mainclause$item[mainclause$Type=="mainclause_singular_NPZ"]),FUN=mean,na.rm=T)
byitem_mainclause_singular_NPZ$sd <- (aggregate(mainclause$Value[mainclause$Type=="mainclause_singular_NPZ"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_singular_NPZ"],mainclause$item[mainclause$Type=="mainclause_singular_NPZ"]),FUN=sd,na.rm=T))$x
byitem_mainclause_plural_NPZ <- aggregate(mainclause$Value[mainclause$Type=="mainclause_plural_NPZ"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_plural_NPZ"],mainclause$item[mainclause$Type=="mainclause_plural_NPZ"]),FUN=mean,na.rm=T)
byitem_mainclause_plural_NPZ$sd <- (aggregate(mainclause$Value[mainclause$Type=="mainclause_plural_NPZ"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_plural_NPZ"],mainclause$item[mainclause$Type=="mainclause_plural_NPZ"]),FUN=sd,na.rm=T))$x
byitem_mainclause_NPS <- aggregate(mainclause$Value[mainclause$Type=="mainclause_NPS"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_NPS"],mainclause$item[mainclause$Type=="mainclause_NPS"]),FUN=mean,na.rm=T)
byitem_mainclause_NPS$sd <- (aggregate(mainclause$Value[mainclause$Type=="mainclause_NPS"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_NPS"],mainclause$item[mainclause$Type=="mainclause_NPS"]),FUN=sd,na.rm=T))$x
byitem_mainclause_MVRR <- aggregate(mainclause$Value[mainclause$Type=="mainclause_MVRR"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_MVRR"],mainclause$item[mainclause$Type=="mainclause_MVRR"]),FUN=mean,na.rm=T)
byitem_mainclause_MVRR$sd <- (aggregate(mainclause$Value[mainclause$Type=="mainclause_MVRR"],by=list(mainclause$Sentence[mainclause$Type=="mainclause_MVRR"],mainclause$item[mainclause$Type=="mainclause_MVRR"]),FUN=sd,na.rm=T))$x
same_for_NPS <- byitem_mainclause_NPZ[byitem_mainclause_NPZ$Group.2=="10"|byitem_mainclause_NPZ$Group.2=="15",]
same_for_NPZ <- byitem_mainclause_NPS[byitem_mainclause_NPS$Group.2=="2"|byitem_mainclause_NPS$Group.2=="5"|byitem_mainclause_NPS$Group.2=="11"|byitem_mainclause_NPS$Group.2=="14"|byitem_mainclause_NPS$Group.2=="17"|byitem_mainclause_NPS$Group.2=="19"|byitem_mainclause_NPS$Group.2=="23",]
byitem_mainclause_NPS <- rbind(byitem_mainclause_NPS,same_for_NPS)
byitem_mainclause_NPZ <- rbind(byitem_mainclause_NPZ,same_for_NPZ)
byitem_mainclause_NPS$Type <- "NPS"
byitem_mainclause_NPZ$Type <- "NPZ"
byitem_mainclause_singular_NPZ$Type <- "singular_NPZ"
byitem_mainclause_plural_NPZ$Type <- "plural_NPZ"
byitem_mainclause_MVRR$Type <- "MVRR"
colnames(byitem_mainclause_NPS) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_mainclause_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_mainclause_singular_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_mainclause_plural_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_mainclause_MVRR) <- c("Sentence","item","meanrating","sd","Type")
byitem_mainclause_NPS <- arrange(byitem_mainclause_NPS,item)
byitem_mainclause_NPZ <- arrange(byitem_mainclause_NPZ,item)
itemwisemean_mainclause <- rbind(byitem_mainclause_NPS,byitem_mainclause_NPZ,byitem_mainclause_singular_NPZ,byitem_mainclause_plural_NPZ,byitem_mainclause_MVRR)
ggplot(itemwisemean_mainclause, aes(x=item, y=meanrating, colour=Type)) +
  geom_errorbar(aes(ymin=meanrating-1.96*(sd/(sqrt(4))),ymax=meanrating+1.96*(sd/(sqrt(4)))), width=.1)+
  geom_point(size=3)+labs(title="mainclause")
ggplot(itemwisemean_mainclause, aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')+
  scale_fill_discrete(name="main_clause",labels = c("MVRR=6.16", "NPS=6.32", "NPZ_past=6.06","NPZ_plural=6.07","NPZ_singular=6.17"))


local <- judgment[startsWith(judgment$Type,"local"),]
byitem_local_NPZ <- aggregate(local$Value[local$Type=="local_NPZ"],by=list(local$Sentence[local$Type=="local_NPZ"],local$item[local$Type=="local_NPZ"]),FUN=mean,na.rm=T)
byitem_local_NPZ$sd <- (aggregate(local$Value[local$Type=="local_NPZ"],by=list(local$Sentence[local$Type=="local_NPZ"],local$item[local$Type=="local_NPZ"]),FUN=sd,na.rm=T))$x
byitem_local_singular_NPZ <- aggregate(local$Value[local$Type=="local_singular_NPZ"],by=list(local$Sentence[local$Type=="local_singular_NPZ"],local$item[local$Type=="local_singular_NPZ"]),FUN=mean,na.rm=T)
byitem_local_singular_NPZ$sd <- (aggregate(local$Value[local$Type=="local_singular_NPZ"],by=list(local$Sentence[local$Type=="local_singular_NPZ"],local$item[local$Type=="local_singular_NPZ"]),FUN=sd,na.rm=T))$x
byitem_local_plural_NPZ <- aggregate(local$Value[local$Type=="local_plural_NPZ"],by=list(local$Sentence[local$Type=="local_plural_NPZ"],local$item[local$Type=="local_plural_NPZ"]),FUN=mean,na.rm=T)
byitem_local_plural_NPZ$sd <- (aggregate(local$Value[local$Type=="local_plural_NPZ"],by=list(local$Sentence[local$Type=="local_plural_NPZ"],local$item[local$Type=="local_plural_NPZ"]),FUN=sd,na.rm=T))$x
byitem_local_NPS <- aggregate(local$Value[local$Type=="local_NPS"],by=list(local$Sentence[local$Type=="local_NPS"],local$item[local$Type=="local_NPS"]),FUN=mean,na.rm=T)
byitem_local_NPS$sd <- (aggregate(local$Value[local$Type=="local_NPS"],by=list(local$Sentence[local$Type=="local_NPS"],local$item[local$Type=="local_NPS"]),FUN=sd,na.rm=T))$x
byitem_local_MVRR <- aggregate(local$Value[local$Type=="local_MVRR"],by=list(local$Sentence[local$Type=="local_MVRR"],local$item[local$Type=="local_MVRR"]),FUN=mean,na.rm=T)
byitem_local_MVRR$sd <- (aggregate(local$Value[local$Type=="local_MVRR"],by=list(local$Sentence[local$Type=="local_MVRR"],local$item[local$Type=="local_MVRR"]),FUN=sd,na.rm=T))$x
byitem_local_NPS$Type <- "NPS"
byitem_local_NPZ$Type <- "NPZ"
byitem_local_singular_NPZ$Type <- "singular_NPZ"
byitem_local_plural_NPZ$Type <- "plural_NPZ"
byitem_local_MVRR$Type <- "MVRR"
colnames(byitem_local_NPS) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_local_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_local_singular_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_local_plural_NPZ) <- c("Sentence","item","meanrating","sd","Type")
colnames(byitem_local_MVRR) <- c("Sentence","item","meanrating","sd","Type")
itemwisemean_local <- rbind(byitem_local_NPS,byitem_local_NPZ,byitem_local_singular_NPZ,byitem_local_plural_NPZ,byitem_local_MVRR)
ggplot(itemwisemean_local, aes(x=item, y=meanrating, colour=Type)) + 
  geom_errorbar(aes(ymin=meanrating-1.96*(sd/(sqrt(4))),ymax=meanrating+1.96*(sd/(sqrt(4)))), width=.1)+
  geom_point(size=3)+labs(title="local")
ggplot(itemwisemean_local, aes(meanrating, fill = Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')+
  scale_fill_discrete(name="local interpretation",labels = c("MVRR=5.88", "NPS=5.78", "NPZ_past=5.91","NPZ_plural=5.67","NPZ_singular=6.13"))


par(mfrow=c(3,1))
hist(itemwisemean_local$meanrating[itemwisemean_local$Type=="NPS"],breaks=100,main="local plaus NPS",xlab="")
hist(itemwisemean_local$meanrating[itemwisemean_local$Type=="NPZ"],breaks=100,main="local plaus NPZ",xlab="")
hist(itemwisemean_local$meanrating[itemwisemean_local$Type=="MVRR"],breaks=100,main="local plaus MVRR",xlab="")
hist(itemwisemean_mainclause$meanrating[itemwisemean_mainclause$Type=="NPS"],breaks=100,main="mainclause plaus NPS",xlab="")
hist(itemwisemean_mainclause$meanrating[itemwisemean_mainclause$Type=="NPZ"],breaks=100,main="mainclause plaus NPZ",xlab="")
hist(itemwisemean_mainclause$meanrating[itemwisemean_mainclause$Type=="MVRR"],breaks=100,main="mainclause plaus MVRR",xlab="")
hist(itemwisemean_ultimate$meanrating[itemwisemean_ultimate$Type=="NPZ"],breaks=100,main="ultimate plaus NPZ",xlab="")
hist(itemwisemean_ultimate$meanrating[itemwisemean_ultimate$Type=="MVRR"],breaks=100,main="ultimate plaus MVRR",xlab="")


ultimate$contrast1 <- ifelse(ultimate$Type=="ultimate_MVRR",1,0)
ultimate$contrast2 <- ifelse(ultimate$Type=="ultimate_pres_NPZ",1,0)
summary(clmm(as.factor(as.character(Value))~contrast1+contrast2+(1|item)+(1|MD5),data=ultimate,link="probit"))

local$contrast1 <- ifelse(local$Type=="local_NPS",1,ifelse(local$Type=="local_NPZ",0,ifelse(local$Type=="local_MVRR",0,NA)))
local$contrast2 <- ifelse(local$Type=="local_NPZ",1,ifelse(local$Type=="local_NPS",0,ifelse(local$Type=="local_MVRR",0,NA)))
summary(clmm(as.factor(as.character(Value))~contrast1+contrast2+(1|item)+(1|MD5),data=local,link="probit"))
local$contrast3 <- ifelse(local$Type=="local_singular_NPZ",1,ifelse(local$Type=="local_plural_NPZ",0,ifelse(local$Type=="local_NPZ",0,NA)))
local$contrast4 <- ifelse(local$Type=="local_plural_NPZ",1,ifelse(local$Type=="local_singular_NPZ",0,ifelse(local$Type=="local_NPZ",0,NA)))
summary(clmm(as.factor(as.character(Value))~contrast3+contrast4+(1|item)+(1|MD5),data=local,link="probit"))

mainclause$contrast1 <- ifelse(mainclause$Type=="mainclause_NPS",1,ifelse(mainclause$Type=="mainclause_NPZ",0,ifelse(mainclause$Type=="mainclause_MVRR",0,NA)))
mainclause$contrast2 <- ifelse(mainclause$Type=="mainclause_NPZ",1,ifelse(mainclause$Type=="mainclause_NPS",0,ifelse(mainclause$Type=="mainclause_MVRR",0,NA)))
summary(clmm(as.factor(as.character(Value))~contrast1+contrast2+(1|item)+(1|MD5),data=mainclause,link="probit"))
mainclause$contrast3 <- ifelse(mainclause$Type=="mainclause_singular_NPZ",1,ifelse(mainclause$Type=="mainclause_plural_NPZ",0,ifelse(mainclause$Type=="mainclause_NPZ",0,NA)))
mainclause$contrast4 <- ifelse(mainclause$Type=="mainclause_plural_NPZ",1,ifelse(mainclause$Type=="mainclause_singular_NPZ",0,ifelse(mainclause$Type=="mainclause_NPZ",0,NA)))
summary(clmm(as.factor(as.character(Value))~contrast3+contrast4+(1|item)+(1|MD5),data=mainclause,link="probit"))


item_mean_wholesentencepl <- read.csv("item_mean_wholesentenceplaus.csv",header=T)
item_mean_wholesentencepl <- arrange(item_mean_wholesentencepl,Type,item)
cor.test(item_mean_wholesentencepl$meanrating[1:24],byitem_local_MVRR$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[1:24],byitem_ultimate_MVRR$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[1:24],byitem_mainclause_MVRR$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[25:48],byitem_local_NPS$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[25:48],byitem_mainclause_NPS$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[49:72],byitem_local_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[49:72],byitem_ultimate_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[49:72],byitem_mainclause_NPZ$meanrating)


cor.test(byitem_mainclause_singular_NPZ$meanrating,byitem_mainclause_plural_NPZ$meanrating)
cor.test(byitem_local_singular_NPZ$meanrating,byitem_local_plural_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[73:90],byitem_mainclause_plural_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[73:90],byitem_local_plural_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[73:90],byitem_ultimate_pres_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[91:108],byitem_mainclause_singular_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[91:108],byitem_local_singular_NPZ$meanrating)
cor.test(item_mean_wholesentencepl$meanrating[91:108],byitem_ultimate_pres_NPZ$meanrating)

cor.test(item_mean_wholesentencepl$meanrating,c(byitem_local_MVRR$meanrating,byitem_local_NPS$meanrating,byitem_local_NPZ$meanrating,byitem_local_plural_NPZ$meanrating,byitem_local_singular_NPZ$meanrating))
cor.test(item_mean_wholesentencepl$meanrating,c(byitem_mainclause_MVRR$meanrating,byitem_mainclause_NPS$meanrating,byitem_mainclause_NPZ$meanrating,byitem_mainclause_plural_NPZ$meanrating,byitem_mainclause_singular_NPZ$meanrating))
cor.test(item_mean_wholesentencepl$meanrating[c(1:24,49:72)],c(byitem_ultimate_MVRR$meanrating,byitem_ultimate_NPZ$meanrating))



itemwisemean_local$coef <- revalue(itemwisemean_local$Type,c("MVRR"="GPE_MVRR","NPZ"="GPE_NPZ","NPS"="GPE_NPS"))
itemwisemean_ultimate$coef <- revalue(itemwisemean_ultimate$Type,c("MVRR"="GPE_MVRR","NPZ"="GPE_NPZ","NPS"="GPE_NPS"))
item_mean_wholesentencepl$coef <- revalue(item_mean_wholesentencepl$Type,c("PlauMVRR"="GPE_MVRR","PlauNPZ"="GPE_NPZ","PlauNPS"="GPE_NPS"))

saveRDS(itemwisemean_local,"local_plausibility.rds")
saveRDS(itemwisemean_mainclause,"mainclause_plausibility.rds")
saveRDS(itemwisemean_ultimate,"ultimate_plausibility.rds")
saveRDS(item_mean_wholesentencepl[,c(1:5,9)],"wholesentence_plausibility.rds")

compiled_plaus <- left_join(item_mean_wholesentencepl[,c(1:4,6,9)],itemwisemean_local,by=c("item","coef"))
compiled_plaus <- left_join(compiled_plaus,itemwisemean_ultimate,by=c("item","coef"))
compiled_plaus <- compiled_plaus[,c(2:9,11:13)]
colnames(compiled_plaus) <-  c("Sentence_whole","item","meanrating_whole","sd_whole","coef","Sentence_local","meanrating_local","sd_local","Sentence_ultimate","meanrating_ultimate","sd_ultimate")

saveRDS(compiled_plaus,"plausibility_norming.rds")
