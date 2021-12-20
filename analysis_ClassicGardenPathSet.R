library(dplyr)
library(lme4)
#load(".RData")
rt.data <- read.csv("ClassicGardenPathSet.csv",header=T)
rt.data <- rt.data%>%filter(ROI%in%c(-2,-1,0,1,2)) %>%filter(RT<=7000)
rt.data$SZM1 <- ifelse(
  rt.data$CONSTRUCTION=="NPS",1,0
)
rt.data$SZM2 <- ifelse(
  rt.data$CONSTRUCTION=="NPZ",1,0
)
#itemwise (collapsing across constructions, 24*3=72)
rt.data$item72 <- ifelse(rt.data$CONSTRUCTION=="NPS",as.numeric(as.character(rt.data$item)),
                         ifelse(rt.data$CONSTRUCTION=="NPZ",as.numeric(as.character(rt.data$item))+24,as.numeric(as.character(rt.data$item))+48))
rt.data$item72 <- as.factor(rt.data$item72)

#use || because the full models are too complicated
#one lmer for each word position
lmer_0_AMBxCONSTR <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||MD5),data=rt.data[rt.data$ROI==0,])
summary(lmer_0_AMBxCONSTR)
lmer_1_AMBxCONSTR <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||MD5),data=rt.data[rt.data$ROI==1,])
summary(lmer_1_AMBxCONSTR)
lmer_2_AMBxCONSTR <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||MD5),data=rt.data[rt.data$ROI==2,])
summary(lmer_2_AMBxCONSTR)

#one-factor model
lmer_0_item72 <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=rt.data[rt.data$ROI==0,])
summary(lmer_0_item72)
lmer_0_item72_RTacross3words <- lmer(RTacross3words ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=rt.data[rt.data$ROI==0&rt.data$RTacross3words<=7000/3,])
summary(lmer_0_item72_RTacross3words)


#testing reliability of itemwise GPE estimates (split-half analysis)
splithalf0 <- arrange(rt.data[rt.data$ROI==0,],MD5,Type,item)
numberpercondpersubj0 <- aggregate(splithalf0$Time,by=list(splithalf0$MD5,splithalf0$Type),FUN=length)
numberpercondpersubj0$x1 <- round(numberpercondpersubj0$x/2)
numberpercondpersubj0$x2 <- numberpercondpersubj0$x-numberpercondpersubj0$x1
colnames(numberpercondpersubj0) <- c("MD5","Type","totalnumb","numb1","numb2")
splithalf0$splitgroup <- NA
for(i in 1:nrow(numberpercondpersubj0)){
  splithalf0[splithalf0$MD5==numberpercondpersubj0[i,'MD5']&splithalf0$Type==numberpercondpersubj0[i,'Type'],]$splitgroup <- sample(c(rep("first",numberpercondpersubj0[i,'numb1']),rep("second",numberpercondpersubj0[i,'numb2'])))
}
lmer0_firsthalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf0[splithalf0$splitgroup=="first",])
lmer0_secondhalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf0[splithalf0$splitgroup=="second",])
lmer0_total <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf0)
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB,ranef(lmer0_secondhalf)[['item72']]$AMBUAMB)
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB[1:24],ranef(lmer0_secondhalf)[['item72']]$AMBUAMB[1:24])
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB[25:48],ranef(lmer0_secondhalf)[['item72']]$AMBUAMB[25:48])
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB[49:72],ranef(lmer0_secondhalf)[['item72']]$AMBUAMB[49:72])

#do this for ROI1 (spillover1)
splithalf1 <- arrange(rt.data[rt.data$ROI==1,],MD5,Type,item)
numberpercondpersubj1 <- aggregate(splithalf1$Time,by=list(splithalf1$MD5,splithalf1$Type),FUN=length)
numberpercondpersubj1$x1 <- round(numberpercondpersubj1$x/2)
numberpercondpersubj1$x2 <- numberpercondpersubj1$x-numberpercondpersubj1$x1
colnames(numberpercondpersubj1) <- c("MD5","Type","totalnumb","numb1","numb2")
splithalf1$splitgroup <- NA
for(i in 1:nrow(numberpercondpersubj1)){
  splithalf1[splithalf1$MD5==numberpercondpersubj1[i,'MD5']&splithalf1$Type==numberpercondpersubj1[i,'Type'],]$splitgroup <- sample(c(rep("first",numberpercondpersubj1[i,'numb1']),rep("second",numberpercondpersubj1[i,'numb2'])))
}

lmer1_firsthalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf1[splithalf1$splitgroup=="first",])
lmer1_secondhalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf1[splithalf1$splitgroup=="second",])
lmer1_total <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf1)
cor.test(ranef(lmer1_firsthalf)[['item72']]$AMBUAMB,ranef(lmer1_secondhalf)[['item72']]$AMBUAMB)

#for ROI2 (spillover2)
splithalf2 <- arrange(rt.data[rt.data$ROI==2,],MD5,Type,item)
numberpercondpersubj2 <- aggregate(splithalf2$Time,by=list(splithalf2$MD5,splithalf2$Type),FUN=length)
numberpercondpersubj2$x1 <- round(numberpercondpersubj2$x/2)
numberpercondpersubj2$x2 <- numberpercondpersubj2$x-numberpercondpersubj2$x1
colnames(numberpercondpersubj2) <- c("MD5","Type","totalnumb","numb1","numb2")
splithalf2$splitgroup <- NA
for(i in 1:nrow(numberpercondpersubj2)){
  splithalf2[splithalf2$MD5==numberpercondpersubj2[i,'MD5']&splithalf2$Type==numberpercondpersubj2[i,'Type'],]$splitgroup <- sample(c(rep("first",numberpercondpersubj2[i,'numb1']),rep("second",numberpercondpersubj2[i,'numb2'])))
}

#load lm's results
lm_prediction <- read.csv("lm_prediction.csv",header=T)


#run the script for the brm versions and load them here
load("brm_P0P1P2_AMBxCONSTR_item72_RTacross3words.RData")
library(brms)
#ps_P0 <- posterior_summary(brm_P0_item72)
randomslope_names_item72 <- rownames(ps_P0)[grepl("item72",rownames(ps_P0))][76:147]
#rm(ps)
#extract the posterior for each iteration
psamp_P0_item72 <- posterior_samples(brm_P0_item72, fixed=TRUE, pars=c("b_AMBUAMB",randomslope_names_item72))
#summing the posterior for fixed and random effect
for(i in 2:73){
  psamp_P0_item72[,i+72] =psamp_P0_item72[,1]+psamp_P0_item72[,i]
}
psamp_P0_item72 <- psamp_P0_item72[,74:145]; colnames(psamp_P0_item72) <- paste0("item",as.character(1:72))
model_based_predictions_P0_item72 <- data.frame(ITEM72_MEAN_P0 = colMeans(psamp_P0_item72))
for(i in 1:72){
  model_based_predictions_P0_item72$HDI_low_P0[i] <- sort(psamp_P0_item72[,i])[151]
  model_based_predictions_P0_item72$HDI_high_P0[i] <- sort(psamp_P0_item72[,i])[5850]
  model_based_predictions_P0_item72$SE_P0[i] <- (sort(psamp_P0_item72[,i])[5850]-sort(psamp_P0_item72[,i])[151])/4}
model_based_predictions_P0_item72$item72 <- 1:72
lm_prediction <- left_join(lm_prediction,model_based_predictions_P0_item72)
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb, lm_prediction$ITEM72_MEAN_P0)
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb, lm_prediction$ITEM72_MEAN_P0)
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="MVRR"])
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPZ"])
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="MVRR"])
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPS"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPS"])
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPZ"])


library(ggplot2)
library(gridExtra)
library(grid)
library(egg)
#xlab("Surprisal difference at the disambiguating verb")+
  #ylab("Garden path effect at the verb")+
#NPS error bar = 95HDI
cor.test(lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPS"],lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPS"])
NPS_itemgpe_by_lstmsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPS",],aes(x=lstmsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.51, (NPS)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="green4")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPS_itemgpe_by_lstmsurprisal <- set_panel_size(NPS_itemgpe_by_lstmsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))
#NPZ
cor.test(lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPZ"],lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"])
NPZ_itemgpe_by_lstmsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPZ",],aes(x=lstmsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.12, (NPZ)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="blue")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPZ_itemgpe_by_lstmsurprisal <- set_panel_size(NPZ_itemgpe_by_lstmsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))
#MVRR
cor.test(lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="MVRR"],lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"])
MVRR_itemgpe_by_lstmsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="MVRR",],aes(x=lstmsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.36, (Wiki-LSTM, MVRR)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="red")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
MVRR_itemgpe_by_lstmsurprisal <- set_panel_size(MVRR_itemgpe_by_lstmsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))

#NPS
cor.test(lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPS"],lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPS"])
NPS_itemgpe_by_gptsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPS",],aes(x=gptsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = -0.01, (NPS)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="green4")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPS_itemgpe_by_gptsurprisal <- set_panel_size(NPS_itemgpe_by_gptsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))

#NPZ
cor.test(lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPZ"],lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"])
NPZ_itemgpe_by_gptsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPZ",],aes(x=gptsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.35, (NPZ)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="blue")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPZ_itemgpe_by_gptsurprisal <- set_panel_size(NPZ_itemgpe_by_gptsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))

#MVRR
cor.test(lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="MVRR"],lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"])
MVRR_itemgpe_by_gptsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="MVRR",],aes(x=gptsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r =-0.06, (GPT-2, MVRR)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="red")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
MVRR_itemgpe_by_gptsurprisal <- set_panel_size(MVRR_itemgpe_by_gptsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))


grid.arrange(MVRR_itemgpe_by_lstmsurprisal,
             NPS_itemgpe_by_lstmsurprisal,
             NPZ_itemgpe_by_lstmsurprisal,
             MVRR_itemgpe_by_gptsurprisal,
             NPS_itemgpe_by_gptsurprisal,
             NPZ_itemgpe_by_gptsurprisal,nrow=2,
             bottom=textGrob("Surprisal difference at the disambiguating verb (ambig - unambig)", gp=gpar(fontsize=14)),
             left=textGrob("Empirical Garden Path Effect at the disambiguating verb", gp=gpar(fontsize=14),rot=90))




ggplot(lm_prediction,aes(x=item,y=ITEM72_MEAN_P0,color=Type))+
  geom_point(size=2)+
  xlab("Triplet Index")+
  ylab("Garden path effect at the verb")+
  ggtitle("Comparing GPEs within a triplet of three constructions")+
  geom_errorbar(aes(ymin=HDI_low_P0,ymax=HDI_high_P0),width=.3)+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14))


#plotting human's mean GPE
#ps_P0_AMBxCONSTR <- posterior_summary(brm_P0_AMBxCONSTR)
#randomslope_names_AMBxCONSTR <- rownames(ps_P0_AMBxCONSTR)[which(grepl("r_item",rownames(ps_P0_AMBxCONSTR)))][-(1:15)]
psamp_P0_AMBxCONSTR <- posterior_samples(brm_P0_AMBxCONSTR,fixed=TRUE,pars=c("b_AMBUAMB", "b_AMBUAMB:SZM1", "b_AMBUAMB:SZM2"))
psamp_P1_AMBxCONSTR <- posterior_samples(brm_P1_AMBxCONSTR,fixed=TRUE,pars=c("b_AMBUAMB", "b_AMBUAMB:SZM1", "b_AMBUAMB:SZM2"))
psamp_P2_AMBxCONSTR <- posterior_samples(brm_P2_AMBxCONSTR,fixed=TRUE,pars=c("b_AMBUAMB", "b_AMBUAMB:SZM1", "b_AMBUAMB:SZM2"))
mean_3positions <- data.frame(CONSTRUCTION=c("MVRR","NPS","NPZ"),
                               POSITION=c(rep(c("Disambiguating Verb","Spillover1","Spillover2"),each=3)),
                               MEAN=c(mean(psamp_P0_AMBxCONSTR[,1]),mean((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,2])),mean((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,3])),mean(psamp_P1_AMBxCONSTR[,1]),mean((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,2])),mean((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,3])),mean(psamp_P2_AMBxCONSTR[,1]),mean((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,2])),mean((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,3]))),
                               HDI_low=c(sort(psamp_P0_AMBxCONSTR[,1])[151],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,2]))[151],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,3]))[151],sort(psamp_P1_AMBxCONSTR[,1])[151],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,2]))[151],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,3]))[151],sort(psamp_P2_AMBxCONSTR[,1])[151],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,2]))[151],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,3]))[151]),
                               HDI_high=c(sort(psamp_P0_AMBxCONSTR[,1])[5850],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,2]))[5850],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,3]))[5850],sort(psamp_P1_AMBxCONSTR[,1])[5850],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,2]))[5850],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,3]))[5850],sort(psamp_P2_AMBxCONSTR[,1])[5850],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,2]))[5850],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,3]))[5850]))
mean_3positions <- mutate(mean_3positions,SE=(HDI_high-HDI_low)/4)
Human <- ggplot(data=mean_3positions, aes(x=POSITION, y=MEAN, fill=CONSTRUCTION)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=MEAN-2*SE,ymax=MEAN+2*SE),width=.2,position=position_dodge(.9))+
  xlab("Human")+
  ylab("Mean Garden Path Effect")+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.position="top")


#plotting lm's mean surprisal difference
library(reshape2)
lm_mean <- melt(lm_prediction[,c('item','Type','lstmsurprisaldiff_ambminusuamb','gptsurprisaldiff_ambminusuamb','lstmsurprisaldiff_ambminusuamb_spillover1','lstmsurprisaldiff_ambminusuamb_spillover2','gptsurprisaldiff_ambminusuamb_spillover1','gptsurprisaldiff_ambminusuamb_spillover2')],
                    id.vars=c("item",'Type'),
                    variable.name="DV",value.name="surprisaldiff")
lm_mean$position <- ifelse(grepl("spillover1",lm_mean$DV),"Spillover1",
                               ifelse(grepl("spillover2",lm_mean$DV),"Spillover2","Disambiguating Verb"))
lm_mean$LM <- ifelse(grepl("gpt",lm_mean$DV),"GPT-2","Wiki-LSTM")
A1 <- aggregate(lm_mean$surprisaldiff,
                by=list(lm_mean$Type,lm_mean$LM,lm_mean$position),FUN=mean) %>% filter(Group.2=="Wiki-LSTM")
A2 <- aggregate(lm_mean$surprisaldiff,
                by=list(lm_mean$Type,lm_mean$LM,lm_mean$position),FUN=mean) %>% filter(Group.2=="GPT-2")
lm_mean_wiki <- ggplot(data=A1[A1$Group.3=="Disambiguating Verb",], aes(x=Group.1, y=x)) +
  geom_bar(stat="identity",position=position_dodge(),fill=c("#FF6666","#32CD32","#6495ED"))+
  xlab("Wiki-LSTM")+
  ylab("")+
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(size=0),
        axis.text.y = element_text(size=14,face="bold"))
lm_mean_gpt <- ggplot(data=A2[A2$Group.3=="Disambiguating Verb",], aes(x=Group.1, y=x)) +
  geom_bar(stat="identity",position=position_dodge(),fill=c("#FF6666","#32CD32","#6495ED"))+
  xlab("GPT-2 (Disambiguating Verb)")+
  ylab("Mean Surprisal Difference")+
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(size=0),
        axis.text.y = element_text(size=14,face="bold"))





#inspect results if using RTacross3words (rather than word by word)
psamp_RTacross3words <- posterior_samples(brm_P0_item72_RTacross3words, fixed=TRUE, pars=c("b_AMBUAMB",randomslope_names))
for(i in 2:73){
  psamp_RTacross3words[,i+72] =psamp_RTacross3words[,1]+psamp_RTacross3words[,i]
}
psamp_RTacross3words <- psamp_RTacross3words[,74:145]; colnames(psamp_RTacross3words) <- paste0("item",as.character(1:72))
model_based_predictions_item72_RTacross3words <- data.frame(ITEM72_MEAN_RTacross3words = colMeans(psamp_RTacross3words))
for(i in 1:72){
  model_based_predictions_item72_RTacross3words$HDI_low_RTacross3words[i] <- sort(psamp_RTacross3words[,i])[151]
  model_based_predictions_item72_RTacross3words$HDI_high_RTacross3words[i] <- sort(psamp_RTacross3words[,i])[5850]
  model_based_predictions_item72_RTacross3words$SE_RTacross3words[i] <- (sort(psamp_RTacross3words[,i])[5850]-sort(psamp_RTacross3words[,i])[151])/4}
model_based_predictions_item72_RTacross3words$item72 <- 1:72

lm_prediction <- left_join(lm_prediction,model_based_predictions_item72_RTacross3words)
