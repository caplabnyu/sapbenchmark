## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
library(tidyr)
library(dplyr)

rt.rdata <- read.csv("data/AgreementSet.csv", header=TRUE)
rt.data <- rt.rdata %>% 
  filter(RT<=7000) %>% filter(RT>0) %>%
  filter(ROI%in%c(-3,-2,-1,0,1,2)) %>%
  rename(participant = MD5)

rt.data$Type <- as.character(rt.data$Type) # Just in case it's automatically read as a factor

# Maybe this should all just be a recode call?
rt.data$Type[rt.data$Type == "AGREE"] <- "AGREE_G"

rt.data <- rt.data %>% separate(Type, c("Type", "pGram"), sep="_")

rt.data$pGram[rt.data$pGram == "UAMB"] <- "G"
rt.data$pGram[rt.data$pGram == "AMB"] <- "U"
rt.data$pGram[rt.data$pGram == "UNG"] <- "U"

rt.data$pGram <- as.factor(rt.data$pGram)
rt.data$Type <- as.factor(rt.data$Type)


## -----------------------------------------------------------------------------

# Sum coding for the position factor
rt.ht_data <- rt.data %>% subset(rt.data$ROI >= 0) # critical + spillover data only

# Keep only NPZ items with an agreement counterpart
agree_items <- unique(rt.ht_data$item[rt.ht_data$Type == "AGREE"])
rt.ht_data <- rt.ht_data[rt.ht_data$item %in% agree_items,]

rt.ht_data$position <- droplevels(as.factor(rt.ht_data$ROI))

contrasts(rt.ht_data$position) <- contr.sum(3)/2
contrasts(rt.ht_data$position)


rt.ht_data$pGram.coded <- recode(rt.ht_data$pGram, "U" = 1, "G" = 0)
rt.ht_data$Type.coded <- recode(rt.ht_data$Type, "AGREE" = 0, "NPZ" = 1)
rt.ht_data$position.coded.1 <- recode(rt.ht_data$position, "0"=0.5, "1"=0, "2"=-0.5)
rt.ht_data$position.coded.2 <- recode(rt.ht_data$position, "0"=0, "1"=0.5, "2"=-0.5)


## -----------------------------------------------------------------------------
library(lme4)
rt.fillers <- read.csv("./data/Fillers.csv") %>% 
  filter(RT <= 7000) %>% filter(RT>0) %>% 
  rename(participant = MD5)

model.trialnumcorr <- lmer(RT ~ scale(trialnumber) + (1 + scale(trialnumber) | participant),
                           data=rt.fillers,
                           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


rt.ht_data$trialnum_pred <- predict(model.trialnumcorr, newdata=rt.ht_data)
rt.ht_data$RT_corrected <- rt.ht_data$RT - rt.ht_data$trialnum_pred

saveRDS(rt.ht_data, "datasets/agreement_data.rds")



## -----------------------------------------------------------------------------
rt.ht_data <- readRDS("datasets/agreement_data.rds")
summary(rt.ht_data)
head(subset(rt.ht_data, is.na(rt.ht_data$pGram)))


## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(data=rt.ht_data, aes(x=RT, fill=Type)) + geom_histogram(binwidth=50) + labs(title="Raw RT distribution")


## -----------------------------------------------------------------------------
ggplot(data=rt.ht_data, aes(x=RT_corrected, fill=Type)) + geom_histogram(binwidth=50) + labs(title="Corrected RT distribution")


## -----------------------------------------------------------------------------
ggplot(data=rt.ht_data, aes(x=trialnum_pred, fill=Type)) + geom_histogram(binwidth=50) + labs(title="Distribution predicted by trial number alone")


## -----------------------------------------------------------------------------
library(brms)
rt.ht_data <- readRDS("datasets/agreement_data.rds")

prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),
            prior("normal(0,500)", class = "sigma"))

rt.bmodel <- brm(RT_corrected ~ Type.coded * pGram.coded * (position.coded.1 + position.coded.2) + (1 + Type.coded * pGram.coded * (position.coded.1 + position.coded.2) || item) + (1 + Type.coded * pGram.coded * (position.coded.1 + position.coded.2) || participant),
                 data = rt.ht_data,
                 prior = prior1,
                 iter = 6000,
                 cores = 4,
                 seed = 117,
                )

saveRDS(rt.bmodel, "models/agreement_bmodel_prior1_corrected.rds")



## -----------------------------------------------------------------------------

model.prior1.corrected <- readRDS("models/agreement_bmodel_prior1_corrected.rds")
summary(model.prior1.corrected)


## -----------------------------------------------------------------------------
source("../../shared/util.R")

reshape_item_output <- reshape_item_dat(model.prior1.corrected, 'item')
reshape_item_output$item <- as.numeric(reshape_item_output$item)
by_item <- reshape_item_output %>%
  mutate(GPE = b_pGram.coded + `b_Type.coded:pGram.coded` + r_pGram.coded + `r_Type.coded:pGram.coded`,
         Agr = b_pGram.coded + r_pGram.coded,
         GPE_0 = GPE + 0.5*`b_pGram.coded:position.coded.1` + 0.5 * `b_Type.coded:pGram.coded:position.coded.1` + 0.5*`r_pGram.coded:position.coded.1` + 0.5*`r_Type.coded:pGram.coded:position.coded.1`,
         GPE_1 = GPE + 0.5*`b_pGram.coded:position.coded.2` + 0.5 * `b_Type.coded:pGram.coded:position.coded.2` + 0.5*`r_pGram.coded:position.coded.2` + 0.5*`r_Type.coded:pGram.coded:position.coded.2`,
         GPE_2 = GPE  - 0.5 * `b_pGram.coded:position.coded.1` - 0.5 * `b_Type.coded:pGram.coded:position.coded.1` -  0.5 * `b_pGram.coded:position.coded.2` - 0.5 * `b_Type.coded:pGram.coded:position.coded.2`- 0.5 * `r_pGram.coded:position.coded.1` - 0.5 * `r_Type.coded:pGram.coded:position.coded.1` -  0.5 * `r_pGram.coded:position.coded.2` - 0.5 * `r_Type.coded:pGram.coded:position.coded.2`,
         Agr_0 = Agr + 0.5 * `b_pGram.coded:position.coded.1` + 0.5 * `r_pGram.coded:position.coded.1`,
         Agr_1 = Agr + 0.5 * `b_pGram.coded:position.coded.2` + 0.5 * `r_pGram.coded:position.coded.2`,
         Agr_2 = Agr  - 0.5 * `b_pGram.coded:position.coded.1`- 0.5 * `b_pGram.coded:position.coded.2` - 0.5 * `r_pGram.coded:position.coded.1`- 0.5 * `r_pGram.coded:position.coded.2`
  ) %>%
  select(`.chain`, `.draw`, `.iteration`, item, GPE, Agr, GPE_0, GPE_1, GPE_2, Agr_0, Agr_1, Agr_2) %>%
  gather(key = 'coef', value = 'val', GPE, Agr, GPE_0, GPE_1, GPE_2, Agr_0, Agr_1, Agr_2) %>%
  group_by(item, coef) %>%
  summarise(mean = mean(val),
            lower = quantile(val, 0.025)[[1]],
            upper = quantile(val, 0.975)[[1]])%>%separate(coef,c('coef','ROI'),sep='_')


## -----------------------------------------------------------------------------
by_construction <- reshape_item_output[,c('.draw',colnames(reshape_item_output)[str_detect(colnames(reshape_item_output),'b')])] %>% unique() %>%
  mutate(GPE_0 = b_pGram.coded + `b_Type.coded:pGram.coded` + 0.5*`b_pGram.coded:position.coded.1` + 0.5 * `b_Type.coded:pGram.coded:position.coded.1`,
         GPE_1 = b_pGram.coded + `b_Type.coded:pGram.coded` + 0.5*`b_pGram.coded:position.coded.2` + 0.5 * `b_Type.coded:pGram.coded:position.coded.2` ,
         GPE_2 = b_pGram.coded + `b_Type.coded:pGram.coded` - 0.5 * `b_pGram.coded:position.coded.1` - 0.5 * `b_Type.coded:pGram.coded:position.coded.1` -  0.5 * `b_pGram.coded:position.coded.2` - 0.5 * `b_Type.coded:pGram.coded:position.coded.2`,
         Agr_0 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.1`,
         Agr_1 = b_pGram.coded + 0.5 * `b_pGram.coded:position.coded.2`,
         Agr_2 = b_pGram.coded - 0.5 * `b_pGram.coded:position.coded.1`- 0.5 * `b_pGram.coded:position.coded.2`) %>%
  select(GPE_0,GPE_1,GPE_2,Agr_0,Agr_1,Agr_2) %>% gather(key='coef',value='val')%>%group_by(coef)%>%
  summarise(mean=mean(val),
            lower=quantile(val,0.025)[[1]],
            upper=quantile(val,0.975)[[1]])%>%separate(coef, c('coef', 'ROI'), sep= '_')


## -----------------------------------------------------------------------------
Plot_empirical_construction_level(by_construction,"Agreement")
ggsave("../../../plots/spr/Agreement/empirical_bycondition.pdf")

Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=0)
ggsave("../../../plots/spr/Agreement/empirical_byitem.pdf")

by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"Agreement")
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
ggsave("../../../plots/spr/Agreement/surprisal_correlation.pdf")

