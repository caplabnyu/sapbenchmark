## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
library(tidyr)
library(dplyr)

## -----------------------------------------------------------------------------

model.prior1.corrected <- readRDS("models/agreement_bmodel_prior1_corrected.rds")
summary(model.prior1.corrected)


## -----------------------------------------------------------------------------
source("../../shared/util.R")


reshape_item_output <- reshape_item_dat(model.prior1.corrected, 'item')
saveRDS(reshape_item_output, "datasets/reshape_item_agreement.rds")

print(colnames(reshape_item_output))
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

saveRDS(by_item, "./datasets/by_item_agreement.rds")

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

saveRDS(by_construction, "./datasets/by_construction_agreement.rds")

## -----------------------------------------------------------------------------
Plot_empirical_construction_level(by_construction,"Agreement")
ggsave("../../../plots/spr/Agreement/empirical_bycondition_nofit.pdf")

Plot_itemwise_by_magnitude(by_item,"Agreement",ROI=0)
ggsave("../../../plots/spr/Agreement/empirical_byitem_nofit.pdf")

by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"Agreement")
Plot_humanresults_surprisaldiff_correlation(by_item_surprisalmerged,0)
ggsave("../../../plots/spr/Agreement/surprisal_correlation_nofit.pdf")

