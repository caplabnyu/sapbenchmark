library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(stringr)
library(brms)

model <- "lstm"

spr <- read.csv("AgreementSet.csv", header=TRUE) %>% 
  filter(RT<=7000) %>% filter(RT>0) %>%
  filter(ROI%in%c(-3,-2,-1,0,1,2)) %>%
  rename(participant = MD5)

spr$Type <- as.character(spr$Type) # Just in case it's automatically read as a factor

# Maybe this should all just be a recode call?
spr$Type[spr$Type == "AGREE"] <- "AGREE_G"

spr <- spr %>% separate(Type, c("Type", "pGram"), sep="_")

spr$pGram[spr$pGram == "UAMB"] <- "G"
spr$pGram[spr$pGram == "AMB"] <- "U"
spr$pGram[spr$pGram == "UNG"] <- "U"

spr$pGram <- as.factor(spr$pGram)
spr$Type <- as.factor(spr$Type)

surps_agr <- read.csv(paste0("../../../Surprisals/data/gulordava/items_Agreement.", model, ".csv.scaled"))
surps_npz <- read.csv(paste0("../../../Surprisals/data/gulordava/items_ClassicGP.", model, ".csv.scaled"))

relevant_cols = intersect(colnames(surps_agr), colnames(surps_npz))
surps <- rbind(surps_agr[,relevant_cols], surps_npz[,relevant_cols])

surps$word_pos <- surps$word_pos + 1
surps$surprisal_s <- surps$sum_surprisal_s


merged <- merge(x=spr, y=surps,
                by.x=c("Sentence", "WordPosition"), by.y=c("Sentence", "word_pos"), 
                all.x=TRUE)

merged$item <- merged$item.x

with_lags <- merged %>% group_by_at(vars(item, participant)) %>%
  arrange("word_pos", .by_group = TRUE) %>%
  mutate(RT_p1 = lag(RT), 
         RT_p2 = lag(RT_p1), 
         RT_p3 = lag(RT_p2),
         length_p1_s = lag(length_s), 
         length_p2_s = lag(length_p1_s),
         length_p3_s = lag(length_p2_s),
         logfreq_p1_s = lag(logfreq_s), 
         logfreq_p2_s = lag(logfreq_p1_s),
         logfreq_p3_s = lag(logfreq_p2_s),
         surprisal_p1_s = lag(surprisal_s),
         surprisal_p2_s = lag(surprisal_p1_s),
         surprisal_p3_s = lag(surprisal_p2_s)
  )

filler_model <- readRDS(paste0("../../../Surprisals/analysis/filler_models/filler_", model, "_sum.rds"))

with_lags$predicted_rt <- predict(filler_model, newdata=with_lags, allow.new.levels=TRUE)

saveRDS(with_lags, paste0("datasets/agreement_data_", model, "_predicted.", model, ".rds"))

rt.ht_data <- readRDS(paste0("datasets/agreement_data_", model, "_predicted.rds"))

prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),
            prior("normal(0,500)", class = "sigma"))

rt.bmodel <- brm(predicted_rt ~ Type.coded * pGram.coded * (position.coded.1 + position.coded.2) + (1 + Type.coded * pGram.coded * (position.coded.1 + position.coded.2) || item) + (1 + Type.coded * pGram.coded * (position.coded.1 + position.coded.2) || participant),
                 data = rt.ht_data,
                 prior = prior1,
                 iter = 6000,
                 cores = 4,
                 seed = 117,
)

saveRDS(rt.bmodel, paste0("models/agreement_bmodel_prior1_", model, "_predicted.rds"))
