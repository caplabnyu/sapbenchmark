library(plyr)
library(dplyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(stringr)
library(brms)

## Load in shard functions and variables

source("../../shared/brms_parameters.R")
params <- get_brms_parameters("prior_bernoulli") #since this is regout

## Custom
preprocess <- function(data) {
  data <- data %>% 
    separate('cond', c("GPTYPE", "AMBIG")) %>%
    mutate(MVS = ifelse(GPTYPE == "NPS", 1, 0),
           MVZ = ifelse(GPTYPE == "NPZ", 1, 0),
           AMB = ifelse(ambiguity == "ambiguous", 1, 0))
  return(data)
}

## Load in data
data.lstm.raw <- read.csv("../predicted/items_ClassicGP_ro_lstm_pred.csv")
data.gpt2.raw <- read.csv("../predicted/items_ClassicGP_ro_gpt2_pred.csv")

data.lstm <- preprocess(data.lstm.raw)
data.gpt2 <- preprocess(data.gpt2.raw)

formula = predicted ~ AMB * (MVS + MVZ) + 
  (1 + AMB * (MVS + MVZ) || item) + 
  (1 | subj)

rois <- c(0, 1, 2)
lms <- c("lstm", "gpt2")

for (roi_offset in rois) {
  for (lm in lms) {
    model <- brm(formula,
                 data=subset(get(paste0("data.", lm)), 
                             region == ROI + roi_offset), 
                 iter=2,
                 cores=params$ncores,
                 #warmup = 7500,
                 seed = params$seed,
                 prior = params$prior,
                 control = list(adapt_delta=params$adapt_delta))
    
    saveRDS(model,
            paste0("../brms_models/classicGP_", 
                   measure, "_",
                   lm, "_p", 
                   roi_offset, ".rds")
    )
    #stat_models[paste0(measure, ".", lm, ".p", roi_offset)] <- model
    
  }
}
