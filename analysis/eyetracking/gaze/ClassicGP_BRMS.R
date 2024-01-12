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

source("../../shared/brms_parameters.R")

preprocess <- function(data) {
  data <- data %>% 
    separate('cond', c("GPTYPE", "AMBIG")) %>%
    mutate(MVS = ifelse(GPTYPE == "NPS", 1, 0),
           MVZ = ifelse(GPTYPE == "NPZ", 1, 0),
           AMB = ifelse(ambiguity == "ambiguous", 1, 0))
  return(data)
}

formula = predicted ~ AMB * (MVS + MVZ) + 
  (1 + AMB * (MVS + MVZ) || item) + 
  (1 + AMB * (MVS + MVZ) || subj)

rois <- c(0, 1)
lms <- c("lstm", "gpt2", "baseline")
measures <- c("gaze") 

params <- get_brms_parameters("prior1")

#stat_models <- list()
for (lm in lms) {
  for (measure in measures) {
    data <- read.csv(paste0("../predicted/items_ClassicGP_", measure, '_', lm, "_pred.csv"))
    data <- preprocess(data)
    for (roi_offset in rois) {
      model <- brm(formula,
                   data=subset(data, 
                               region == ROI + roi_offset), 
                   iter=15000,
                   cores=params$ncores,
                   warmup = 7500,
                   seed = params$seed,
                   prior = params$prior,
                   control = list(adapt_delta=params$adapt_delta))
       
      saveRDS(model,
              paste0("brms/classicGP_", 
                     measure, "_",
                     lm, "_p", 
                     roi_offset, ".rds")
              )
      #stat_models[paste0(measure, ".", lm, ".p", roi_offset)] <- model
      
    }
  }
}
