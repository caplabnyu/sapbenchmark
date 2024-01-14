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
library(argparse)

## Load in shared functions and variables

source("../../shared/brms_parameters.R")
params <- get_brms_parameters("prior_bernoulli") #since this is regout

## Argument parser

parser <- ArgumentParser()
parser$add_argument("--model", help = "lstm, gpt2, baseline")
parser$add_argument("--region", help = "0, 1, 2")
args <- parser$parse_args()

roi_offset <- as.numeric(args$region)
model <- args$model

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
data.raw <- read.csv(paste0("../predicted/items_ClassicGP_ro_", model, "_pred.csv"))
data <- preprocess(data.raw)

formula = predicted ~ AMB * (MVS + MVZ) + 
  (1 + AMB * (MVS + MVZ) | item) + 
  (1 + AMB * (MVS + MVZ) | subj)


fit <- brm(formula,
             data=subset(data, 
                         region == ROI + roi_offset), 
             iter=15000,
             cores=params$ncores,
             warmup = 7500,
             seed = params$seed,
             prior = params$prior,
             control = list(adapt_delta=params$adapt_delta))

saveRDS(fit, paste0("../brms_models/classicGP_ro_",model,
                    "_p", roi_offset, ".rds"))

