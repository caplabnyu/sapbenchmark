library(plyr)
library(dplyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(stringr)

data.gaze.lstm.raw <- read.csv("predicted/items_ClassicGP_gaze_lstm_pred.csv")
data.gaze.gpt2.raw <- read.csv("predicted/items_ClassicGP_gaze_gpt2_pred.csv")
data.ro.lstm.raw <- read.csv("predicted/items_ClassicGP_ro_lstm_pred.csv")
data.ro.gpt2.raw <- read.csv("predicted/items_ClassicGP_ro_gpt2_pred.csv")

preprocess <- function(data) {
  data <- data %>% 
    separate('cond', c("GPTYPE", "AMBIG")) %>%
    mutate(MVS = ifelse(GPTYPE == "NPS", 1, 0),
           MVZ = ifelse(GPTYPE == "NPZ", 1, 0),
           AMB = ifelse(ambiguity == "ambiguous", 1, 0))
  return(data)
}

data.gaze.lstm <- preprocess(data.gaze.lstm.raw)
data.gaze.gpt2 <- preprocess(data.gaze.gpt2.raw)
data.ro.lstm <- preprocess(data.ro.lstm.raw)
data.ro.gpt2 <- preprocess(data.ro.gpt2.raw)

formula = predicted ~ AMB * (MVS + MVZ) + 
  (1 + AMB * (MVS + MVZ) || item) + 
  (1 | subj)

rois <- c(0, 1, 2)
lms <- c("lstm", "gpt2")
measures <- c("gaze", "ro")

stat_models <- list()
for (lm in lms) {
  for (measure in measures) {
    for (roi_offset in rois) {
      model <- lmer(data=subset(get(paste0("data.", measure, ".", lm)), 
                         region == ROI + roi_offset), 
             formula)
       
      saveRDS(model,
              paste0("predicted/lmer/classicGP_", measure, "_",
                    lm, "_p", 
                    roi_offset, ".rds")
              )
      stat_models[paste0(measure, ".", lm, ".p", roi_offset)] <- model
      
    }
  }
}
