#These scripts are run with NYU greene high performance computing service
library(lme4)
library(tidyverse)
library(ggplot2)
library(brms)
# library(bayestestR)
source('../shared/util.R')
source('../shared/brms_parameters.R')

args <- commandArgs(trailingOnly=TRUE)
rt.data <- load_data("RelativeClause")
filler.data <- load_data("Fillers") 

rt.data <- Predicting_RT_with_spillover_anysurp(rt.data, 'RelativeClause', args[1])
filler.data <-  Predicting_RT_with_spillover_anysurp(filler.data, 'Fillers', args[1])

position_fit_lmer_nocor <- lmer(RT ~ scale(WordPosition) + (1 + scale(WordPosition) || participant), subset(filler.data, model==args[1])) # model doesn't matter for RT.
summary(position_fit_lmer_nocor)

rt.data$wordpos_predrt <- predict(position_fit_lmer_nocor, rt.data)
rt.data$corrected_rt <- rt.data$RT - rt.data$wordpos_predrt

prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),    
            prior("normal(0,500)", class = "sigma"))

# residualizing
curr_rc <- subset(rt.data, model == args[1] & !is.na(RT))
curr_filler <- subset(filler.data, model == args[1] & !is.na(RT))
print(paste(nrow(curr_filler), nrow(filler.data)))

curr_fit <-  lmer(predicted ~ scale(WordPosition) + (1 + scale(WordPosition) || participant), curr_filler)

print(summary(curr_fit))

curr_rc$wordpos_predicted = predict(curr_fit, curr_rc)
curr_rc$corrected_predicted = curr_rc$predicted - curr_rc$wordpos_predicted

predicted_dat <- curr_rc
predicted_dat <- predicted_dat %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))


brms_parms <- get_brms_parameters('prior1')

print("Fitting brm model for S0...")
brm_predicted_P0 <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==0 & model == args[1] &!is.na(RT)),
                                prior = prior1,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(brm_predicted_P0, paste0("brm_models/brm_predicted_", args[1], "_RelativeClause_P0.rds"))

print("Fitting brm model for S1...")
brm_predicted_P1 <- brm(corrected_predicted ~ Type_num +
                                  (0 + Type_num || participant) +
                                  (1 + Type_num || item),
                                data=subset(predicted_dat, ROI==1 & model == args[1] &!is.na(RT)),
                                prior = prior1,
                                cores = brms_parms$ncores,
                                iter = brms_parms$niters,
                                seed = brms_parms$seed,
                                warmup = brms_parms$warmup,
                                control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(brm_predicted_P1, paste0("brm_models/brm_predicted_", args[1], "_RelativeClause_P1.rds"))

print("Fitting brm model for S2...")
brm_predicted_P2 <- brm(corrected_predicted ~ Type_num +
                                 (0 + Type_num || participant) +
                                 (1 + Type_num || item),
                               data=subset(predicted_dat, ROI==2 & model == args[1] &!is.na(RT)),
                               prior = prior1,
                               cores = brms_parms$ncores,
                               iter = brms_parms$niters,
                               seed = brms_parms$seed,
                               warmup = brms_parms$warmup,
                               control = list(adapt_delta = brms_parms$adapt_delta)
)

saveRDS(brm_predicted_P2, paste0("brm_models/brm_predicted_", args[1], "_RelativeClause_P2.rds"))
