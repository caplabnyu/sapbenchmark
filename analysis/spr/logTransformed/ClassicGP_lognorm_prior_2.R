source("util_logadd.R")
source('brms_parameters_log.R')
ClassicGP <- load_data("ClassicGP")
ClassicGP$SZM1 <- ifelse(ClassicGP$CONSTRUCTION=="NPS",1,0)
ClassicGP$SZM2 <- ifelse(ClassicGP$CONSTRUCTION=="NPZ",1,0)
brms_parms <- get_brms_parameters()
ClassicGP$RT <- log(ClassicGP$RT)
ClassicGP_ROI2_lognorm <- brm(RT ~ AMBUAMB * (SZM1 + SZM2) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || participant) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || item),
                      data=ClassicGP[ClassicGP$ROI==2,],
                      prior = brms_parms$prior,
                      cores = brms_parms$ncores,
                      iter = brms_parms$niters,
                      seed = brms_parms$seed,
                      warmup = brms_parms$warmup,
                      control = list(adapt_delta = brms_parms$adapt_delta))
summary(ClassicGP_ROI2_lognorm)
saveRDS(ClassicGP_ROI2_lognorm,"ClassicGP_lognorm_prior_2.rds")