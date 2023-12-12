source("util.R")
source('brms_parameters.R')
ClassicGP <- load_data("ClassicGP")
ClassicGP$SZM1 <- ifelse(ClassicGP$CONSTRUCTION=="NPS",1,0)
ClassicGP$SZM2 <- ifelse(ClassicGP$CONSTRUCTION=="NPZ",1,0)
brms_parms <- get_brms_parameters()
ClassicGP_ROI0 <- brm(RT ~ AMBUAMB * (SZM1 + SZM2) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || participant) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || item),
                      data=ClassicGP[ClassicGP$ROI==0,],
                      prior = brms_parms$prior,
                      cores = brms_parms$ncores,
                      iter = brms_parms$niters,
                      seed = brms_parms$seed,
                      warmup = brms_parms$warmup,
                      control = list(adapt_delta = brms_parms$adapt_delta))
summary(ClassicGP_ROI0)
saveRDS(ClassicGP_ROI0,"ClassicGP_P0.rds")


ClassicGP_ROI1 <- brm(RT ~ AMBUAMB * (SZM1 + SZM2) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || participant) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || item),
                      data=ClassicGP[ClassicGP$ROI==1,],
                      prior = brms_parms$prior,
                      cores = brms_parms$ncores,
                      iter = brms_parms$niters,
                      seed = brms_parms$seed,
                      warmup = brms_parms$warmup,
                      control = list(adapt_delta = brms_parms$adapt_delta))
summary(ClassicGP_ROI1)
saveRDS(ClassicGP_ROI1,"ClassicGP_P1.rds")


ClassicGP_ROI2 <- brm(RT ~ AMBUAMB * (SZM1 + SZM2) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || participant) +
                        (1 + AMBUAMB * (SZM1 + SZM2) || item),
                      data=ClassicGP[ClassicGP$ROI==2,],
                      prior = brms_parms$prior,
                      cores = brms_parms$ncores,
                      iter = brms_parms$niters,
                      seed = brms_parms$seed,
                      warmup = brms_parms$warmup,
                      control = list(adapt_delta = brms_parms$adapt_delta))
summary(ClassicGP_ROI2)
saveRDS(ClassicGP_ROI2,"ClassicGP_P2.rds")