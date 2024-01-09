library(StanHeaders, lib.loc="../../rpackages")
library(ps, lib.loc="../../rpackages")
library(Rcpp, lib.loc="../../rpackages")
library(backports, lib.loc="../../rpackages")
library(withr, lib.loc="../../rpackages")
library(ggplot2, lib.loc="../../rpackages")
library(rstan, lib.loc="../../rpackages")
library(brms, lib.loc="../../rpackages")
library(BH, lib.loc="../../rpackages")
library(RcppEigen, lib.loc="../../rpackages")
library(labeling, lib.loc="../../rpackages")
library(plyr, lib.loc="../../rpackages")
library(dplyr, lib.loc="../../rpackages")
library(Matrix, lib.loc="../../rpackages")
library(lme4, lib.loc="../../rpackages")
library(stringr, lib.loc="../../rpackages")
source("brms_parameters.R")
brm_param_list <- get_brms_parameters('prior_bernoulli')
numbofmeasures = 6  # e.g., ffd, gz, gp, tt, regin, regout (six kinds of measures)
df <- readRDS("valid_data_allmeasures_N368.rds")

for(i in 1:nrow(df)){   #identify the column name for the ROI position of each unique item
  df$whichcolumn[i] = ifelse(df$ROI[i]==1000,NA,paste0("R",df$ROI[i]))
}
for(i in 1:nrow(df)){   #identify the numeric index of the columns that contain the ROI duration for different measures
  df$ROIcolumnsindex[i] <-  ifelse(df$ROI[i]==1000,list(rep(NA,numbofmeasures)),list(which(grepl(df$whichcolumn[i],colnames(df)))))
}

ROImeasures <- df[,c("seq","subj","item","cond",'Session_name','correct','sentence','question','RT_question')]   #create a new df but storing only information that are of interest
for(i in 1:nrow(df)){
  ROImeasures$ffdpre2[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][1]-2])
  ROImeasures$gzpre2[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][2]-2])
  ROImeasures$gppre2[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][3]-2])
  ROImeasures$ttpre2[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][4]-2])
  ROImeasures$reginpre2[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][5]-2])
  ROImeasures$regoutpre2[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][6]-2])
  ROImeasures$ffdpre[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][1]-1])
  ROImeasures$gzpre[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][2]-1])
  ROImeasures$gppre[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][3]-1])
  ROImeasures$ttpre[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][4]-1])
  ROImeasures$reginpre[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][5]-1])
  ROImeasures$regoutpre[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][6]-1])
  ROImeasures$ffd0[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][1]])
  ROImeasures$gz0[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][2]])
  ROImeasures$gp0[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][3]])
  ROImeasures$tt0[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][4]])
  ROImeasures$regin0[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][5]])
  ROImeasures$regout0[i] <- ifelse(df$ROI[i]==1000,NA,df[i,df$ROIcolumnsindex[[i]][6]])
  ROImeasures$ffd1[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][1]+2],
                                                          ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][1]-3],df[i,df$ROIcolumnsindex[[i]][1]-2]),df[i,df$ROIcolumnsindex[[i]][1]+1])))
  ROImeasures$gz1[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][2]+2],
                                                         ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][2]-3],df[i,df$ROIcolumnsindex[[i]][2]-2]),df[i,df$ROIcolumnsindex[[i]][2]+1])))
  ROImeasures$gp1[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][3]+2],
                                                         ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][3]-3],df[i,df$ROIcolumnsindex[[i]][3]-2]),df[i,df$ROIcolumnsindex[[i]][3]+1])))
  ROImeasures$tt1[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][4]+2],
                                                         ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][4]-3],df[i,df$ROIcolumnsindex[[i]][4]-2]),df[i,df$ROIcolumnsindex[[i]][4]+1])))
  ROImeasures$regin1[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][5]+2],
                                                            ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][5]-3],df[i,df$ROIcolumnsindex[[i]][5]-2]),df[i,df$ROIcolumnsindex[[i]][5]+1])))
  ROImeasures$regout1[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][6]+2],
                                                             ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][6]-3],df[i,df$ROIcolumnsindex[[i]][6]-2]),df[i,df$ROIcolumnsindex[[i]][6]+1])))
  ROImeasures$ffd2[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][1]+3],
                                                          ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][1]-2],df[i,df$ROIcolumnsindex[[i]][1]-1]),df[i,df$ROIcolumnsindex[[i]][1]+2])))
  ROImeasures$gz2[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][2]+3],
                                                         ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][2]-2],df[i,df$ROIcolumnsindex[[i]][2]-1]),df[i,df$ROIcolumnsindex[[i]][2]+2])))
  ROImeasures$gp2[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][3]+3],
                                                         ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][3]-2],df[i,df$ROIcolumnsindex[[i]][3]-1]),df[i,df$ROIcolumnsindex[[i]][3]+2])))
  ROImeasures$tt2[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][4]+3],
                                                         ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][4]-2],df[i,df$ROIcolumnsindex[[i]][4]-1]),df[i,df$ROIcolumnsindex[[i]][4]+2])))
  ROImeasures$regin2[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][5]+3],
                                                            ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][5]-2],df[i,df$ROIcolumnsindex[[i]][5]-1]),df[i,df$ROIcolumnsindex[[i]][5]+2])))
  ROImeasures$regout2[i] <- ifelse(df$ROI[i]==1000,NA,ifelse(df$cond[i]=="RC_Subj"&df$item[i]==45,df[i,df$ROIcolumnsindex[[i]][6]+3],
                                                             ifelse(df$cond[i]=="RC_Obj",ifelse(df$item[i]==42,df[i,df$ROIcolumnsindex[[i]][6]-2],df[i,df$ROIcolumnsindex[[i]][6]-1]),df[i,df$ROIcolumnsindex[[i]][6]+2])))
}
ROImeasures <- ROImeasures %>% filter(!grepl("FILLER",.$cond))   #remove fillers because they don't have ROIs
ROImeasures$subj <- as.factor(ROImeasures$subj)
ROImeasures$item <- as.factor(ROImeasures$item)


ClassicGP <- ROImeasures[ROImeasures$cond%in%c("MVRR_AMB","MVRR_UAMB","NPS_AMB","NPS_UAMB","NPZ_AMB","NPZ_UAMB"),]
ClassicGP$SZM1 <- ifelse(ClassicGP$cond%in%c("NPS_UAMB","NPS_AMB"),1,0)
ClassicGP$SZM2 <- ifelse(ClassicGP$cond%in%c("NPZ_UAMB","NPZ_AMB"),1,0)
ClassicGP$AMBUAMB <- ifelse(ClassicGP$cond%in%c("MVRR_UAMB","NPS_UAMB","NPZ_UAMB"),0,1)

GP_regout0 <- brm(regout0~AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)|item)+(1+AMBUAMB*(SZM1+SZM2)|subj),
                                data=ClassicGP[ClassicGP$gz0<=2000,],
                                iter=brm_param_list$niters,
                                cores=brm_param_list$ncores,
                                warmup = brm_param_list$warmup,
                                chains = 4,
                                seed = brm_param_list$seed,
                                prior = brm_param_list$prior,
                                control = list(adapt_delta=brm_param_list$adapt_delta),
                  family = "bernoulli")
summary(GP_regout0)
saveRDS(GP_regout0,"GP_regout0_normal_24000.rds")


GP_regout1 <- brm(regout1~AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)|item)+(1+AMBUAMB*(SZM1+SZM2)|subj),
                  data=ClassicGP[ClassicGP$gz1<=2000,],
                  iter=brm_param_list$niters,
                  cores=brm_param_list$ncores,
                  warmup = brm_param_list$warmup,
                  chains = 4,
                  seed = brm_param_list$seed,
                  prior = brm_param_list$prior,
                  control = list(adapt_delta=brm_param_list$adapt_delta),
                  family="bernoulli")
summary(GP_regout1)
saveRDS(GP_regout1,"GP_regout1_normal_24000.rds")

