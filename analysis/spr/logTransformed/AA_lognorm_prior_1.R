#These scripts are run with NYU greene high performance computing service
source("util_logadd.R")
source('brms_parameters_log.R')

brms_parms <- get_brms_parameters()
rt.data <- load_data("AttachmentAmbiguity")


rt.data$ambiguity <- ifelse(
  rt.data$AMBIG=="Amb",2/3,-1/3)

rt.data <- rt.data %>% 
  mutate(height = case_when(Type=="AttachMulti" ~ 0, 
                            Type=="AttachLow" ~ -1/2,
                            Type=="AttachHigh" ~ 1/2))
rt.data$RT_log <- ifelse(rt.data$RT==0,1,rt.data$RT)
rt.data$RT_log <- log(rt.data$RT_log)




### ROI=1
brm_prior1_emp_1_log <- brm(RT_log ~ ambiguity + height + (1+ambiguity+height||item) + (1+ambiguity+height||participant),
                              data=subset(rt.data, ROI==1&!is.na(RT)),
                              prior = brms_parms$prior,
                              cores = brms_parms$ncores,
                              iter = 24000,
                              seed = brms_parms$seed,
                              warmup = 12000,
                              control = list(adapt_delta = brms_parms$adapt_delta))
saveRDS(brm_prior1_emp_1_log, file="AA_logadd/brm_prior1_AA_emp_1_log.rds")
summary(brm_prior1_emp_1_log)
