SAP Benchmark (SPR): RC subset
================

## Load in data

### Empirical data

``` r
rt.data <- load_data("RelativeClause") 

filler.data <- load_data("Fillers") 

rt.data <- Predicting_RT_with_spillover(rt.data, 'RelativeClause')
```

    ## [1] "This will take a while."
    ## [1] "Processing model gpt2"
    ## [1] "Processing model lstm"

``` r
saveRDS(rt.data, './saved_objects/predicted_dat_rc.rds')

filler.data <-  Predicting_RT_with_spillover(filler.data, 'filler')
```

    ## [1] "This will take a while."
    ## [1] "Processing model gpt2"
    ## [1] "Processing model lstm"

``` r
saveRDS(filler.data, './saved_objects/predicted_dat_filler.rds')
```

**Correcting for the effect of word position**

``` r
position_fit_lmer_nocor <- lmer(RT ~ scale(WordPosition) + (1 + scale(WordPosition) || participant), subset(filler.data, model=='lstm')) # model doesn't matter for RT. 
# We want data from only one model to avoid duplicating rows. 

position_fit_lm <- lm(RT ~ scale(WordPosition), subset(filler.data, model=='lstm'))

summary(position_fit_lmer_nocor)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: RT ~ scale(WordPosition) + (1 + scale(WordPosition) || participant)
    ##    Data: subset(filler.data, model == "lstm")
    ## 
    ## REML criterion at convergence: 13194745
    ## 
    ## Scaled residuals: 
    ##    Min     1Q Median     3Q    Max 
    ## -4.772 -0.401 -0.150  0.161 35.921 
    ## 
    ## Random effects:
    ##  Groups        Name                Variance Std.Dev.
    ##  participant   (Intercept)         10292.4  101.45  
    ##  participant.1 scale(WordPosition)   256.5   16.01  
    ##  Residual                          32826.6  181.18  
    ## Number of obs: 995814, groups:  participant, 2000
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)          374.2622     2.2758 1998.9384  164.46   <2e-16 ***
    ## scale(WordPosition)   -7.7729     0.4015 1998.3065  -19.36   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scl(WrdPst) 0.000

``` r
summary(position_fit_lm)
```

    ## 
    ## Call:
    ## lm(formula = RT ~ scale(WordPosition), data = subset(filler.data, 
    ##     model == "lstm"))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -378.2 -108.8  -37.8   49.8 6596.4 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         374.2437     0.2087 1793.41   <2e-16 ***
    ## scale(WordPosition)  -7.7650     0.2087  -37.21   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 208.2 on 995812 degrees of freedom
    ##   (186 observations deleted due to missingness)
    ## Multiple R-squared:  0.001389,   Adjusted R-squared:  0.001388 
    ## F-statistic:  1385 on 1 and 995812 DF,  p-value: < 2.2e-16

``` r
rt.data$wordpos_predrt <- predict(position_fit_lmer_nocor, rt.data)
rt.data$wordpos_predrt_lm <- predict(position_fit_lm, rt.data)

rt.data$corrected_rt <- rt.data$RT - rt.data$wordpos_predrt
rt.data$corrected_rt_lm <- rt.data$RT - rt.data$wordpos_predrt_lm
```

### Predicted data from language models

``` r
print(getwd())
```

    ## [1] "/Users/grushaprasad/Documents/Work/Johns Hopkins/SAP Benchmark/sapbenchmark/analysis/spr/RelativeClause"

``` r
temp <- list()

i <- 1
for(m in unique(rt.data$model)){
  print(m)
  curr_rc <- subset(rt.data, model == m & !is.na(RT))
  curr_filler <- subset(filler.data, model == m & !is.na(RT))
  print(paste(nrow(curr_filler), nrow(filler.data)))
  
  curr_fit <-  lmer(predicted ~ scale(WordPosition) + (1 + scale(WordPosition) || participant),
                    curr_filler)

  print(summary(curr_fit))

  curr_rc$wordpos_predicted = predict(curr_fit, curr_rc)
  curr_rc$corrected_predicted = curr_rc$predicted - curr_rc$wordpos_predicted

  temp[[i]] <- curr_rc
  i <- i + 1
}
```

    ## [1] "gpt2"
    ## [1] "995814 1992000"

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0128012 (tol = 0.002, component 1)

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: predicted ~ scale(WordPosition) + (1 + scale(WordPosition) ||  
    ##     participant)
    ##    Data: curr_filler
    ## 
    ## REML criterion at convergence: 10148920
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4469 -0.6213 -0.0626  0.5674 12.9372 
    ## 
    ## Random effects:
    ##  Groups        Name                Variance  Std.Dev.
    ##  participant   (Intercept)         10241.646 101.201 
    ##  participant.1 scale(WordPosition)     4.361   2.088 
    ##  Residual                           1534.022  39.167 
    ## Number of obs: 995814, groups:  participant, 2000
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)          374.260      2.263 1998.136   165.4   <2e-16 ***
    ## scale(WordPosition)   -7.771      0.061 1998.336  -127.4   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scl(WrdPst) 0.000 
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0128012 (tol = 0.002, component 1)
    ## 
    ## [1] "lstm"
    ## [1] "995814 1992000"

    ## boundary (singular) fit: see ?isSingular

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: predicted ~ scale(WordPosition) + (1 + scale(WordPosition) ||  
    ##     participant)
    ##    Data: curr_filler
    ## 
    ## REML criterion at convergence: 10087305
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.3207 -0.6481 -0.0677  0.5974 11.3308 
    ## 
    ## Random effects:
    ##  Groups        Name                Variance Std.Dev.
    ##  participant   (Intercept)         10232    101.2   
    ##  participant.1 scale(WordPosition)     0      0.0   
    ##  Residual                           1444     38.0   
    ## Number of obs: 995814, groups:  participant, 2000
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)          3.743e+02  2.262e+00  1.999e+03   165.4   <2e-16 ***
    ## scale(WordPosition) -7.771e+00  3.808e-02  9.938e+05  -204.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scl(WrdPst) 0.000 
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
predicted_dat <- dplyr::bind_rows(temp)

predicted_dat <- predicted_dat %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))

rm(temp)
```

## Analyses with LMER models

### Empirical data

``` r
verb_dat <- rt.data %>%
  filter(model=='lstm') %>% # empirical data is same for lstm and gpt. 
  filter(ROI == 0) %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))

contrasts(verb_dat$Type)
```

    ##         RC_Obj
    ## RC_Subj      0
    ## RC_Obj       1

``` r
## part intercept is 0 because we removed out this intercept through word pos correction
fit_verb_lmer <- lmer(corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item), data=verb_dat)

summary(fit_verb_lmer)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 +  
    ##     Type_num || item)
    ##    Data: verb_dat
    ## 
    ## REML criterion at convergence: 225159.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5887 -0.3544 -0.1532  0.1180 20.5008 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num    23485.7  153.25  
    ##  item        (Intercept)   215.9   14.69  
    ##  item.1      Type_num     2314.0   48.10  
    ##  Residual                73833.5  271.72  
    ## Number of obs: 15908, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   26.026      4.276 24.777   6.087 2.41e-06 ***
    ## Type_num      52.064     11.258 28.107   4.625 7.67e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.193

``` r
saveRDS(fit_verb_lmer, './saved_objects/fit_verb_lmer')
```

**Looking at other word positions**

``` r
det_dat <- rt.data %>%
  filter(model=='lstm') %>%
  filter(ROI == 1) %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))

contrasts(det_dat$Type)
```

    ##         RC_Obj
    ## RC_Subj      0
    ## RC_Obj       1

``` r
## part intercept is 0 because we removed out this intercept through word pos correction
fit_det_lmer <- lmer(corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item), data=det_dat)

summary(fit_det_lmer)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 +  
    ##     Type_num || item)
    ##    Data: det_dat
    ## 
    ## REML criterion at convergence: 211645
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6886 -0.4010 -0.1339  0.1764 28.7521 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num     3227.28  56.809 
    ##  item        (Intercept)    35.13   5.927 
    ##  item.1      Type_num      149.67  12.234 
    ##  Residual                33560.23 183.195 
    ## Number of obs: 15912, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)
    ## (Intercept)   -2.436      2.384 35.516  -1.022    0.314
    ## Type_num       4.757      4.036 43.036   1.179    0.245
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.439

``` r
saveRDS(fit_det_lmer, './saved_objects/fit_det_lmer')



noun_dat <- rt.data %>%
  filter(model=='lstm') %>%
  filter(ROI == 2) %>%
  mutate(Type = factor(Type, levels = c('RC_Subj', 'RC_Obj')),
         Type_num = ifelse(Type == 'RC_Subj', 0, 1))

contrasts(noun_dat$Type)
```

    ##         RC_Obj
    ## RC_Subj      0
    ## RC_Obj       1

``` r
## part intercept is 0 because we removed out this intercept through word pos correction
fit_noun_lmer <- lmer(corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item), data=noun_dat)

summary(fit_noun_lmer)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 +  
    ##     Type_num || item)
    ##    Data: noun_dat
    ## 
    ## REML criterion at convergence: 221546.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4450 -0.4179 -0.1954  0.1157 21.0004 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num      860.2   29.33  
    ##  item        (Intercept)  1119.4   33.46  
    ##  item.1      Type_num      314.2   17.72  
    ##  Residual                64589.8  254.15  
    ## Number of obs: 15911, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   46.148      7.400 24.928   6.236 1.62e-06 ***
    ## Type_num      -6.177      5.455 25.534  -1.132    0.268    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.201

``` r
saveRDS(fit_noun_lmer, './saved_objects/fit_noun_lmer')
```

### Predicted data

``` r
fit_verb_lmer_pred_lstm <- lmer(corrected_predicted ~ Type_num +
                              (0 + Type_num || participant) +
                              (1 + Type_num || item),
                  data=subset(predicted_dat, ROI==0 & model == 'lstm' &!is.na(RT))
                  )

saveRDS(fit_verb_lmer_pred_lstm, './saved_objects/fit_verb_lmer_pred_lstm')

summary(fit_verb_lmer_pred_lstm)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_predicted ~ Type_num + (0 + Type_num || participant) +  
    ##     (1 + Type_num || item)
    ##    Data: subset(predicted_dat, ROI == 0 & model == "lstm" & !is.na(RT))
    ## 
    ## REML criterion at convergence: 130220.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1811 -0.4303 -0.0829  0.2592 13.9264 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num    254.5    15.95   
    ##  item        (Intercept) 232.1    15.23   
    ##  item.1      Type_num    372.9    19.31   
    ##  Residual                160.8    12.68   
    ## Number of obs: 15908, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   14.274      3.113 23.000   4.585 0.000131 ***
    ## Type_num      22.590      3.963 23.277   5.700    8e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.002

``` r
fit_verb_lmer_pred_gpt2 <- lmer(corrected_predicted ~ Type_num +
                              (0 + Type_num || participant) +
                              (1 + Type_num || item),
                  data=subset(predicted_dat, ROI==0 & model == 'gpt2' &!is.na(RT))
                  )

saveRDS(fit_verb_lmer_pred_gpt2, './saved_objects/fit_verb_lmer_pred_gpt2')

summary(fit_verb_lmer_pred_gpt2)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_predicted ~ Type_num + (0 + Type_num || participant) +  
    ##     (1 + Type_num || item)
    ##    Data: subset(predicted_dat, ROI == 0 & model == "gpt2" & !is.na(RT))
    ## 
    ## REML criterion at convergence: 139429.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.8919 -0.4377 -0.0713  0.2186 11.8886 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num    417.3    20.43   
    ##  item        (Intercept) 215.9    14.69   
    ##  item.1      Type_num    423.4    20.58   
    ##  Residual                290.4    17.04   
    ## Number of obs: 15908, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   17.012      3.005 23.001   5.661 9.20e-06 ***
    ## Type_num      23.459      4.234 23.415   5.541 1.16e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.003

``` r
#fit_det_bayes_pred <- readRDS('./saved_objects/fit_det_bayes_ored_prior1')
```

``` r
create_dfs_lmer <- function(fit, model_name){
  coef_typenum <- coef(summary(fit))[2, 'Estimate']
  se_typenum <- coef(summary(fit))[2, 'Std. Error']
  
  by_construction <- data.frame(ROI = 0,
                                coef = 'RC',
                                mean = coef_typenum,
                                lower = coef_typenum-(2*se_typenum),
                                upper = coef_typenum+(2*se_typenum))
  dir <- '../../../plots/spr/RelativeClause/'
  constr_fname <- ifelse(model_name == 'human', 
                         paste0(dir, 'by_construction_lmer.rds'),
                         paste0(dir, 'by_construction_lmer_', model_name, '.rds'))
  
  saveRDS(by_construction, constr_fname)
  
  
  by_item <- data.frame(ranef(fit)[['item']]) %>%
    add_rownames(var='item') %>%
    mutate(mean = coef_typenum + Type_num,
           lower = NA,
           upper = NA,
           ROI = 0,
           coef = 'RC') %>%
    select(item, ROI, coef, mean, lower, upper)
  
  item_fname <- ifelse(model_name == 'human', 
                         paste0(dir, 'by_item_lmer.rds'),
                         paste0(dir, 'by_item_lmer_', model_name, '.rds'))
  
  saveRDS(by_item, item_fname)
  
}
```

``` r
create_dfs_lmer(fit_verb_lmer, 'human')
```

    ## Warning: `add_rownames()` was deprecated in dplyr 1.0.0.
    ## Please use `tibble::rownames_to_column()` instead.

``` r
create_dfs_lmer(fit_verb_lmer_pred_gpt2, 'gpt2')
create_dfs_lmer(fit_verb_lmer_pred_lstm, 'lstm')
```

## Analyses with BRMS model

### Empirical data

``` r
brms_parms <- get_brms_parameters('prior1')

#Note brms automatically truncates the distributions for sd and sigma.
```

``` r
fit_verb_bayes <- brm(corrected_rt ~ Type_num +
                        (0 + Type_num || participant) +
                        (1 + Type_num || item),
                  data=verb_dat,
                  prior = brms_parms$prior,
                  cores = brms_parms$ncores,
                  iter = brms_parms$niters,
                  seed = brms_parms$seed,
                  warmup = brms_parms$warmup,
                  control = list(adapt_delta = brms_parms$adapt_delta)
                  )
```

    ## Warning: Rows containing NAs were excluded from the model.

    ## Compiling Stan program...

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
saveRDS(fit_verb_bayes, './saved_objects/fit_verb_bayes_prior1')

summary(fit_verb_bayes)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: verb_dat (Number of observations: 15908) 
    ##   Draws: 4 chains, each with iter = 12000; warmup = 6000; thin = 1;
    ##          total post-warmup draws = 24000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    15.20      4.87     5.91    25.31 1.00    10030    10078
    ## sd(Type_num)     51.14      9.46    35.70    72.68 1.00    11660    16341
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)   153.29      4.41   144.74   161.94 1.00     9919    14369
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    26.05      4.47    17.27    34.89 1.00    28915    20093
    ## Type_num     51.63     11.79    28.19    74.61 1.00    10595    13955
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma   271.75      1.63   268.58   274.99 1.00    32138    16982
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#fit_verb_bayes <- readRDS('./saved_objects/fit_verb_bayes_prior1')
```

``` r
fit_det_bayes <- brm(corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item),
                  data=det_dat,
                  prior = brms_parms$prior,
                  cores = brms_parms$ncores,
                  iter = brms_parms$niters,
                  seed = brms_parms$seed,
                  warmup = brms_parms$warmup,
                  control = list(adapt_delta = brms_parms$adapt_delta)
                  )
```

    ## Warning: Rows containing NAs were excluded from the model.

    ## Compiling Stan program...

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
saveRDS(fit_det_bayes, './saved_objects/fit_det_bayes_prior1')

summary(fit_det_bayes)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: det_dat (Number of observations: 15912) 
    ##   Draws: 4 chains, each with iter = 12000; warmup = 6000; thin = 1;
    ##          total post-warmup draws = 24000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     6.01      2.98     0.53    12.02 1.00     6509     9652
    ## sd(Type_num)     13.01      3.90     5.87    21.35 1.00     8434    10189
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    56.67      3.46    49.72    63.36 1.00     7681    11129
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -2.43      2.48    -7.34     2.43 1.00    28395    19013
    ## Type_num      4.74      4.23    -3.50    13.13 1.00    23878    18668
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma   183.25      1.10   181.14   185.42 1.00    28851    17977
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#fit_det_bayes <- readRDS('./saved_objects/fit_det_bayes_prior1')
```

``` r
fit_noun_bayes <- brm(corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item),
                  data=noun_dat,
                  prior = brms_parms$prior,
                  cores = brms_parms$ncores,
                  iter = brms_parms$niters,
                  seed = brms_parms$seed,
                  warmup = brms_parms$warmup,
                  control = list(adapt_delta = brms_parms$adapt_delta)
                  )
```

    ## Warning: Rows containing NAs were excluded from the model.

    ## Compiling Stan program...

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

    ## Warning: There were 348 divergent transitions after warmup. See
    ## https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    ## to find out why this is a problem and how to eliminate them.

    ## Warning: Examine the pairs() plot to diagnose sampling problems

``` r
saveRDS(fit_noun_bayes, './saved_objects/fit_noun_bayes_prior1')

summary(fit_noun_bayes)
```

    ## Warning: There were 348 divergent transitions after warmup. Increasing
    ## adapt_delta above 0.8 may help. See http://mc-stan.org/misc/
    ## warnings.html#divergent-transitions-after-warmup

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: noun_dat (Number of observations: 15911) 
    ##   Draws: 4 chains, each with iter = 12000; warmup = 6000; thin = 1;
    ##          total post-warmup draws = 24000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    35.66      6.32    25.41    50.37 1.00     5613    10394
    ## sd(Type_num)     18.25      6.53     4.79    31.36 1.00     4430     3396
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    24.94     11.03     2.44    43.69 1.00     1690     3318
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    46.16      8.00    30.30    61.91 1.00     4034     7712
    ## Type_num     -6.16      5.65   -17.27     4.88 1.00    14947    14694
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma   254.30      1.53   251.30   257.33 1.00    10383     9568
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#fit_noun_bayes <- readRDS('./saved_objects/fit_noun_bayes_prior1')
```

``` r
emp_dat_verb <- reshape_item_dat(fit_verb_bayes, "item") %>%
  mutate(ROI = 0)
emp_dat_det <- reshape_item_dat(fit_det_bayes, "item") %>%
  mutate(ROI = 1)
emp_dat_noun <- reshape_item_dat(fit_noun_bayes, "item")%>%
  mutate(ROI = 2)

emp_dat <- dplyr::bind_rows(emp_dat_verb, emp_dat_det, emp_dat_noun)
 
rm(emp_dat_verb, emp_dat_det, emp_dat_noun)

saveRDS(emp_dat, './saved_objects/rc_subset_sampledsumm_empirical')
```

``` r
by_construction <- emp_dat %>%
  mutate(diff = b_Type_num,
         coef = 'RC') %>%
  group_by(ROI, coef) %>%
  summarise(mean = mean(diff),
            lower = quantile(diff, 0.025)[[1]],
            upper = quantile(diff, 0.975)[[1]])
```

    ## `summarise()` has grouped output by 'ROI'. You can override using the `.groups` argument.

``` r
saveRDS(by_construction, '../../../plots/spr/RelativeClause/by_construction.rds')

by_construction <- readRDS('../../../plots/spr/RelativeClause/by_construction.rds')

by_item <- emp_dat %>%
  mutate(diff = b_Type_num + r_Type_num,
         coef = 'RC',
         item = as.numeric(item)) %>%
  group_by(item,ROI, coef) %>%
  summarise(mean = mean(diff),
            lower = quantile(diff, 0.025)[[1]],
            upper = quantile(diff, 0.975)[[1]]) 
```

    ## `summarise()` has grouped output by 'item', 'ROI'. You can override using the `.groups` argument.

``` r
saveRDS(by_item, '../../../plots/spr/RelativeClause/by_item.rds')

by_item <- readRDS('../../../plots/spr/RelativeClause/by_item.rds')

rt.data.human_only <- rt.data %>%
  filter(model == 'lstm') %>%
  select(!model)  # because merge_surprisal does not think there is a model
                  # column in the the rt.data

by_item_surprisalmerged <- merge_surprisal(rt.data.human_only,by_item,"RelativeClause")
```

    ## [1] 124672
    ## [1] 143235

    ## `summarise()` has grouped output by 'item.x', 'Type', 'ROI'. You can override using the `.groups` argument.

``` r
# Plot_empirical_construction_level(by_construction,"RelativeClause")
# 
# by_item$coef <- 'RC'
# Plot_itemwise_by_magnitude(by_item,"RelativeClause",ROI='Verb')
```

### Predicted data

``` r
fit_verb_bayes_pred_gpt2 <- brm(corrected_predicted ~ Type_num +
                              (0 + Type_num || participant) +
                              (1 + Type_num || item),
                  data=subset(predicted_dat, ROI==0 & model == 'gpt2' &!is.na(RT)),
                  prior = brms_parms$prior,
                  cores = brms_parms$ncores,
                  iter = brms_parms$niters,
                  seed = brms_parms$seed,
                  warmup = brms_parms$warmup,
                  control = list(adapt_delta = brms_parms$adapt_delta)
                  )
```

    ## Compiling Stan program...

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
saveRDS(fit_verb_bayes_pred_gpt2, './saved_objects/fit_verb_bayes_pred_gpt2_prior1')


#fit_verb_bayes_pred_gpt2 <- readRDS('./saved_objects/fit_verb_bayes_pred_gpt2_prior1')

summary(fit_verb_bayes_pred_gpt2)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_predicted ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: subset(predicted_dat, ROI == 0 & model == "gpt2" & (Number of observations: 15908) 
    ##   Draws: 4 chains, each with iter = 12000; warmup = 6000; thin = 1;
    ##          total post-warmup draws = 24000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    15.50      2.47    11.56    21.21 1.00     4270     7828
    ## sd(Type_num)     21.74      3.48    16.21    29.81 1.00     3713     8055
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    20.45      0.38    19.70    21.21 1.00     6689    12837
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    16.92      3.32    10.40    23.50 1.00     1724     3437
    ## Type_num     23.39      4.47    14.63    32.36 1.00     1936     4218
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    17.04      0.10    16.84    17.24 1.00    34438    18600
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
fit_verb_bayes_pred_lstm <- brm(corrected_predicted ~ Type_num +
                              (0 + Type_num || participant) +
                              (1 + Type_num || item),
                  data=subset(predicted_dat, ROI==0 & model == 'lstm' &!is.na(RT)),
                  prior = brms_parms$prior,
                  cores = brms_parms$ncores,
                  iter = brms_parms$niters,
                  seed = brms_parms$seed,
                  warmup = brms_parms$warmup,
                  control = list(adapt_delta = brms_parms$adapt_delta)
                  )
```

    ## Compiling Stan program...

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
saveRDS(fit_verb_bayes_pred_lstm, './saved_objects/fit_verb_bayes_pred_lstm_prior1')


#fit_verb_bayes_pred_lstm <- readRDS('./saved_objects/fit_verb_bayes_pred_lstm_prior1')

summary(fit_verb_bayes_pred_lstm)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_predicted ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: subset(predicted_dat, ROI == 0 & model == "lstm" & (Number of observations: 15908) 
    ##   Draws: 4 chains, each with iter = 12000; warmup = 6000; thin = 1;
    ##          total post-warmup draws = 24000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    16.12      2.57    12.03    21.99 1.00     3557     7345
    ## sd(Type_num)     20.38      3.25    15.23    27.86 1.00     3825     6755
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    15.97      0.30    15.39    16.55 1.00     5520    10218
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    14.37      3.25     7.95    20.74 1.00     1644     3419
    ## Type_num     22.62      4.17    14.26    30.87 1.00     1616     3505
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    12.68      0.08    12.53    12.83 1.00    24370    18561
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pred_dat_verb_gpt2 <- reshape_item_dat(fit_verb_bayes_pred_gpt2, "item") %>%
   mutate(ROI = 0,
          model = 'gpt2')


pred_dat_verb_lstm <- reshape_item_dat(fit_verb_bayes_pred_lstm, "item") %>%
   mutate(ROI = 0,
          model = 'lstm')

pred_dat_verb <- dplyr::bind_rows(pred_dat_verb_gpt2, pred_dat_verb_lstm)


saveRDS(pred_dat_verb_lstm, './saved_objects/rc_subset_sampledsumm_lstm')
saveRDS(pred_dat_verb_gpt2, './saved_objects/rc_subset_sampledsumm_gpt2')

saveRDS(pred_dat_verb, './saved_objects/rc_subset_sampledsumm_predicted')
```

``` r
for(m in unique(pred_dat_verb$model)){
  curr_dat <- subset(pred_dat_verb, model == m)
  print(paste(nrow(curr_dat), nrow(pred_dat_verb)))
  
  curr_by_construction <- curr_dat %>%
    mutate(diff = b_Type_num,
           coef='RC') %>%
    group_by(ROI, coef) %>%
    summarise(mean = mean(diff),
              lower = quantile(diff, 0.025)[[1]],
              upper = quantile(diff, 0.975)[[1]])
  
  dir <- '../../../plots/spr/RelativeClause/'
  
  saveRDS(curr_by_construction, paste0(dir,'by_construction_', m, '.rds'))
  
  curr_by_item <- curr_dat %>%
    mutate(diff = b_Type_num + r_Type_num,
           coef = 'RC',
           item = as.numeric(item)) %>%
    group_by(item,ROI, coef) %>%
    summarise(mean = mean(diff),
              lower = quantile(diff, 0.025)[[1]],
              upper = quantile(diff, 0.975)[[1]]) 
  
  saveRDS(curr_by_item, paste0(dir,'by_item_', m, '.rds'))
}
```

    ## [1] "576000 1152000"

    ## `summarise()` has grouped output by 'ROI'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'item', 'ROI'. You can override using the `.groups` argument.

    ## [1] "576000 1152000"

    ## `summarise()` has grouped output by 'ROI'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'item', 'ROI'. You can override using the `.groups` argument.
