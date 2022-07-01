SAP Benchmark (SPR): RC subset
================

## Load in data

### Empirical data

``` r
rt.data <- read.csv("../../../preprocessed_data/RelativeClauseSet.csv", header=TRUE) %>%
  filter(RT <= 7000) %>%
  filter(RT > 0) %>%
  rename(participant = MD5)

filler.data <- read.csv("../../../preprocessed_data/Fillers.csv", header = TRUE) %>%
  filter(RT <=7000) %>%
  filter(RT > 0) %>%
  rename(participant = MD5)
```

**Correcting for the effect of word position**

``` r
# position_fit_lmer <- lmer(RT ~ scale(WordPosition) + (1 + scale(WordPosition) | participant), filler.data)
position_fit_lmer_nocor <- lmer(RT ~ scale(WordPosition) + (1 + scale(WordPosition) || participant), filler.data)

position_fit_lm <- lm(RT ~ scale(WordPosition), filler.data)

#summary(position_fit_lmer)
summary(position_fit_lmer_nocor)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## RT ~ scale(WordPosition) + ((1 | participant) + (0 + scale(WordPosition) |  
    ##     participant))
    ##    Data: filler.data
    ## 
    ## REML criterion at convergence: 18977703
    ## 
    ## Scaled residuals: 
    ##    Min     1Q Median     3Q    Max 
    ## -4.324 -0.400 -0.161  0.145 32.206 
    ## 
    ## Random effects:
    ##  Groups        Name                Variance Std.Dev.
    ##  participant   (Intercept)         10812.5  103.98  
    ##  participant.1 scale(WordPosition)   506.5   22.51  
    ##  Residual                          43179.9  207.80  
    ## Number of obs: 1403516, groups:  participant, 2000
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error t value
    ## (Intercept)          388.531      2.332  166.63
    ## scale(WordPosition)   -7.191      0.533  -13.49
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scl(WrdPst) 0.000

``` r
summary(position_fit_lm)
```

    ## 
    ## Call:
    ## lm(formula = RT ~ scale(WordPosition), data = filler.data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -396.1 -117.7  -43.5   48.7 6590.1 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          388.498      0.197 1971.82   <2e-16 ***
    ## scale(WordPosition)   -7.173      0.197  -36.41   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 233.4 on 1403514 degrees of freedom
    ## Multiple R-squared:  0.0009434,  Adjusted R-squared:  0.0009427 
    ## F-statistic:  1325 on 1 and 1403514 DF,  p-value: < 2.2e-16

``` r
rt.data$wordpos_predrt <- predict(position_fit_lmer_nocor, rt.data)
rt.data$wordpos_predrt_lm <- predict(position_fit_lm, rt.data)

rt.data$corrected_rt <- rt.data$RT - rt.data$wordpos_predrt
rt.data$corrected_rt_lm <- rt.data$RT - rt.data$wordpos_predrt_lm
```

### Predicted data from language models

``` r
predicted_dat_rc <- Predicting_RT_with_spillover(rt.data, 'RelativeClause')
```

    ## [1] "This will take a while."
    ## [1] "Processing model gpt2"

    ## Loading required package: lmerTest

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    ## [1] "Processing model lstm"

``` r
predicted_dat_filler <-  Predicting_RT_with_spillover(filler.data, 'filler')
```

    ## [1] "This will take a while."
    ## [1] "Processing model gpt2"
    ## [1] "Processing model lstm"

``` r
print(getwd())
```

    ## [1] "/Users/grushaprasad/Documents/Work/Johns Hopkins/SAP Benchmark/sapbenchmark/analysis/spr/RelativeClause"

``` r
temp <- list()

i <- 1
for(m in unique(predicted_dat_rc$model)){
  print(m)
  curr_rc <- subset(predicted_dat_rc, model == m)
  curr_filler <- subset(predicted_dat_filler, model == m)
  print(paste(nrow(curr_filler), nrow(predicted_dat_filler)))
  
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
    ## [1] "303875 607750"
    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: predicted ~ scale(WordPosition) + (1 + scale(WordPosition) ||  
    ##     participant)
    ##    Data: curr_filler
    ## 
    ## REML criterion at convergence: 3098233
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.2543 -0.5927 -0.1022  0.4712 12.8304 
    ## 
    ## Random effects:
    ##  Groups        Name                Variance  Std.Dev.
    ##  participant   (Intercept)         1.014e+04 100.6949
    ##  participant.1 scale(WordPosition) 4.404e-01   0.6636
    ##  Residual                          1.498e+03  38.7050
    ## Number of obs: 303875, groups:  participant, 2000
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)          373.28858    2.25270 1998.86563   165.7   <2e-16 ***
    ## scale(WordPosition)   -9.78628    0.07176 1995.89521  -136.4   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scl(WrdPst) 0.000 
    ## [1] "lstm"
    ## [1] "303875 607750"

    ## boundary (singular) fit: see ?isSingular

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: predicted ~ scale(WordPosition) + (1 + scale(WordPosition) ||  
    ##     participant)
    ##    Data: curr_filler
    ## 
    ## REML criterion at convergence: 3063604
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4995 -0.6195 -0.1063  0.5094 10.9276 
    ## 
    ## Random effects:
    ##  Groups        Name                Variance Std.Dev.
    ##  participant   (Intercept)         10164    100.82  
    ##  participant.1 scale(WordPosition)     0      0.00  
    ##  Residual                           1336     36.55  
    ## Number of obs: 303875, groups:  participant, 2000
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)          3.733e+02  2.255e+00  2.000e+03   165.5   <2e-16 ***
    ## scale(WordPosition) -1.040e+01  6.631e-02  3.019e+05  -156.9   <2e-16 ***
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
    ## REML criterion at convergence: 225173.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6278 -0.3539 -0.1501  0.1179 20.4747 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num    23818    154.33  
    ##  item        (Intercept)   220     14.83  
    ##  item.1      Type_num     2338     48.35  
    ##  Residual                73823    271.70  
    ## Number of obs: 15908, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   15.835      4.295 24.768   3.686  0.00112 ** 
    ## Type_num      51.311     11.308 28.088   4.537 9.75e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.191

``` r
saveRDS(fit_verb_lmer, './saved_objects/fit_verb_lmer')
```

**Looking at other word positions**

``` r
det_dat <- rt.data %>%
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
fit_det_lmer <- lmer(corrected_rt ~ Type_num + (0 + Type_num | participant) + (1 + Type_num | item), data=det_dat)

summary(fit_det_lmer)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num | participant) + (1 +  
    ##     Type_num | item)
    ##    Data: det_dat
    ## 
    ## REML criterion at convergence: 211632.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6181 -0.4032 -0.1280  0.1772 28.8201 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev. Corr 
    ##  participant Type_num     3189.63  56.477       
    ##  item        (Intercept)    94.07   9.699       
    ##              Type_num      299.77  17.314  -0.75
    ##  Residual                33536.51 183.130       
    ## Number of obs: 15912, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  -13.025      2.853  22.994  -4.566 0.000137 ***
    ## Type_num       5.167      4.745  26.031   1.089 0.286157    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.698

``` r
saveRDS(fit_det_lmer, './saved_objects/fit_det_lmer')



noun_dat <- rt.data %>%
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
fit_noun_lmer <- lmer(corrected_rt ~ Type_num + (0 + Type_num | participant) + (1 + Type_num | item), data=noun_dat)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00268602 (tol = 0.002, component 1)

``` r
summary(fit_noun_lmer)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num | participant) + (1 +  
    ##     Type_num | item)
    ##    Data: noun_dat
    ## 
    ## REML criterion at convergence: 221586.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3810 -0.4189 -0.1906  0.1231 21.0427 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev. Corr 
    ##  participant Type_num      960.7   31.00        
    ##  item        (Intercept)  1220.4   34.93        
    ##              Type_num      345.3   18.58   -0.32
    ##  Residual                64708.5  254.38        
    ## Number of obs: 15911, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   35.195      7.680 22.979   4.582 0.000132 ***
    ## Type_num      -5.768      5.580 23.575  -1.034 0.311808    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Type_num -0.389
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.00268602 (tol = 0.002, component 1)

``` r
saveRDS(fit_noun_lmer, './saved_objects/fit_noun_lmer')
```

### Predicted data

``` r
fit_verb_lmer_pred_lstm <- lmer(corrected_predicted ~ Type_num +
                              (0 + Type_num || participant) +
                              (1 + Type_num || item),
                  data=subset(predicted_dat, ROI==0 & model == 'lstm')
                  )

saveRDS(fit_verb_lmer_pred_lstm, './saved_objects/fit_verb_lmer_pred_lstm')

summary(fit_verb_lmer_pred_lstm)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_predicted ~ Type_num + (0 + Type_num || participant) +  
    ##     (1 + Type_num || item)
    ##    Data: subset(predicted_dat, ROI == 0 & model == "lstm")
    ## 
    ## REML criterion at convergence: 130973.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0472 -0.4342 -0.0819  0.2619 13.8462 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num    270.6    16.45   
    ##  item        (Intercept) 236.0    15.36   
    ##  item.1      Type_num    373.5    19.33   
    ##  Residual                168.4    12.98   
    ## Number of obs: 15907, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)    8.775      3.139 23.001   2.795   0.0103 *  
    ## Type_num      26.506      3.968 23.293   6.681 7.64e-07 ***
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
                  data=subset(predicted_dat, ROI==0 & model == 'gpt2')
                  )

saveRDS(fit_verb_lmer_pred_gpt2, './saved_objects/fit_verb_lmer_pred_gpt2')

summary(fit_verb_lmer_pred_gpt2)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: corrected_predicted ~ Type_num + (0 + Type_num || participant) +  
    ##     (1 + Type_num || item)
    ##    Data: subset(predicted_dat, ROI == 0 & model == "gpt2")
    ## 
    ## REML criterion at convergence: 142037
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.0797 -0.4381 -0.0748  0.2205 10.7369 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant Type_num    495.6    22.26   
    ##  item        (Intercept) 217.2    14.74   
    ##  item.1      Type_num    423.4    20.58   
    ##  Residual                342.2    18.50   
    ## Number of obs: 15907, groups:  participant, 2000; item, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   13.134      3.016 23.001   4.355 0.000232 ***
    ## Type_num      26.370      4.240 23.486   6.219 2.19e-06 ***
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

    ## Loading required package: lmerTest

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    ## Warning: `add_rownames()` was deprecated in dplyr 1.0.0.
    ## Please use `tibble::rownames_to_column()` instead.

``` r
create_dfs_lmer(fit_verb_lmer_pred_gpt2, 'gpt2')
create_dfs_lmer(fit_verb_lmer_pred_lstm, 'lstm')
```

## Analyses with BRMS model

### Empirical data

``` r
prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),    
            prior("normal(0,500)", class = "sigma"))

#Note brms automatically truncates the distributions for sd and sigma.
```

``` r
fit_verb_bayes <- brm(corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item),
                  data=verb_dat,
                  prior = prior1,
                  cores = 4,
                  iter = 6000,
                  seed = 117
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
saveRDS(fit_verb_bayes, './saved_objects/fit_verb_bayes_prior1')

summary(fit_verb_bayes)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: verb_dat (Number of observations: 15908) 
    ##   Draws: 4 chains, each with iter = 6000; warmup = 3000; thin = 1;
    ##          total post-warmup draws = 12000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    15.41      4.88     6.11    25.50 1.00     5121     4316
    ## sd(Type_num)     51.34      9.66    35.55    73.32 1.00     6111     7226
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)   154.29      4.40   145.66   163.08 1.00     5086     7904
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    15.84      4.49     7.07    24.78 1.00    14329    10737
    ## Type_num     51.25     12.04    26.79    74.96 1.00     4763     7118
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma   271.75      1.63   268.53   274.94 1.00    16906     9459
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
                  prior = prior1,
                  cores = 4,
                  iter = 6000,
                  seed = 117
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
saveRDS(fit_det_bayes, './saved_objects/fit_det_bayes_prior1')

summary(fit_det_bayes)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: det_dat (Number of observations: 15912) 
    ##   Draws: 4 chains, each with iter = 6000; warmup = 3000; thin = 1;
    ##          total post-warmup draws = 12000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     6.21      3.02     0.67    12.45 1.00     2873     3958
    ## sd(Type_num)     13.00      3.88     5.67    21.27 1.00     3952     5216
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    56.24      3.44    49.37    62.85 1.00     3959     6493
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept   -13.02      2.50   -17.96    -8.06 1.00    16165    10267
    ## Type_num      5.19      4.21    -3.13    13.52 1.00    12027     9715
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma   183.25      1.10   181.12   185.45 1.00    14987     9640
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
                  prior = prior1,
                  cores = 4,
                  iter = 6000,
                  seed = 117
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
saveRDS(fit_noun_bayes, './saved_objects/fit_noun_bayes_prior1')

summary(fit_noun_bayes)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_rt ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: noun_dat (Number of observations: 15911) 
    ##   Draws: 4 chains, each with iter = 6000; warmup = 3000; thin = 1;
    ##          total post-warmup draws = 12000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    35.43      6.19    25.40    49.38 1.00     3567     7014
    ## sd(Type_num)     17.25      6.69     3.59    30.87 1.00     2501     1840
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    26.67     10.96     3.02    44.84 1.01      924     1494
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    35.38      8.04    19.42    50.87 1.00     2316     4145
    ## Type_num     -5.78      5.54   -16.81     5.16 1.00     9474     8218
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma   254.55      1.50   251.63   257.53 1.00     6648     7595
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

by_item_surprisalmerged <- merge_surprisal(rt.data,by_item,"RelativeClause")
```

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
                  data=subset(predicted_dat, ROI==0 & model == 'gpt2'),
                  prior = prior1,
                  cores = 4,
                  iter = 12000,
                  seed = 117
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
    ##    Data: subset(predicted_dat, ROI == 0 & model == "gpt2") (Number of observations: 15907) 
    ##   Draws: 4 chains, each with iter = 12000; warmup = 6000; thin = 1;
    ##          total post-warmup draws = 24000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    15.62      2.50    11.64    21.44 1.00     4763     8905
    ## sd(Type_num)     21.87      3.54    16.27    30.06 1.00     5535     9809
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    22.28      0.42    21.48    23.12 1.00     6538    11293
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    13.07      3.21     6.72    19.44 1.00     2265     4556
    ## Type_num     26.43      4.52    17.43    35.44 1.00     2144     4860
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    18.50      0.11    18.28    18.72 1.00    36587    18826
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
fit_verb_bayes_pred_lstm <- brm(corrected_predicted ~ Type_num +
                              (0 + Type_num || participant) +
                              (1 + Type_num || item),
                  data=subset(predicted_dat, ROI==0 & model == 'lstm'),
                  prior = prior1,
                  cores = 4,
                  iter = 12000,
                  seed = 117
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


fit_verb_bayes_pred_lstm <- readRDS('./saved_objects/fit_verb_bayes_pred_lstm_prior1')

summary(fit_verb_bayes_pred_lstm)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: corrected_predicted ~ Type_num + (0 + Type_num || participant) + (1 + Type_num || item) 
    ##    Data: subset(predicted_dat, ROI == 0 & model == "lstm") (Number of observations: 15907) 
    ##   Draws: 4 chains, each with iter = 12000; warmup = 6000; thin = 1;
    ##          total post-warmup draws = 24000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 24) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    16.36      2.60    12.21    22.26 1.00     2959     5291
    ## sd(Type_num)     20.44      3.30    15.25    27.94 1.00     2286     5153
    ## 
    ## ~participant (Number of levels: 2000) 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Type_num)    16.46      0.30    15.88    17.06 1.00     4962     9649
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     8.90      3.45     2.11    15.79 1.00     1173     2359
    ## Type_num     26.30      4.22    18.00    34.44 1.00     1165     2548
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    12.98      0.08    12.83    13.13 1.00    22569    18694
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
