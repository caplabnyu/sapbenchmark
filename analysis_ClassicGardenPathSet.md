SAP Benchmark (SPR): ClassicGP subset
================

### Load in data

``` r
rt.data <- read.csv("ClassicGardenPathSet.csv", header=TRUE) %>%
  filter(ROI %in% c(-2,-1,0,1,2)) %>%
  filter(RT <= 7000)
filler.data <- read.csv("Fillers.csv", header = TRUE) %>%
  filter(RT <=7000)
#column 'MD5' = participant's index
```

### Plotting the data

Lets start by plotting the mean RTs for words in the critical positions

``` r
rt.data_summ <- rt.data %>%
  group_by(ROI, AMBIG, CONSTRUCTION) %>%
  summarise(mean_rt = mean(RT),
            se_rt = sd(RT)/sqrt(n())) %>%
  ungroup() 
```

    ## `summarise()` has grouped output by 'ROI', 'AMBIG'. You can override using the `.groups` argument.

``` r
ggplot(rt.data_summ, aes(x=ROI, y=mean_rt, colour=CONSTRUCTION, shape=AMBIG)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_rt - (2*se_rt), 
                    ymax = mean_rt + (2*se_rt)),
                width=.5,position=position_dodge(0.02)) +
  labs(x = '', y = 'Mean RT (ms)')
```

![](analysis_ClassicGardenPathSet_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#all ambs (circles) are longer than their unamb counterparts (triangles)
#pretarget regions (-1) were very comparable
#pretarget regions (-2) interestingly differed for NPS and NPZ (possibly because of clause boundaries)
```

\#\#\#recode some factors and set contrasts

``` r
#itemwise (collapsing across constructions, 24*3=72)
rt.data$item72 <- ifelse(rt.data$CONSTRUCTION=="NPS",as.numeric(as.character(rt.data$item)),
                         ifelse(rt.data$CONSTRUCTION=="NPZ",as.numeric(as.character(rt.data$item))+24,as.numeric(as.character(rt.data$item))+48))
rt.data$item72 <- as.factor(rt.data$item72)

rt.data$SZM1 <- ifelse(
  rt.data$CONSTRUCTION=="NPS",1,0
)
rt.data$SZM2 <- ifelse(
  rt.data$CONSTRUCTION=="NPZ",1,0
)
```

### Fitting mixed effects model (two factors and their interaction)

``` r
#use || because the full models are too complex
#one lmer for each word position
lmer_0_AMBxCONSTR <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||MD5),data=rt.data[rt.data$ROI==0,])
```

    ## boundary (singular) fit: see ?isSingular

``` r
#one significant interaction: NPS similar to MVRR, and NPZ larger than MVRR at ROI0
summary(lmer_0_AMBxCONSTR)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RT ~ AMBUAMB * (SZM1 + SZM2) + ((1 | item) + (0 + AMBUAMB | item) +  
    ##     (0 + SZM1 | item) + (0 + SZM2 | item) + (0 + AMBUAMB:SZM1 |  
    ##     item) + (0 + AMBUAMB:SZM2 | item)) + ((1 | MD5) + (0 + AMBUAMB |  
    ##     MD5) + (0 + SZM1 | MD5) + (0 + SZM2 | MD5) + (0 + AMBUAMB:SZM1 |  
    ##     MD5) + (0 + AMBUAMB:SZM2 | MD5))
    ##    Data: rt.data[rt.data$ROI == 0, ]
    ## 
    ## REML criterion at convergence: 695674.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1001 -0.3542 -0.1409  0.1168 17.6101 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance  Std.Dev. 
    ##  MD5      AMBUAMB:SZM2 6.022e+04 2.454e+02
    ##  MD5.1    AMBUAMB:SZM1 1.787e-04 1.337e-02
    ##  MD5.2    SZM2         2.544e-04 1.595e-02
    ##  MD5.3    SZM1         4.536e-05 6.735e-03
    ##  MD5.4    AMBUAMB      1.528e+04 1.236e+02
    ##  MD5.5    (Intercept)  2.487e+04 1.577e+02
    ##  item     AMBUAMB:SZM2 8.696e+02 2.949e+01
    ##  item.1   AMBUAMB:SZM1 1.467e+03 3.830e+01
    ##  item.2   SZM2         5.725e+02 2.393e+01
    ##  item.3   SZM1         5.678e+01 7.535e+00
    ##  item.4   AMBUAMB      1.238e+03 3.518e+01
    ##  item.5   (Intercept)  3.161e+02 1.778e+01
    ##  Residual              1.087e+05 3.296e+02
    ## Number of obs: 47700, groups:  MD5, 2000; item, 24
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   458.557      6.301  72.775
    ## AMBUAMB        59.230      9.370   6.321
    ## SZM1          -37.446      5.531  -6.770
    ## SZM2          -27.566      7.242  -3.806
    ## AMBUAMB:SZM1  -13.658     10.877  -1.256
    ## AMBUAMB:SZM2   61.973     11.151   5.558
    ## 
    ## Correlation of Fixed Effects:
    ##              (Intr) AMBUAMB SZM1   SZM2   AMBUAMB:SZM1
    ## AMBUAMB      -0.237                                   
    ## SZM1         -0.408  0.272                            
    ## SZM2         -0.311  0.209   0.357                    
    ## AMBUAMB:SZM1  0.206 -0.282  -0.467 -0.181             
    ## AMBUAMB:SZM2  0.201 -0.277  -0.231 -0.353  0.240      
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
lmer_1_AMBxCONSTR <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||MD5),data=rt.data[rt.data$ROI==1,])
```

    ## boundary (singular) fit: see ?isSingular

``` r
#two significant interactions: NPS smaller than MVRR, and NPZ also smaller than MVRR at ROI1
summary(lmer_1_AMBxCONSTR)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RT ~ AMBUAMB * (SZM1 + SZM2) + ((1 | item) + (0 + AMBUAMB | item) +  
    ##     (0 + SZM1 | item) + (0 + SZM2 | item) + (0 + AMBUAMB:SZM1 |  
    ##     item) + (0 + AMBUAMB:SZM2 | item)) + ((1 | MD5) + (0 + AMBUAMB |  
    ##     MD5) + (0 + SZM1 | MD5) + (0 + SZM2 | MD5) + (0 + AMBUAMB:SZM1 |  
    ##     MD5) + (0 + AMBUAMB:SZM2 | MD5))
    ##    Data: rt.data[rt.data$ROI == 1, ]
    ## 
    ## REML criterion at convergence: 701437
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6454 -0.3852 -0.1484  0.1518 15.8516 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance Std.Dev.
    ##  MD5      AMBUAMB:SZM2  28725.5 169.49  
    ##  MD5.1    AMBUAMB:SZM1   3455.6  58.78  
    ##  MD5.2    SZM2              0.0   0.00  
    ##  MD5.3    SZM1              0.0   0.00  
    ##  MD5.4    AMBUAMB       32391.9 179.98  
    ##  MD5.5    (Intercept)   18307.9 135.31  
    ##  item     AMBUAMB:SZM2   2998.1  54.75  
    ##  item.1   AMBUAMB:SZM1   1678.9  40.97  
    ##  item.2   SZM2            342.3  18.50  
    ##  item.3   SZM1              0.0   0.00  
    ##  item.4   AMBUAMB        1692.8  41.14  
    ##  item.5   (Intercept)     817.7  28.60  
    ##  Residual              124293.0 352.55  
    ## Number of obs: 47704, groups:  MD5, 2000; item, 24
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   446.055      7.707  57.876
    ## AMBUAMB       203.506     10.937  18.607
    ## SZM1           -1.360      5.691  -0.239
    ## SZM2           19.058      6.857   2.779
    ## AMBUAMB:SZM1 -139.347     11.720 -11.889
    ## AMBUAMB:SZM2  -52.835     14.343  -3.684
    ## 
    ## Correlation of Fixed Effects:
    ##              (Intr) AMBUAMB SZM1   SZM2   AMBUAMB:SZM1
    ## AMBUAMB      -0.191                                   
    ## SZM1         -0.372  0.261                            
    ## SZM2         -0.309  0.217   0.422                    
    ## AMBUAMB:SZM1  0.180 -0.258  -0.484 -0.204             
    ## AMBUAMB:SZM2  0.147 -0.211  -0.201 -0.332  0.198      
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
lmer_2_AMBxCONSTR <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)+(1+AMBUAMB*(SZM1+SZM2)||item)+(1+AMBUAMB*(SZM1+SZM2)||MD5),data=rt.data[rt.data$ROI==2,])
```

    ## boundary (singular) fit: see ?isSingular

``` r
###two significant interactions: NPS smaller than MVRR, and NPZ also smaller than MVRR at ROI2
summary(lmer_2_AMBxCONSTR)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RT ~ AMBUAMB * (SZM1 + SZM2) + ((1 | item) + (0 + AMBUAMB | item) +  
    ##     (0 + SZM1 | item) + (0 + SZM2 | item) + (0 + AMBUAMB:SZM1 |  
    ##     item) + (0 + AMBUAMB:SZM2 | item)) + ((1 | MD5) + (0 + AMBUAMB |  
    ##     MD5) + (0 + SZM1 | MD5) + (0 + SZM2 | MD5) + (0 + AMBUAMB:SZM1 |  
    ##     MD5) + (0 + AMBUAMB:SZM2 | MD5))
    ##    Data: rt.data[rt.data$ROI == 2, ]
    ## 
    ## REML criterion at convergence: 677617.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.3344 -0.3989 -0.1462  0.1576 21.8581 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance  Std.Dev. 
    ##  MD5      AMBUAMB:SZM2 1.264e+04 1.124e+02
    ##  MD5.1    AMBUAMB:SZM1 2.000e-04 1.414e-02
    ##  MD5.2    SZM2         4.790e-05 6.921e-03
    ##  MD5.3    SZM1         0.000e+00 0.000e+00
    ##  MD5.4    AMBUAMB      9.411e+03 9.701e+01
    ##  MD5.5    (Intercept)  1.368e+04 1.169e+02
    ##  item     AMBUAMB:SZM2 1.145e+03 3.384e+01
    ##  item.1   AMBUAMB:SZM1 8.199e+02 2.863e+01
    ##  item.2   SZM2         3.187e+02 1.785e+01
    ##  item.3   SZM1         0.000e+00 0.000e+00
    ##  item.4   AMBUAMB      1.547e+03 3.933e+01
    ##  item.5   (Intercept)  7.546e+02 2.747e+01
    ##  Residual              7.663e+04 2.768e+02
    ## Number of obs: 47716, groups:  MD5, 2000; item, 24
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   430.009      6.949  61.883
    ## AMBUAMB       113.726      9.458  12.024
    ## SZM1            5.905      4.477   1.319
    ## SZM2           19.512      5.793   3.368
    ## AMBUAMB:SZM1  -88.665      8.643 -10.259
    ## AMBUAMB:SZM2  -48.820      9.751  -5.007
    ## 
    ## Correlation of Fixed Effects:
    ##              (Intr) AMBUAMB SZM1   SZM2   AMBUAMB:SZM1
    ## AMBUAMB      -0.152                                   
    ## SZM1         -0.325  0.238                            
    ## SZM2         -0.251  0.184   0.394                    
    ## AMBUAMB:SZM1  0.168 -0.250  -0.516 -0.203             
    ## AMBUAMB:SZM2  0.149 -0.222  -0.233 -0.358  0.245      
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

### Fitting mixed effects model (one-factor,ambiguity, only)

``` r
#Position 0
lmer_0_item72 <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=rt.data[rt.data$ROI==0,])
```

    ## boundary (singular) fit: see ?isSingular

``` r
#point estimate of ambiguity effect across constructions = 76 ms at ROI0
summary(lmer_0_item72)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RT ~ AMBUAMB + (1 + AMBUAMB | item72) + (1 + AMBUAMB | MD5)
    ##    Data: rt.data[rt.data$ROI == 0, ]
    ## 
    ## REML criterion at convergence: 696450
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9573 -0.3654 -0.1446  0.1292 17.5797 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  MD5      (Intercept)  17739.0 133.19        
    ##           AMBUAMB      16233.4 127.41   1.00 
    ##  item72   (Intercept)    688.4  26.24        
    ##           AMBUAMB       2618.2  51.17   -0.01
    ##  Residual             116145.2 340.80        
    ## Number of obs: 47700, groups:  MD5, 2000; item72, 72
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  437.464      4.852   90.17
    ## AMBUAMB       76.022      7.405   10.27
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr)
    ## AMBUAMB 0.088 
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
#averaging RT across Positions 0-2
lmer_item72_RTacross3words <- lmer(RTacross3words ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=rt.data[rt.data$ROI==0&rt.data$RTacross3words<=7000/3,])
#point estimate of ambiguity effect across constructions and positions = 90 ms
summary(lmer_item72_RTacross3words)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RTacross3words ~ AMBUAMB + (1 + AMBUAMB | item72) + (1 + AMBUAMB |  
    ##     MD5)
    ##    Data: rt.data[rt.data$ROI == 0 & rt.data$RTacross3words <= 7000/3,      ]
    ## 
    ## REML criterion at convergence: 636368.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5112 -0.5047 -0.1510  0.3000  9.1043 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  MD5      (Intercept) 16097.4  126.88       
    ##           AMBUAMB      8621.8   92.85   0.64
    ##  item72   (Intercept)   526.8   22.95       
    ##           AMBUAMB      1758.8   41.94   0.24
    ##  Residual             32105.3  179.18       
    ## Number of obs: 47603, groups:  MD5, 2000; item72, 72
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  442.145      4.099   107.9
    ## AMBUAMB       90.019      5.625    16.0
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr)
    ## AMBUAMB 0.239

\#\#\#testing reliability of itemwise GPE estimates (split-half
analysis)

``` r
set.seed(1111)

#create a new dataset sorting by MD5(worker_id), condition, and item
splithalf0 <- arrange(rt.data[rt.data$ROI==0,],MD5,Type,item)

#for each participant, divide the number of observations for each condition they have into two.
numberpercondpersubj0 <- aggregate(splithalf0$Time,by=list(splithalf0$MD5,splithalf0$Type),FUN=length)
numberpercondpersubj0$x1 <- round(numberpercondpersubj0$x/2)
numberpercondpersubj0$x2 <- numberpercondpersubj0$x-numberpercondpersubj0$x1
colnames(numberpercondpersubj0) <- c("MD5","Type","totalnumb","numb1","numb2")

splithalf0$splitgroup <- NA
#randomly assign "group1" to some observation of each condition for each participant (by using sample())
for(i in 1:nrow(numberpercondpersubj0)){
  splithalf0[splithalf0$MD5==numberpercondpersubj0[i,'MD5']&splithalf0$Type==numberpercondpersubj0[i,'Type'],]$splitgroup <- sample(c(rep("first",numberpercondpersubj0[i,'numb1']),rep("second",numberpercondpersubj0[i,'numb2'])))
}

#fit lmer for each half of the dataset
lmer0_firsthalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf0[splithalf0$splitgroup=="first",])
```

    ## boundary (singular) fit: see ?isSingular

``` r
lmer0_secondhalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf0[splithalf0$splitgroup=="second",])
```

    ## boundary (singular) fit: see ?isSingular

``` r
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB,ranef(lmer0_secondhalf)[['item72']]$AMBUAMB)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  ranef(lmer0_firsthalf)[["item72"]]$AMBUAMB and ranef(lmer0_secondhalf)[["item72"]]$AMBUAMB
    ## t = 8.6324, df = 70, p-value = 1.251e-12
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.5834702 0.8142834
    ## sample estimates:
    ##       cor 
    ## 0.7180753

``` r
#NPS
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB[1:24],ranef(lmer0_secondhalf)[['item72']]$AMBUAMB[1:24])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  ranef(lmer0_firsthalf)[["item72"]]$AMBUAMB[1:24] and ranef(lmer0_secondhalf)[["item72"]]$AMBUAMB[1:24]
    ## t = 4.4603, df = 22, p-value = 0.000196
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3957133 0.8548658
    ## sample estimates:
    ##       cor 
    ## 0.6891081

``` r
#NPZ
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB[25:48],ranef(lmer0_secondhalf)[['item72']]$AMBUAMB[25:48])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  ranef(lmer0_firsthalf)[["item72"]]$AMBUAMB[25:48] and ranef(lmer0_secondhalf)[["item72"]]$AMBUAMB[25:48]
    ## t = 3.2311, df = 22, p-value = 0.00384
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2125435 0.7899250
    ## sample estimates:
    ##       cor 
    ## 0.5673003

``` r
#MVRR
cor.test(ranef(lmer0_firsthalf)[['item72']]$AMBUAMB[49:72],ranef(lmer0_secondhalf)[['item72']]$AMBUAMB[49:72])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  ranef(lmer0_firsthalf)[["item72"]]$AMBUAMB[49:72] and ranef(lmer0_secondhalf)[["item72"]]$AMBUAMB[49:72]
    ## t = 3.3146, df = 22, p-value = 0.003151
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2264260 0.7953465
    ## sample estimates:
    ##       cor 
    ## 0.5771098

``` r
#overall pretty high reliability of item-wise GP effects (note, however, results vary somewhat with different random seeds)
```

\#\#\#Same analysis for ROI1 and ROI2

``` r
#do this for ROI1 (spillover1)
set.seed(1111)
splithalf1 <- arrange(rt.data[rt.data$ROI==1,],MD5,Type,item)
numberpercondpersubj1 <- aggregate(splithalf1$Time,by=list(splithalf1$MD5,splithalf1$Type),FUN=length)
numberpercondpersubj1$x1 <- round(numberpercondpersubj1$x/2)
numberpercondpersubj1$x2 <- numberpercondpersubj1$x-numberpercondpersubj1$x1
colnames(numberpercondpersubj1) <- c("MD5","Type","totalnumb","numb1","numb2")
splithalf1$splitgroup <- NA
for(i in 1:nrow(numberpercondpersubj1)){
  splithalf1[splithalf1$MD5==numberpercondpersubj1[i,'MD5']&splithalf1$Type==numberpercondpersubj1[i,'Type'],]$splitgroup <- sample(c(rep("first",numberpercondpersubj1[i,'numb1']),rep("second",numberpercondpersubj1[i,'numb2'])))
}
lmer1_firsthalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf1[splithalf1$splitgroup=="first",])
```

    ## boundary (singular) fit: see ?isSingular

``` r
lmer1_secondhalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf1[splithalf1$splitgroup=="second",])
```

    ## boundary (singular) fit: see ?isSingular

``` r
#high reliability for ROI1 too
cor.test(ranef(lmer1_firsthalf)[['item72']]$AMBUAMB,ranef(lmer1_secondhalf)[['item72']]$AMBUAMB)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  ranef(lmer1_firsthalf)[["item72"]]$AMBUAMB and ranef(lmer1_secondhalf)[["item72"]]$AMBUAMB
    ## t = 12.918, df = 70, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.7543551 0.8966553
    ## sample estimates:
    ##       cor 
    ## 0.8393403

``` r
#for ROI2 (spillover2)
splithalf2 <- arrange(rt.data[rt.data$ROI==2,],MD5,Type,item)
numberpercondpersubj2 <- aggregate(splithalf2$Time,by=list(splithalf2$MD5,splithalf2$Type),FUN=length)
numberpercondpersubj2$x1 <- round(numberpercondpersubj2$x/2)
numberpercondpersubj2$x2 <- numberpercondpersubj2$x-numberpercondpersubj2$x1
colnames(numberpercondpersubj2) <- c("MD5","Type","totalnumb","numb1","numb2")
splithalf2$splitgroup <- NA
for(i in 1:nrow(numberpercondpersubj2)){
  splithalf2[splithalf2$MD5==numberpercondpersubj2[i,'MD5']&splithalf2$Type==numberpercondpersubj2[i,'Type'],]$splitgroup <- sample(c(rep("first",numberpercondpersubj2[i,'numb1']),rep("second",numberpercondpersubj2[i,'numb2'])))
}
lmer2_firsthalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf2[splithalf2$splitgroup=="first",])
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00485495 (tol = 0.002, component 1)

``` r
lmer2_secondhalf <- lmer(RT ~ AMBUAMB+(1+AMBUAMB|item72)+(1+AMBUAMB|MD5),data=splithalf2[splithalf2$splitgroup=="second",])
```

    ## boundary (singular) fit: see ?isSingular

``` r
#high reliability for ROI2 too
cor.test(ranef(lmer2_firsthalf)[['item72']]$AMBUAMB,ranef(lmer2_secondhalf)[['item72']]$AMBUAMB)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  ranef(lmer2_firsthalf)[["item72"]]$AMBUAMB and ranef(lmer2_secondhalf)[["item72"]]$AMBUAMB
    ## t = 10.313, df = 70, p-value = 1.091e-15
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.6644653 0.8545199
    ## sample estimates:
    ##       cor 
    ## 0.7765891

\#\#\#compare empirical data with language models’ predictions

``` r
#load lm's results
lm_prediction <- read.csv("lm_prediction.csv",header=T)

#pre-run the script for the brm versions, and load them here
load("brms_results_GP.RData")

ps_P0 <- posterior_summary(brm_P0_item72)
#extract only random item-slopes
randomslope_names_item72 <- rownames(ps_P0)[grepl("item72",rownames(ps_P0))][76:147]
#extract the posterior for each iteration
psamp_P0_item72 <- posterior_samples(brm_P0_item72, fixed=TRUE, pars=c("b_AMBUAMB",randomslope_names_item72))
```

    ## Warning: Method 'posterior_samples' is deprecated. Please see ?as_draws for
    ## recommended alternatives.

``` r
#summing the posterior for fixed and random effect
for(i in 2:73){
  psamp_P0_item72[,i+72] =psamp_P0_item72[,1]+psamp_P0_item72[,i]
}
psamp_P0_item72 <- psamp_P0_item72[,74:145]; colnames(psamp_P0_item72) <- paste0("item",as.character(1:72))

#total iterations = 6000
#therefore, 150-5850 = 95% HDI posterior estimates
model_based_predictions_P0_item72 <- data.frame(ITEM72_MEAN_P0 = colMeans(psamp_P0_item72))
for(i in 1:72){
  model_based_predictions_P0_item72$HDI_low_P0[i] <- sort(psamp_P0_item72[,i])[151]
  model_based_predictions_P0_item72$HDI_high_P0[i] <- sort(psamp_P0_item72[,i])[5850]
  model_based_predictions_P0_item72$SE_P0[i] <- (sort(psamp_P0_item72[,i])[5850]-sort(psamp_P0_item72[,i])[151])/4}
model_based_predictions_P0_item72$item72 <- 1:72

lm_prediction <- left_join(lm_prediction,model_based_predictions_P0_item72)
```

    ## Joining, by = "item72"

``` r
#overall r = 0.5 (ignoring consturctions) for wiki lstm lm
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb, lm_prediction$ITEM72_MEAN_P0)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$lstmsurprisaldiff_ambminusuamb and lm_prediction$ITEM72_MEAN_P0
    ## t = 4.8646, df = 70, p-value = 6.816e-06
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3066912 0.6577246
    ## sample estimates:
    ##       cor 
    ## 0.5026463

``` r
#overall r = 0.4 (ignoring constructions) for GPT2
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb, lm_prediction$ITEM72_MEAN_P0)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$gptsurprisaldiff_ambminusuamb and lm_prediction$ITEM72_MEAN_P0
    ## t = 3.722, df = 70, p-value = 0.0003959
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1929569 0.5832079
    ## sample estimates:
    ##      cor 
    ## 0.406456

``` r
#zoomming inside each construction for each lm
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="MVRR"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type == "MVRR"] and lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type == "MVRR"]
    ## t = 1.8124, df = 22, p-value = 0.0836
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.0502824  0.6668636
    ## sample estimates:
    ##       cor 
    ## 0.3604251

``` r
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPS"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPS"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPS"] and lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type == "NPS"]
    ## t = 2.7991, df = 22, p-value = 0.01046
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1374855 0.7589611
    ## sample estimates:
    ##       cor 
    ## 0.5124605

``` r
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPZ"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPZ"] and lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type == "NPZ"]
    ## t = 0.55178, df = 22, p-value = 0.5867
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3007357  0.4968163
    ## sample estimates:
    ##       cor 
    ## 0.1168349

``` r
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="MVRR"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type == "MVRR"] and lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type == "MVRR"]
    ## t = -0.29685, df = 22, p-value = 0.7694
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4549672  0.3491295
    ## sample estimates:
    ##         cor 
    ## -0.06316295

``` r
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPS"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPS"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPS"] and lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type == "NPS"]
    ## t = -0.057199, df = 22, p-value = 0.9549
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4135564  0.3931365
    ## sample estimates:
    ##         cor 
    ## -0.01219403

``` r
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"], lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type=="NPZ"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPZ"] and lm_prediction$ITEM72_MEAN_P0[lm_prediction$Type == "NPZ"]
    ## t = 1.7453, df = 22, p-value = 0.09489
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.06360691  0.65937412
    ## sample estimates:
    ##      cor 
    ## 0.348738

\#\#\#The rest is just plotting

``` r
#error bar = 95HDI
#NPS
NPS_itemgpe_by_lstmsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPS",],aes(x=lstmsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.51, (NPS)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="green4")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPS_itemgpe_by_lstmsurprisal <- set_panel_size(NPS_itemgpe_by_lstmsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))
#NPZ
NPZ_itemgpe_by_lstmsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPZ",],aes(x=lstmsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.12, (NPZ)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="blue")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPZ_itemgpe_by_lstmsurprisal <- set_panel_size(NPZ_itemgpe_by_lstmsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))
#MVRR
MVRR_itemgpe_by_lstmsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="MVRR",],aes(x=lstmsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.36, (Wiki-LSTM, MVRR)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="red")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
MVRR_itemgpe_by_lstmsurprisal <- set_panel_size(MVRR_itemgpe_by_lstmsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))

#NPS
NPS_itemgpe_by_gptsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPS",],aes(x=gptsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = -0.01, (NPS)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="green4")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPS_itemgpe_by_gptsurprisal <- set_panel_size(NPS_itemgpe_by_gptsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))

#NPZ
NPZ_itemgpe_by_gptsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="NPZ",],aes(x=gptsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r = 0.35, (NPZ)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="blue")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
NPZ_itemgpe_by_gptsurprisal <- set_panel_size(NPZ_itemgpe_by_gptsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))

#MVRR
MVRR_itemgpe_by_gptsurprisal <- ggplot(lm_prediction[lm_prediction$Type=="MVRR",],aes(x=gptsurprisaldiff_ambminusuamb,y=ITEM72_MEAN_P0))+
  labs(title = "r =-0.06, (GPT-2, MVRR)")+
  xlab("")+
  ylab("")+
  geom_pointrange(aes(ymin=HDI_low_P0, ymax=HDI_high_P0),color="red")+
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size = 13))
MVRR_itemgpe_by_gptsurprisal <- set_panel_size(MVRR_itemgpe_by_gptsurprisal,width=unit(5,"cm"),height=unit(5,"cm"))


grid.arrange(MVRR_itemgpe_by_lstmsurprisal,
             NPS_itemgpe_by_lstmsurprisal,
             NPZ_itemgpe_by_lstmsurprisal,
             MVRR_itemgpe_by_gptsurprisal,
             NPS_itemgpe_by_gptsurprisal,
             NPZ_itemgpe_by_gptsurprisal,nrow=2,
             bottom=textGrob("Surprisal difference at the disambiguating verb (ambig - unambig)", gp=gpar(fontsize=14)),
             left=textGrob("Empirical Garden Path Effect at the disambiguating verb", gp=gpar(fontsize=14),rot=90))
```

![](analysis_ClassicGardenPathSet_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(lm_prediction,aes(x=item,y=ITEM72_MEAN_P0,color=Type))+
  geom_point(size=2)+
  xlab("Triplet Index")+
  ylab("Garden path effect at the verb")+
  ggtitle("Comparing GPEs within a triplet of three constructions")+
  geom_errorbar(aes(ymin=HDI_low_P0,ymax=HDI_high_P0),width=.3)+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14))
```

![](analysis_ClassicGardenPathSet_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

\#\#\#plotting human’s mean GPE

``` r
ps_P0_AMBxCONSTR <- posterior_summary(brm_P0_AMBxCONSTR)
randomslope_names_AMBxCONSTR <- rownames(ps_P0_AMBxCONSTR)[which(grepl("r_item",rownames(ps_P0_AMBxCONSTR)))][-(1:15)]
psamp_P0_AMBxCONSTR <- posterior_samples(brm_P0_AMBxCONSTR,fixed=TRUE,pars=c("b_AMBUAMB", "b_AMBUAMB:SZM1", "b_AMBUAMB:SZM2"))
```

    ## Warning: Method 'posterior_samples' is deprecated. Please see ?as_draws for
    ## recommended alternatives.

``` r
psamp_P1_AMBxCONSTR <- posterior_samples(brm_P1_AMBxCONSTR,fixed=TRUE,pars=c("b_AMBUAMB", "b_AMBUAMB:SZM1", "b_AMBUAMB:SZM2"))
```

    ## Warning: Method 'posterior_samples' is deprecated. Please see ?as_draws for
    ## recommended alternatives.

``` r
psamp_P2_AMBxCONSTR <- posterior_samples(brm_P2_AMBxCONSTR,fixed=TRUE,pars=c("b_AMBUAMB", "b_AMBUAMB:SZM1", "b_AMBUAMB:SZM2"))
```

    ## Warning: Method 'posterior_samples' is deprecated. Please see ?as_draws for
    ## recommended alternatives.

``` r
mean_3positions <- data.frame(CONSTRUCTION=c("MVRR","NPS","NPZ"),
                               POSITION=c(rep(c("Disambiguating Verb","Spillover1","Spillover2"),each=3)),
                               MEAN=c(mean(psamp_P0_AMBxCONSTR[,1]),mean((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,2])),mean((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,3])),mean(psamp_P1_AMBxCONSTR[,1]),mean((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,2])),mean((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,3])),mean(psamp_P2_AMBxCONSTR[,1]),mean((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,2])),mean((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,3]))),
                               HDI_low=c(sort(psamp_P0_AMBxCONSTR[,1])[151],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,2]))[151],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,3]))[151],sort(psamp_P1_AMBxCONSTR[,1])[151],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,2]))[151],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,3]))[151],sort(psamp_P2_AMBxCONSTR[,1])[151],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,2]))[151],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,3]))[151]),
                               HDI_high=c(sort(psamp_P0_AMBxCONSTR[,1])[5850],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,2]))[5850],sort((psamp_P0_AMBxCONSTR[,1]+psamp_P0_AMBxCONSTR[,3]))[5850],sort(psamp_P1_AMBxCONSTR[,1])[5850],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,2]))[5850],sort((psamp_P1_AMBxCONSTR[,1]+psamp_P1_AMBxCONSTR[,3]))[5850],sort(psamp_P2_AMBxCONSTR[,1])[5850],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,2]))[5850],sort((psamp_P2_AMBxCONSTR[,1]+psamp_P2_AMBxCONSTR[,3]))[5850]))
mean_3positions <- mutate(mean_3positions,SE=(HDI_high-HDI_low)/4)
Human <- ggplot(data=mean_3positions, aes(x=POSITION, y=MEAN, fill=CONSTRUCTION)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=MEAN-2*SE,ymax=MEAN+2*SE),width=.2,position=position_dodge(.9))+
  xlab("Human data")+
  ylab("Mean Garden Path Effect")+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.position="top")
plot(Human)
```

![](analysis_ClassicGardenPathSet_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

\#\#\#Plotting lm’s prediction

``` r
lm_mean <- melt(lm_prediction[,c('item','Type','lstmsurprisaldiff_ambminusuamb','gptsurprisaldiff_ambminusuamb','lstmsurprisaldiff_ambminusuamb_spillover1','lstmsurprisaldiff_ambminusuamb_spillover2','gptsurprisaldiff_ambminusuamb_spillover1','gptsurprisaldiff_ambminusuamb_spillover2')],
                    id.vars=c("item",'Type'),
                    variable.name="DV",value.name="surprisaldiff")
lm_mean$position <- ifelse(grepl("spillover1",lm_mean$DV),"Spillover1",
                               ifelse(grepl("spillover2",lm_mean$DV),"Spillover2","Disambiguating Verb"))
lm_mean$LM <- ifelse(grepl("gpt",lm_mean$DV),"GPT-2","Wiki-LSTM")
A1 <- aggregate(lm_mean$surprisaldiff,
                by=list(lm_mean$Type,lm_mean$LM,lm_mean$position),FUN=mean) %>% filter(Group.2=="Wiki-LSTM")
A2 <- aggregate(lm_mean$surprisaldiff,
                by=list(lm_mean$Type,lm_mean$LM,lm_mean$position),FUN=mean) %>% filter(Group.2=="GPT-2")
lm_mean_wiki <- ggplot(data=A1[A1$Group.3=="Disambiguating Verb",], aes(x=Group.1, y=x)) +
  geom_bar(stat="identity",position=position_dodge(),fill=c("#FF6666","#32CD32","#6495ED"))+
  xlab("Wiki-LSTM (at Disambiguating Verb)")+
  ylab("Mean Surprisal Difference")+
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(size=0),
        axis.text.y = element_text(size=14,face="bold"))
lm_mean_gpt <- ggplot(data=A2[A2$Group.3=="Disambiguating Verb",], aes(x=Group.1, y=x)) +
  geom_bar(stat="identity",position=position_dodge(),fill=c("#FF6666","#32CD32","#6495ED"))+
  xlab("GPT-2 (at Disambiguating Verb)")+
  ylab("Mean Surprisal Difference")+
  theme(legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(size=0),
        axis.text.y = element_text(size=14,face="bold"))
grid.arrange(lm_mean_wiki, lm_mean_gpt,nrow=2)
```

![](analysis_ClassicGardenPathSet_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

\#\#\#inspect results if using RTacross3words (rather than word by word)

``` r
psamp_RTacross3words <- posterior_samples(brm_P0_item72_RTacross3words, fixed=TRUE, pars=c("b_AMBUAMB",randomslope_names_item72))
```

    ## Warning: Method 'posterior_samples' is deprecated. Please see ?as_draws for
    ## recommended alternatives.

``` r
for(i in 2:73){
  psamp_RTacross3words[,i+72] =psamp_RTacross3words[,1]+psamp_RTacross3words[,i]
}
psamp_RTacross3words <- psamp_RTacross3words[,74:145]; colnames(psamp_RTacross3words) <- paste0("item",as.character(1:72))
model_based_predictions_item72_RTacross3words <- data.frame(ITEM72_MEAN_RTacross3words = colMeans(psamp_RTacross3words))
for(i in 1:72){
  model_based_predictions_item72_RTacross3words$HDI_low_RTacross3words[i] <- sort(psamp_RTacross3words[,i])[151]
  model_based_predictions_item72_RTacross3words$HDI_high_RTacross3words[i] <- sort(psamp_RTacross3words[,i])[5850]
  model_based_predictions_item72_RTacross3words$SE_RTacross3words[i] <- (sort(psamp_RTacross3words[,i])[5850]-sort(psamp_RTacross3words[,i])[151])/4}
model_based_predictions_item72_RTacross3words$item72 <- 1:72


#GPT2 performed better than Wiki-LSTM (contrary to the ROI0 results)
#but both still do badly when zooming inside each construction
lm_prediction <- left_join(lm_prediction,model_based_predictions_item72_RTacross3words)
```

    ## Joining, by = "item72"

``` r
cor.test(lm_prediction$ITEM72_MEAN_RTacross3words,lm_prediction$lstmsurprisaldiff_ambminusuamb)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$ITEM72_MEAN_RTacross3words and lm_prediction$lstmsurprisaldiff_ambminusuamb
    ## t = 3.4424, df = 70, p-value = 0.0009768
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1632211 0.5625787
    ## sample estimates:
    ##       cor 
    ## 0.3805016

``` r
cor.test(lm_prediction$ITEM72_MEAN_RTacross3words,lm_prediction$gptsurprisaldiff_ambminusuamb)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$ITEM72_MEAN_RTacross3words and lm_prediction$gptsurprisaldiff_ambminusuamb
    ## t = 5.4343, df = 70, p-value = 7.552e-07
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3582504 0.6893841
    ## sample estimates:
    ##       cor 
    ## 0.5447105

``` r
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"], lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type=="MVRR"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type == "MVRR"] and lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type == "MVRR"]
    ## t = 1.4513, df = 22, p-value = 0.1608
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1223924  0.6245249
    ## sample estimates:
    ##       cor 
    ## 0.2955988

``` r
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPS"], lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type=="NPS"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPS"] and lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type == "NPS"]
    ## t = 2.4126, df = 22, p-value = 0.02462
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.06623053 0.72671293
    ## sample estimates:
    ##       cor 
    ## 0.4574066

``` r
cor.test(lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"], lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type=="NPZ"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$lstmsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPZ"] and lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type == "NPZ"]
    ## t = 1.0857, df = 22, p-value = 0.2894
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1956949  0.5764612
    ## sample estimates:
    ##       cor 
    ## 0.2255037

``` r
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="MVRR"], lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type=="MVRR"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type == "MVRR"] and lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type == "MVRR"]
    ## t = 0.35632, df = 22, p-value = 0.725
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3379739  0.4649397
    ## sample estimates:
    ##        cor 
    ## 0.07575041

``` r
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPS"], lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type=="NPS"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPS"] and lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type == "NPS"]
    ## t = 2.3773, df = 22, p-value = 0.02656
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.05956062 0.72353756
    ## sample estimates:
    ##      cor 
    ## 0.452095

``` r
cor.test(lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type=="NPZ"], lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type=="NPZ"])
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  lm_prediction$gptsurprisaldiff_ambminusuamb[lm_prediction$Type == "NPZ"] and lm_prediction$ITEM72_MEAN_RTacross3words[lm_prediction$Type == "NPZ"]
    ## t = 1.2604, df = 22, p-value = 0.2207
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1607070  0.6000889
    ## sample estimates:
    ##       cor 
    ## 0.2595136

\#\#\#look at timecourse effects (e.g., fatigue, adaptation)

``` r
#look at syntactic-independent timecourse effects (i.e., look at filler sentences that had no specific syntactic structures)
#averaging reading times across all positions for each filler sentnece

filler_averaging_readingtime_bytrial <- filler.data %>% group_by(MD5,Sentence,item) %>% summarise(mean_rt = mean(RT,na.rm=T),
            se_rt = sd(RT)/sqrt(n()))
```

    ## `summarise()` has grouped output by 'MD5', 'Sentence'. You can override using the `.groups` argument.

``` r
filler_averaging_readingtime_bytrial <- left_join(filler_averaging_readingtime_bytrial,distinct(filler.data,MD5,Sentence,trialnumber))
```

    ## Joining, by = c("MD5", "Sentence")

``` r
summary(lmer(mean_rt~trialnumber+(1|item)+(1|MD5),data=filler_averaging_readingtime_bytrial))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: mean_rt ~ trialnumber + (1 | item) + (1 | MD5)
    ##    Data: filler_averaging_readingtime_bytrial
    ## 
    ## REML criterion at convergence: 928572.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -35.816  -0.513  -0.104   0.351  37.642 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  MD5      (Intercept) 10747.7  103.67  
    ##  item     (Intercept)   575.1   23.98  
    ##  Residual              5759.0   75.89  
    ## Number of obs: 80000, groups:  MD5, 2000; item, 40
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept) 464.58933    4.50859   103.0
    ## trialnumber  -1.49712    0.01433  -104.5
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## trialnumber -0.158

``` r
ggplot(filler_averaging_readingtime_bytrial, aes(x=trialnumber, y=mean_rt)) +
    geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](analysis_ClassicGardenPathSet_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
aggregate(filler_averaging_readingtime_bytrial$mean_rt,by=list(filler_averaging_readingtime_bytrial$trialnumber),FUN=mean)
```

    ##    Group.1        x
    ## 1        1 223.3333
    ## 2        2 485.2156
    ## 3        3 498.5951
    ## 4        4 470.0307
    ## 5        5 480.2456
    ## 6        7 459.5660
    ## 7        8 461.4639
    ## 8        9 449.2033
    ## 9       10 452.6163
    ## 10      12 435.5439
    ## 11      13 439.8446
    ## 12      14 424.2954
    ## 13      15 432.1183
    ## 14      17 418.4797
    ## 15      18 427.2502
    ## 16      19 412.4878
    ## 17      20 415.4262
    ## 18      22 410.0802
    ## 19      23 415.5206
    ## 20      24 406.9533
    ## 21      25 410.9382
    ## 22      27 402.4658
    ## 23      28 405.1060
    ## 24      29 395.1101
    ## 25      30 402.1186
    ## 26      32 394.3497
    ## 27      33 397.7268
    ## 28      34 388.4231
    ## 29      35 393.3050
    ## 30      37 388.6013
    ## 31      38 388.7237
    ## 32      39 385.1348
    ## 33      40 383.3895
    ## 34      41 381.2541
    ## 35      42 380.6250
    ## 36      44 381.5451
    ## 37      45 384.6779
    ## 38      46 377.5715
    ## 39      47 382.4253
    ## 40      49 376.7389
    ## 41      50 372.9537
    ## 42      51 374.9415
    ## 43      52 378.7731
    ## 44      54 373.4866
    ## 45      55 373.8909
    ## 46      56 372.0582
    ## 47      57 371.6949
    ## 48      59 367.0496
    ## 49      60 373.9153
    ## 50      61 370.5609
    ## 51      62 365.5796
    ## 52      64 365.7831
    ## 53      65 362.0732
    ## 54      66 365.4451
    ## 55      67 364.1654
    ## 56      69 363.0137
    ## 57      70 360.7769
    ## 58      71 363.0864
    ## 59      72 362.3464
    ## 60      73 361.9385
    ## 61      74 358.9115
    ## 62      75 367.8786
    ## 63      76 374.6366
    ## 64      77 363.0491
    ## 65      78 367.6934
    ## 66      79 362.8273
    ## 67      80 370.1192
    ## 68      81 359.1141
    ## 69      82 372.3802
    ## 70      83 371.8568
    ## 71      84 365.1301
    ## 72      85 368.2614
    ## 73      86 357.1296
    ## 74      87 360.2668
    ## 75      88 360.7212
    ## 76      89 369.4147
    ## 77      90 362.4976
    ## 78      91 364.5810
    ## 79      92 358.1542

``` r
#significant speed-up as trial number went up
```

\#\#\#look at syntactic adaption

``` r
numbeachcell <- distinct(rt.data,MD5,Type,item,trialnumber) %>% group_by(MD5,Type) %>% mutate(numbeachcell=n()) %>% ungroup() %>%  arrange(MD5,Type,trialnumber) %>% distinct(MD5,Type,numbeachcell)
Seq_each_cell_eachparticipant <-c()
for(i in 1:nrow(numbeachcell)){
  Seq_each_cell_eachparticipant <- c(Seq_each_cell_eachparticipant,1:as.numeric(numbeachcell[i,'numbeachcell']))
}
rt.data <- left_join(rt.data,distinct(rt.data,MD5,item) %>% mutate(seq_each_cell_eachparticipant=Seq_each_cell_eachparticipant))
```

    ## Joining, by = c("MD5", "item")

``` r
rt.data$seq_each_cell_eachparticipant <- as.factor(rt.data$seq_each_cell_eachparticipant)


#takes forever to run the model with full random slopes
lmer_0_AMBxCONSTRxSeq <- lmer(RT ~ AMBUAMB*(SZM1+SZM2)*seq_each_cell_eachparticipant +(1|item)+(1|MD5),data=rt.data[rt.data$ROI==0,])
summary(lmer_0_AMBxCONSTRxSeq)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RT ~ AMBUAMB * (SZM1 + SZM2) * seq_each_cell_eachparticipant +  
    ##     (1 | item) + (1 | MD5)
    ##    Data: rt.data[rt.data$ROI == 0, ]
    ## 
    ## REML criterion at convergence: 697819.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8707 -0.3847 -0.1430  0.1179 17.6240 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  MD5      (Intercept)  38468.2 196.13  
    ##  item     (Intercept)    561.4  23.69  
    ##  Residual             121028.1 347.89  
    ## Number of obs: 47700, groups:  MD5, 2000; item, 24
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error t value
    ## (Intercept)                                  451.904     10.233  44.161
    ## AMBUAMB                                       69.331     11.154   6.216
    ## SZM1                                         -31.474     11.249  -2.798
    ## SZM2                                         -23.416     11.235  -2.084
    ## seq_each_cell_eachparticipant2                12.655     11.244   1.125
    ## seq_each_cell_eachparticipant3                 7.175     11.117   0.645
    ## seq_each_cell_eachparticipant4                 9.783     11.272   0.868
    ## AMBUAMB:SZM1                                 -27.090     15.832  -1.711
    ## AMBUAMB:SZM2                                  52.055     15.767   3.302
    ## AMBUAMB:seq_each_cell_eachparticipant2       -25.608     15.866  -1.614
    ## AMBUAMB:seq_each_cell_eachparticipant3       -26.121     15.824  -1.651
    ## AMBUAMB:seq_each_cell_eachparticipant4        -8.960     16.017  -0.559
    ## SZM1:seq_each_cell_eachparticipant2          -11.387     15.953  -0.714
    ## SZM1:seq_each_cell_eachparticipant3            0.773     15.860   0.049
    ## SZM1:seq_each_cell_eachparticipant4          -13.307     16.028  -0.830
    ## SZM2:seq_each_cell_eachparticipant2           -9.003     15.998  -0.563
    ## SZM2:seq_each_cell_eachparticipant3          -11.620     15.775  -0.737
    ## SZM2:seq_each_cell_eachparticipant4          -10.311     16.021  -0.644
    ## AMBUAMB:SZM1:seq_each_cell_eachparticipant2   25.617     22.423   1.142
    ## AMBUAMB:SZM1:seq_each_cell_eachparticipant3   23.021     22.457   1.025
    ## AMBUAMB:SZM1:seq_each_cell_eachparticipant4   28.825     22.671   1.271
    ## AMBUAMB:SZM2:seq_each_cell_eachparticipant2   16.878     22.462   0.751
    ## AMBUAMB:SZM2:seq_each_cell_eachparticipant3   37.370     22.425   1.666
    ## AMBUAMB:SZM2:seq_each_cell_eachparticipant4   17.675     22.547   0.784

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
rt.data$Type_sumcoded1 <- ifelse(rt.data$Type=="NPS_UAMB",5/6,-1/6)
rt.data$Type_sumcoded2 <- ifelse(rt.data$Type=="NPZ_AMB",5/6,-1/6)
rt.data$Type_sumcoded3 <- ifelse(rt.data$Type=="NPZ_UAMB",5/6,-1/6)
rt.data$Type_sumcoded4 <- ifelse(rt.data$Type=="MVRR_AMB",5/6,-1/6)
rt.data$Type_sumcoded5 <- ifelse(rt.data$Type=="MVRR_UAMB",5/6,-1/6)


#takes forever to run the model with full random slopes


lmer_0_TypexSeq <- lmer(RT ~ (Type_sumcoded1+Type_sumcoded2+Type_sumcoded3+Type_sumcoded4+Type_sumcoded5)*seq_each_cell_eachparticipant +(1|item)+(1|MD5),data=rt.data[rt.data$ROI==0,])
summary(lmer_0_TypexSeq)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## RT ~ (Type_sumcoded1 + Type_sumcoded2 + Type_sumcoded3 + Type_sumcoded4 +  
    ##     Type_sumcoded5) * seq_each_cell_eachparticipant + (1 | item) +  
    ##     (1 | MD5)
    ##    Data: rt.data[rt.data$ROI == 0, ]
    ## 
    ## REML criterion at convergence: 697819.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8707 -0.3847 -0.1430  0.1179 17.6240 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  MD5      (Intercept)  38468.2 196.13  
    ##  item     (Intercept)    561.4  23.69  
    ##  Residual             121028.1 347.89  
    ## Number of obs: 47700, groups:  MD5, 2000; item, 24
    ## 
    ## Fixed effects:
    ##                                                 Estimate Std. Error t value
    ## (Intercept)                                   472.433387   7.261366  65.061
    ## Type_sumcoded1                                -42.241039  11.229858  -3.761
    ## Type_sumcoded2                                 87.204049  11.113885   7.846
    ## Type_sumcoded3                                -34.182227  11.216807  -3.047
    ## Type_sumcoded4                                 58.564930  11.182909   5.237
    ## Type_sumcoded5                                -10.766492  11.169453  -0.964
    ## seq_each_cell_eachparticipant2                  0.137134   4.494899   0.031
    ## seq_each_cell_eachparticipant3                  0.563797   4.495555   0.125
    ## seq_each_cell_eachparticipant4                  5.180433   4.522829   1.145
    ## Type_sumcoded1:seq_each_cell_eachparticipant2  -0.009708  15.839303  -0.001
    ## Type_sumcoded1:seq_each_cell_eachparticipant3   3.100127  15.929800   0.195
    ## Type_sumcoded1:seq_each_cell_eachparticipant4 -19.864516  16.027480  -1.239
    ## Type_sumcoded2:seq_each_cell_eachparticipant2  -6.355663  15.834667  -0.401
    ## Type_sumcoded2:seq_each_cell_eachparticipant3   1.955557  15.939158   0.123
    ## Type_sumcoded2:seq_each_cell_eachparticipant4  -8.154008  15.903810  -0.513
    ## Type_sumcoded3:seq_each_cell_eachparticipant2   2.374152  15.919144   0.149
    ## Type_sumcoded3:seq_each_cell_eachparticipant3  -9.293308  15.838776  -0.587
    ## Type_sumcoded3:seq_each_cell_eachparticipant4 -16.868921  16.041201  -1.052
    ## Type_sumcoded4:seq_each_cell_eachparticipant2 -14.230635  15.829027  -0.899
    ## Type_sumcoded4:seq_each_cell_eachparticipant3 -23.794276  15.861404  -1.500
    ## Type_sumcoded4:seq_each_cell_eachparticipant4 -15.518380  16.076523  -0.965
    ## Type_sumcoded5:seq_each_cell_eachparticipant2  11.377007  15.878028   0.717
    ## Type_sumcoded5:seq_each_cell_eachparticipant3   2.327164  15.820016   0.147
    ## Type_sumcoded5:seq_each_cell_eachparticipant4  -6.557950  16.014523  -0.410

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
#lmer_0_TypexSeq_full <- lmer(RT ~ (Type_sumcoded1+Type_sumcoded2+Type_sumcoded3+Type_sumcoded4+Type_sumcoded5)*seq_each_cell_eachparticipant +(1+(Type_sumcoded1+Type_sumcoded2+Type_sumcoded3+Type_sumcoded4+Type_sumcoded5)*seq_each_cell_eachparticipant||item)+(1+(Type_sumcoded1+Type_sumcoded2+Type_sumcoded3+Type_sumcoded4+Type_sumcoded5)*seq_each_cell_eachparticipant||MD5),data=rt.data[rt.data$ROI==0,])
#summary(lmer_0_TypexSeq_full)
```
