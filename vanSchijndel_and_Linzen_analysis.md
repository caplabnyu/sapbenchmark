```R
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(stringr)
```


```R
# Load in SPR and surprisal data for a subset

spr <- read.csv("./Fillers.csv")
spr$Sentence <- str_replace_all(spr$Sentence, "%2C", ",")
spr <- spr %>% filter(RT<=7000) 

surps <- read.csv("../data/gulordava/items_filler.lstm.csv")
surps[surps$surprisal == -1,]$surprisal <- NA # recode NA surprisals as real NAs
surps$word_pos = surps$word_pos + 1# adjust to 1-indexing


# Load in frequencies from the Gulordava Wikipedia corpus
freqs <- read.csv("./freqs.csv")

```


```R
# merge the two dfs such that we have the relevant surprisal and frequency with each rt

merged.freqs <- merge(x=spr, y=freqs, by.x="EachWord", by.y="word", all.x=TRUE)
merged.surps <- merge(x=merged.freqs, y=surps, 
                      by.x=c("Sentence", "WordPosition"), by.y=c("Sentence", "word_pos"), all.x=TRUE)
```


```R
# Store properties of past words in each row (going back 3 words)

merged.with_lags <- merged.surps %>% group_by_at(vars(Sentence, MD5)) %>%
                    mutate(RT_p1 = lag(RT), 
                           RT_p2 = lag(RT_p1), 
                           RT_p3 = lag(RT_p2),
                           length = nchar(EachWord),
                           length_p1 = lag(length), 
                           length_p2 = lag(length_p1),
                           length_p3 = lag(length_p2),
                           logfreq = log(count),
                           logfreq_p1 = lag(logfreq), 
                           logfreq_p2 = lag(logfreq_p1),
                           logfreq_p3 = lag(logfreq_p2),
                           surprisal_p1 = lag(surprisal),
                           surprisal_p2 = lag(surprisal_p1),
                           surprisal_p3 = lag(surprisal_p2)
                  )
```


```R
# drop rows with missing data (surprisals for past 3 words and freqs for past 3 words)

merged.drop <- subset(merged.with_lags, !is.na(surprisal) & !is.na(surprisal_p1) & 
                                        !is.na(surprisal_p2) & !is.na(surprisal_p3) &
                                        !is.na(logfreq) & !is.na(logfreq_p1) &
                                        !is.na(logfreq_p2) & !is.na(logfreq_p3))

# print number of remaining rows
print(nrow(merged.with_lags))
print(nrow(merged.drop))

```

    [1] 1403522
    [1] 435894



```R
models.filler <- lmer(data=merged.drop,
                      RT ~ surprisal + surprisal_p1 + surprisal_p2 + surprisal_p3 +
                           WordPosition + logfreq*length + logfreq_p1*length_p1 + 
                           logfreq_p2*length_p2 + logfreq_p3*length_p3 + (1 | MD5))
summary(models.filler) 

saveRDS(models.filler, "filler_lm.rds")
```

    
    Correlation matrix not shown by default, as p = 18 > 12.
    Use print(obj, correlation=TRUE)  or
        vcov(obj)        if you need it
    
    



    Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    lmerModLmerTest]
    Formula: RT ~ surprisal + surprisal_p1 + surprisal_p2 + surprisal_p3 +  
        WordPosition + logfreq * length + logfreq_p1 * length_p1 +  
        logfreq_p2 * length_p2 + logfreq_p3 * length_p3 + (1 | MD5)
       Data: merged.drop
    
    REML criterion at convergence: 5674196
    
    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -5.231 -0.411 -0.129  0.196 40.341 
    
    Random effects:
     Groups   Name        Variance Std.Dev.
     MD5      (Intercept)  9435     97.13  
     Residual             25830    160.72  
    Number of obs: 435894, groups:  MD5, 2000
    
    Fixed effects:
                           Estimate Std. Error         df t value Pr(>|t|)    
    (Intercept)           2.279e+02  7.690e+00  1.919e+05  29.633  < 2e-16 ***
    surprisal             2.732e+00  1.104e-01  4.339e+05  24.758  < 2e-16 ***
    surprisal_p1          3.356e+00  1.127e-01  4.339e+05  29.771  < 2e-16 ***
    surprisal_p2          2.001e+00  1.098e-01  4.339e+05  18.223  < 2e-16 ***
    surprisal_p3          1.226e+00  1.053e-01  4.339e+05  11.641  < 2e-16 ***
    WordPosition         -1.726e+00  5.003e-02  4.339e+05 -34.506  < 2e-16 ***
    logfreq               8.435e-01  2.313e-01  4.339e+05   3.646 0.000266 ***
    length                4.958e+00  4.551e-01  4.339e+05  10.894  < 2e-16 ***
    logfreq_p1            1.584e+00  2.500e-01  4.339e+05   6.335 2.37e-10 ***
    length_p1             5.417e+00  4.819e-01  4.339e+05  11.240  < 2e-16 ***
    logfreq_p2            2.557e+00  2.531e-01  4.339e+05  10.105  < 2e-16 ***
    length_p2             4.918e+00  4.809e-01  4.339e+05  10.227  < 2e-16 ***
    logfreq_p3            1.391e+00  2.281e-01  4.339e+05   6.097 1.08e-09 ***
    length_p3             1.790e+00  4.145e-01  4.339e+05   4.318 1.58e-05 ***
    logfreq:length        5.545e-02  4.750e-02  4.339e+05   1.167 0.243031    
    logfreq_p1:length_p1 -1.409e-01  5.006e-02  4.339e+05  -2.815 0.004881 ** 
    logfreq_p2:length_p2 -4.120e-01  5.049e-02  4.339e+05  -8.159 3.39e-16 ***
    logfreq_p3:length_p3 -1.691e-01  4.403e-02  4.339e+05  -3.840 0.000123 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



```R
# Now that we've fit a model on the fillers, load in and predict on criticals

agree.spr <- read.csv("./AgreementSet.csv")
agree.spr$Sentence <- str_replace_all(agree.spr$Sentence, "%2C", ",")
agree.spr <- agree.spr %>% filter(RT<=7000) 

agree.surps <- read.csv("../data/gulordava/items_agreement.lstm.csv")
agree.surps[agree.surps$surprisal == -1,]$surprisal <- NA # recode NA surprisals as real NAs
agree.surps$word_pos = agree.surps$word_pos + 1# adjust to 1-indexing

```


```R
agree.freqs <- merge(x=agree.spr, y=freqs, by.x="EachWord", by.y="word", all.x=TRUE)
agree.surps <- merge(x=agree.freqs, y=agree.surps, 
                      by.x=c("Sentence", "WordPosition"), by.y=c("Sentence", "word_pos"), all.x=TRUE)

```


```R
agree.with_lags <-  agree.surps %>% group_by_at(vars(Sentence, MD5)) %>%
                    mutate(RT_p1 = lag(RT), 
                           RT_p2 = lag(RT_p1), 
                           RT_p3 = lag(RT_p2),
                           length = nchar(EachWord),
                           length_p1 = lag(length), 
                           length_p2 = lag(length_p1),
                           length_p3 = lag(length_p2),
                           logfreq = log(count),
                           logfreq_p1 = lag(logfreq), 
                           logfreq_p2 = lag(logfreq_p1),
                           logfreq_p3 = lag(logfreq_p2),
                           surprisal_p1 = lag(surprisal),
                           surprisal_p2 = lag(surprisal_p1),
                           surprisal_p3 = lag(surprisal_p2)
                  )
```


```R
agree.drop <- subset(agree.with_lags, !is.na(surprisal) & !is.na(surprisal_p1) & 
                                        !is.na(surprisal_p2) & !is.na(surprisal_p3) &
                                        !is.na(logfreq) & !is.na(logfreq_p1) &
                                        !is.na(logfreq_p2) & !is.na(logfreq_p3))

# print number of remaining rows
print(nrow(agree.with_lags))
print(nrow(agree.drop))
```

    [1] 420676
    [1] 76829



```R
agree.drop$predicted <- predict(models.filler, newdata=agree.drop)
```


```R
head(agree.drop)
```


<table class="dataframe">
<caption>A grouped_df: 6 x 52</caption>
<thead>
	<tr><th scope=col>Sentence</th><th scope=col>WordPosition</th><th scope=col>EachWord</th><th scope=col>Time</th><th scope=col>MD5</th><th scope=col>Type</th><th scope=col>EventTime</th><th scope=col>Question.x</th><th scope=col>Answer</th><th scope=col>List</th><th scope=col>...</th><th scope=col>length_p2</th><th scope=col>length_p3</th><th scope=col>logfreq</th><th scope=col>logfreq_p1</th><th scope=col>logfreq_p2</th><th scope=col>logfreq_p3</th><th scope=col>surprisal_p1</th><th scope=col>surprisal_p2</th><th scope=col>surprisal_p3</th><th scope=col>predicted</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>...</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>After the diplomat signs, the agreement creates another border conflict as a side effect.</td><td>8</td><td>another</td><td>1639090449</td><td>23bb47ffc97720f565f97e9644e95124</td><td>AGREE</td><td>1.63909e+12</td><td>Does the agreement lead to peace?</td><td>0</td><td> f </td><td>...</td><td>9</td><td>3</td><td>10.4001</td><td>7.354362</td><td>8.817594</td><td>15.46925</td><td>8.870157</td><td>8.932127</td><td>1.84974</td><td>564.4141</td></tr>
	<tr><td>After the diplomat signs, the agreement creates another border conflict as a side effect.</td><td>8</td><td>another</td><td>1635868863</td><td>d29c1f4fdda6be958fa6edad3a6241b1</td><td>AGREE</td><td>1.64000e+12</td><td>Does the agreement lead to peace?</td><td>0</td><td> f </td><td>...</td><td>9</td><td>3</td><td>10.4001</td><td>7.354362</td><td>8.817594</td><td>15.46925</td><td>8.870157</td><td>8.932127</td><td>1.84974</td><td>406.8969</td></tr>
	<tr><td>After the diplomat signs, the agreement creates another border conflict as a side effect.</td><td>8</td><td>another</td><td>1630705356</td><td>e0577fbd716c919f278f03cd8528d783</td><td>AGREE</td><td>1.63000e+12</td><td>Does the agreement lead to peace?</td><td>0</td><td> f </td><td>...</td><td>9</td><td>3</td><td>10.4001</td><td>7.354362</td><td>8.817594</td><td>15.46925</td><td>8.870157</td><td>8.932127</td><td>1.84974</td><td>372.2721</td></tr>
	<tr><td>After the diplomat signs, the agreement creates another border conflict as a side effect.</td><td>8</td><td>another</td><td>1639089322</td><td>1747d390d6731a7cac89f160cbecd085</td><td>AGREE</td><td>1.63909e+12</td><td>Does the agreement lead to peace?</td><td>0</td><td> n </td><td>...</td><td>9</td><td>3</td><td>10.4001</td><td>7.354362</td><td>8.817594</td><td>15.46925</td><td>8.870157</td><td>8.932127</td><td>1.84974</td><td>367.1755</td></tr>
	<tr><td>After the diplomat signs, the agreement creates another border conflict as a side effect.</td><td>8</td><td>another</td><td>1635189532</td><td>1f3195181d005445cbb2a00051dde2d2</td><td>AGREE</td><td>1.63519e+12</td><td>Does the agreement lead to peace?</td><td>0</td><td> f </td><td>...</td><td>9</td><td>3</td><td>10.4001</td><td>7.354362</td><td>8.817594</td><td>15.46925</td><td>8.870157</td><td>8.932127</td><td>1.84974</td><td>337.1942</td></tr>
	<tr><td>After the diplomat signs, the agreement creates another border conflict as a side effect.</td><td>8</td><td>another</td><td>1636482344</td><td>af5d178c58828a26f8823f76b82e343e</td><td>AGREE</td><td>1.64000e+12</td><td>Does the agreement lead to peace?</td><td>0</td><td> p </td><td>...</td><td>9</td><td>3</td><td>10.4001</td><td>7.354362</td><td>8.817594</td><td>15.46925</td><td>8.870157</td><td>8.932127</td><td>1.84974</td><td>420.6281</td></tr>
</tbody>
</table>




```R

```
