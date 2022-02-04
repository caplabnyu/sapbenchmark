```R
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(stringr)
```

    
    Attaching package: 'lmerTest'
    
    
    The following object is masked from 'package:lme4':
    
        lmer
    
    
    The following object is masked from 'package:stats':
    
        step
    
    



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



<table class="dataframe">
<caption>A grouped_df: 435894 x 47</caption>
<thead>
	<tr><th scope=col>Sentence</th><th scope=col>WordPosition</th><th scope=col>EachWord</th><th scope=col>Time</th><th scope=col>MD5</th><th scope=col>Type</th><th scope=col>EventTime</th><th scope=col>Question.x</th><th scope=col>Answer</th><th scope=col>List</th><th scope=col>...</th><th scope=col>length_p1</th><th scope=col>length_p2</th><th scope=col>length_p3</th><th scope=col>logfreq</th><th scope=col>logfreq_p1</th><th scope=col>logfreq_p2</th><th scope=col>logfreq_p3</th><th scope=col>surprisal_p1</th><th scope=col>surprisal_p2</th><th scope=col>surprisal_p3</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>...</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1636487256</td><td>b18be91fab6e92d9d875b7213d85209d</td><td>FILLER1</td><td>1.64000e+12</td><td>Who hates the bill most?</td><td>0</td><td> r </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1634670345</td><td>f77b5899bb42378ae26a8c62fd778f72</td><td>FILLER1</td><td>1.63467e+12</td><td>Who hates the bill most?</td><td>0</td><td> e </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1630697959</td><td>c64b82c52eb7e645a8824134c223f79f</td><td>FILLER1</td><td>1.63000e+12</td><td>Who hates the bill most?</td><td>0</td><td> c </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1636134055</td><td>2448f7b9f1ce5b9de3810508e91b8d2c</td><td>FILLER1</td><td>1.64000e+12</td><td>Who hates the bill most?</td><td>0</td><td> o </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639594360</td><td>af8a7829842816888d78c2b31756127b</td><td>FILLER1</td><td>1.63959e+12</td><td>Who hates the bill most?</td><td>0</td><td> c </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1627335989</td><td>2db95cd08dc86e0140289cea47fddf9d</td><td>FILLER1</td><td>1.63000e+12</td><td>Who hates the bill most?</td><td>0</td><td> h </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1635790232</td><td>c4259766948a9bbcb471938830c3ba35</td><td>FILLER1</td><td>1.63579e+12</td><td>Who hates the bill most?</td><td>0</td><td> p </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639428323</td><td>4901b5398231a6f322d5ef1448a5ef34</td><td>FILLER1</td><td>1.63943e+12</td><td>Who hates the bill most?</td><td>0</td><td> l </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639083236</td><td>5c7412c111666bacff5dbd66075024eb</td><td>FILLER1</td><td>1.63908e+12</td><td>Who hates the bill most?</td><td>0</td><td> n </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639593201</td><td>4d4c228670ade561525af34b4b00bd6f</td><td>FILLER1</td><td>1.63959e+12</td><td>Who hates the bill most?</td><td>0</td><td> d </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639430386</td><td>48aebfa1f885d1215ba1fecf6396eb64</td><td>FILLER1</td><td>1.63943e+12</td><td>Who hates the bill most?</td><td>0</td><td> a </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1634942219</td><td>1ec83d43f3225a3bad829d9f7509b3c5</td><td>FILLER1</td><td>1.63000e+12</td><td>Who hates the bill most?</td><td>0</td><td> k </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1638981811</td><td>32d4cb0b690155331e9fe1a38bec0b65</td><td>FILLER1</td><td>1.63898e+12</td><td>Who hates the bill most?</td><td>0</td><td> m </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639516883</td><td>154a8d699db1c433edb3b15c44ca88a3</td><td>FILLER1</td><td>1.63952e+12</td><td>Who hates the bill most?</td><td>0</td><td> d </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1627687727</td><td>86df1ba7849308cde97f60b0db42a162</td><td>FILLER1</td><td>1.63000e+12</td><td>Who hates the bill most?</td><td>0</td><td> g </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1635366388</td><td>c36764e89bcd382023642cc748c535cc</td><td>FILLER1</td><td>1.63537e+12</td><td>Who hates the bill most?</td><td>0</td><td> a </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1635881955</td><td>857a7a0d953e41ab36ae5365bc5eb6d8</td><td>FILLER1</td><td>1.64000e+12</td><td>Who hates the bill most?</td><td>0</td><td> p </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1634847809</td><td>a0aa331cf61da324bce38dd3a9ff8138</td><td>FILLER1</td><td>1.63485e+12</td><td>Who hates the bill most?</td><td>0</td><td> c </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1635539586</td><td>d639802b8bf8571b0fb52b999be4595d</td><td>FILLER1</td><td>1.63554e+12</td><td>Who hates the bill most?</td><td>0</td><td> q </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639677609</td><td>e361a17c53d37c2b6ab8fd4b638bec16</td><td>FILLER1</td><td>1.63968e+12</td><td>Who hates the bill most?</td><td>0</td><td> l </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1635802912</td><td>c382c8a79050891974c635d88d6ccd12</td><td>FILLER1</td><td>1.63580e+12</td><td>Who hates the bill most?</td><td>0</td><td> q </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1638982414</td><td>039d9f51f317da81cb274a55bc310db1</td><td>FILLER1</td><td>1.63898e+12</td><td>Who hates the bill most?</td><td>0</td><td> k </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639508372</td><td>af69313dfde9bd8fd5617d5c37acbb6f</td><td>FILLER1</td><td>1.63951e+12</td><td>Who hates the bill most?</td><td>0</td><td> i </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639083148</td><td>efd3853435cd8ff2ac6f48c5939de822</td><td>FILLER1</td><td>1.63908e+12</td><td>Who hates the bill most?</td><td>0</td><td> r </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1627762512</td><td>a8ba40154ef4118a4780b6cfc3e147c5</td><td>FILLER1</td><td>1.63000e+12</td><td>Who hates the bill most?</td><td>0</td><td> r </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1634664840</td><td>6e4b5f3da56d34dbde6f892356435deb</td><td>FILLER1</td><td>1.63466e+12</td><td>Who hates the bill most?</td><td>0</td><td> d </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1639429814</td><td>74e90a58fe5ccd2edf4609fbb0ec2a26</td><td>FILLER1</td><td>1.63943e+12</td><td>Who hates the bill most?</td><td>0</td><td> o </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1636484789</td><td>174686ee274fb34a3b6d98f2d55a2c48</td><td>FILLER1</td><td>1.64000e+12</td><td>Who hates the bill most?</td><td>0</td><td> r </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1634679589</td><td>13722bd588c8a9bde2d48e7554e29aed</td><td>FILLER1</td><td>1.63468e+12</td><td>Who hates the bill most?</td><td>0</td><td> h </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>A bill was drafted and introduced into Parliament several times but met with great opposition, mostly from farmers.</td><td>5</td><td>and</td><td>1635188207</td><td>522fc535ef509e38d6e75e8335627602</td><td>FILLER1</td><td>1.63519e+12</td><td>Who hates the bill most?</td><td>0</td><td> i </td><td>...</td><td>7</td><td>3</td><td>4</td><td>14.5461</td><td>7.189168</td><td>13.62667</td><td>9.212737</td><td>4.788502</td><td>3.049126</td><td>9.728388</td></tr>
	<tr><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td></td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1638913560</td><td>69df7e07861e50c268c0edb54788f219</td><td>FILLER1</td><td>1.63891e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> a </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1636046227</td><td>49e8b5e61391e5dbea245bcc51c1fdcd</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> a </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635278857</td><td>3c537062bc3832146c344095d5c303d1</td><td>FILLER1</td><td>1.63528e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> e </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635356399</td><td>a20fac2317e22f97df232e387b39d8f1</td><td>FILLER1</td><td>1.63536e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> h </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1639421541</td><td>7c9666f727357e7aacdbcc59c333cea3</td><td>FILLER1</td><td>1.63942e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> e </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635869075</td><td>48090b364702a160f18ff32bc8b8f671</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> p </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1631221110</td><td>a873db0612ddd7cedd62c616b6d37841</td><td>FILLER1</td><td>1.63000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> g </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635373773</td><td>f10f00f9b0d5fd40a45258f4ea4e58a5</td><td>FILLER1</td><td>1.63537e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> d </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635289152</td><td>d54955f3bf4dc1f521f4d3c2c5ff0283</td><td>FILLER1</td><td>1.63529e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> h </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1636060318</td><td>a4dd21f21acc1e70531074b1128cdd1d</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> h </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635969152</td><td>c485a3c506edf47534392c5dd39572ae</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> i </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1631132970</td><td>b997156b37b995aca9e95df3113d23bf</td><td>FILLER1</td><td>1.63000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> r </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635286805</td><td>60eb83fe37ef08f0d72261223f1a3a35</td><td>FILLER1</td><td>1.63529e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> j </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1636047093</td><td>5a78df7d46960ad44d82f3a66ca42cf6</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> c </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635446509</td><td>b5c5b1669d225ba3cdd7618c5d8a1a71</td><td>FILLER1</td><td>1.63545e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> k </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1634776165</td><td>f27f64cb8c6bbef0325b588c2dc54d1e</td><td>FILLER1</td><td>1.63478e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> c </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1630690334</td><td>aa0e63345cca3ea95ae72cc1462726bf</td><td>FILLER1</td><td>1.63000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> m </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1639083975</td><td>2b229fd9c58fdc7414e5767c14372ea4</td><td>FILLER1</td><td>1.63908e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> d </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635885739</td><td>a7a18881dc6fcd8be53d20a591ebdc30</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> n </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1630606587</td><td>c3e8d54d93d281df75ae471bae40c10e</td><td>FILLER1</td><td>1.63000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> p </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635886669</td><td>76efd7e84ecc82dee7209b906a8918c7</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> h </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635969745</td><td>83d65ad30ab321a9a7aa0a2737daf9b9</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> j </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635787070</td><td>05ec71f9a266d8fe30a371db2a270ae4</td><td>FILLER1</td><td>1.63579e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> q </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1639171128</td><td>3e88295dad76a65bb3acbe4fff12a5cf</td><td>FILLER1</td><td>1.63917e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> j </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1630688934</td><td>8727fe2d88843fbd647f02de719278d7</td><td>FILLER1</td><td>1.63000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> e </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1634942564</td><td>7167f61c512bfdf8c64611e0cec0db6a</td><td>FILLER1</td><td>1.63000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> m </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1639083608</td><td>b9684c3e9cef31d12d6cd497ab16343d</td><td>FILLER1</td><td>1.63908e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> c </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635968871</td><td>d87b1aa89420c797e75395af89ee0b17</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> b </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1636145522</td><td>07fbbfb10c329a3384d977a7a4bb369b</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> m </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
	<tr><td>When the new world was first discovered it was found to be, like the old, well stocked with plants and animals.</td><td>20</td><td>and</td><td>1635881515</td><td>912b22238c08e0a3a95679f01aba7542</td><td>FILLER1</td><td>1.64000e+12</td><td>Was the old world well stocked with plants and animals?</td><td>1</td><td> c </td><td>...</td><td>6</td><td>4</td><td>7</td><td>14.5461</td><td>8.590815</td><td>13.17689</td><td>5.09375</td><td>6.347364</td><td>2.111576</td><td>6.914957</td></tr>
</tbody>
</table>




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
