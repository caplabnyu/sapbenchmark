# Large-scale benchmark yields no evidence that language model surprisal explains syntactic disambiguation difficulty

## Introduction
This is the code repository for the paper [Large-scale benchmark yields no evidence that language model surprisal explains syntactic disambiguation difficulty](https://doi.org/10.1016/j.jml.2024.104510), which has been modified to support analyses with other language models.

## Original Readme
Data analysis and modeling were conducted via R.
Preprocessed (low accuracy removal, technical errors, etc) data (N=2000) are stored at https://drive.google.com/drive/folders/1g-oyH-XuB2oolo1d8KZfuFtiimuNyhjc
(see SAP_preprocessing.R for details)

To ensure correct dependency, the whole sapbenchmark folder should be downloaded as it is from GitHub
Data from different subsets can be loaded using the load_data() function sourced from "analysis/shared/util.R"

Large brm models (rds files) are downloadable at https://drive.google.com/drive/u/0/folders/1OAbjV7X8JvvrIxfxhoAvAkHNhGyxC4uN
and https://drive.google.com/drive/u/0/folders/1DZn5hnk_mFetL1DKBmgcnHhRzPhy4UaK
These models were constructed using the script in "analysis/spr/script_brm_prediction_subsetname.R" on NYU's greene HPC.

Plots in the main text of the manuscript can be reproduced using "plots/spr/generate_plots.Rmd"

SI folders contain norming data (COCA verb subcategorizaiton bias, plausibility norming, and cloze norming) and codes for calculating split-half reliability.

## Setup
A [conda environment](https://docs.anaconda.com/free/miniconda/index.html) with all the dependencies under `analysis/install.R` can be installed with the following command:
```
conda create -n gp r-base r-tidyverse r-ggplot2 r-lme4 r-dplyr r-tidyr r-lmerTest r-brms r-tidybayes
```

## Steps for Calculating Surprisal-Based Garden-Path Effect Magnitudes of LMs  

### Surprisal Calculation
Repositories like [this](https://github.com/byungdoh/llm_surprisal) can be used to calculate word-level surprisal of the stimuli sentences.
The stimuli sentences that can be used with the linked repository can be found under `Surprisals/data/stimuli/`.
The order of sentences (and therefore that of words) is important for smoothly interfacing with the subsequent steps.

Ideally, the output should have a 'model identifier' as the header of the surprisal column, so that the headers of the surprisal columns do not overlap.
```
$ head ClassicGP.gpt2.surprisal
word gpt2_surp
The 4.729296684265137
suspect 12.477431297302246
showed 12.849674224853516
the 5.330494403839111
file 12.128303527832031
deserved 21.008468627929688
further 9.304457664489746
investigation 0.6051654815673828
```

The surprisal columns can then be pasted to the baseline predictors under `Surprisals/data/anysurp/` (notice that the data is comma-delimited).
The file names should be kept as is for subsequent steps.
```
$ head items_byword_ClassicGP.csv
,item,condition,disambPositionAmb,disambPositionUnamb,ambiguity,Sentence,disambPosition_0idx,token,word,word_pos,logfreq,logfreq_s,length,length_s,gpt2_surp,gpt2-medium_surp
0,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,The,The,0,15.687451186819413,1.396106504820095,3,-0.8352420354837095,4.729296684265137,3.0605721473693848
1,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,suspect,suspect,1,8.249313746260636,-0.8583851357939485,7,0.6606739791349112,12.477431297302246,12.412561416625977
2,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,showed,showed,2,9.399389249519563,-0.5097984595066846,6,0.2866949754802561,12.849674224853516,11.88725757598877
3,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,the,the,3,15.687451186819413,1.396106504820095,3,-0.8352420354837095,5.330494403839111,5.775950908660889
4,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,file,file,4,8.696008608880904,-0.7229923924182657,4,-0.4612630318290542,12.128303527832031,11.945284843444824
5,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,deserved,deserved,5,6.742880635791903,-1.314983428288102,8,1.0346529827895663,21.008468627929688,21.44688606262207
6,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,further,further,6,9.655026193237628,-0.432315170544871,7,0.6606739791349112,9.304457664489746,6.800014019012451
7,1,NPS_AMB,6,7,ambiguous,The suspect showed the file deserved further investigation during the murder trial.,5,investigation,investigation,7,8.687442166975924,-0.7255888719561772,13,2.9045480010628424,0.6051654815673828,1.0947941541671753
```

### LMER Fitting on Filler Data
Once all five subsets have been processed, the next step is to derive conversion coefficients between surprisal and RT with `analysis/spr/Fillers_analysis_anysurp.R`, e.g.
```
Rscript Fillers_analysis_anysurp.R ../../Surprisals/data/anysurp/items_byword_Fillers.csv gpt2_surp
```
where the first argument is the .csv file with surprisal predictors, and the second argument is the surprisal predictor of interest.
When LMER fitting is complete, the resulting model will be saved under `filler_models/filler_model_gpt2_surp.rds`.

### Bayesian LMER Fitting on Garden-Path Data
With the 'filler' LMER model in place, the magnitude of garden-path effects can be estimated with `analysis/spr/script_brm_prediction_*_anysurp.R`, e.g.
```
Rscript script_brm_prediction_ClassicGP_anysurp.R gpt2_surp
```
where the first argument is the surprisal predictor of interest.
Once Bayesian LMER fitting is complete (also note that Bayesian LMER fitting is time-intensive), the resulting model for each region of interest will be saved under e.g. `brm_models/brm_predicted_gpt2_surp_ClassicGP_P0.rds`.

## Questions
For questions with the 'anysurp' procedures, please contact Byung-Doh Oh ([oh.531@osu.edu](mailto:oh.531@osu.edu)). Other questions should be directed to the authors of the paper.