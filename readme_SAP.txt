Reference: Huang, K.J., Arehalli, S., Kugemoto, M., Muxica, C., Prasad, G., Linzen, T., & Dillon, B. (under review)
A large-scale investigation of syntactic processing reveals misalignments between humans and neural language models.

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



