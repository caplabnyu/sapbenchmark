## How to read the data filenames

Data files are named data/<modelname>/item_<subset>.<modelname>.csv, with the models considered being the Gulordava LSTM ("lstm"), GPT-2 ("gpt2"), or an RNNG ("rnng").

for the main subset, there is an additional file, item_main.<modelname>.post.csv, which only contains words in the critical region for convenience

## Data File Contents

Data files contain the same columns as their corresponding google sheet with each row representing one word's suprisal, with the following notable columns:
    - word: the word whose suprisal is being measured
    - token: The model's representation of the word. if it was split into subword pieces, those are separated by a period ("."). make particular note of words that are not in the model's vocab which are maked as unknown tokens in angled brackets("<" and ">").
    - word_pos: Position of word in the sentence (0-indexed)
    - {sum/mean}_surprisal: The model's surprisal value at this word. If there are multiple subword pieces, this surprisal is computed using the method indicated in the column name (sum or mean).

After post-processing, you'll also see the addition of:
    -length: the number of characters in the word
    -logfreq: the log-frequency (or, the log-count) of the word pulled from a user supplied set of corpus frequencies
    -*_s columns: the quantity indicated, but scaled and centered w.r.t. that quantity across all SAP subsets.

## Code to Compute Surprisals

- preprocess.py transforms the data in data/item_<subset>.csv into something model-ready in data/item_<subset>.pivot.csv
- get_<modelname>.py computes the surprisals for a particular model.
- postprocess.py selects only the rows in the critical region for the main subset.
- util.py defines the align function, which aligns subword pieces to the space-separated word boundaries that participants see in word-by-word self-paced reading
- rescale.py merges frequencies into the model outputs and creates centered and rescaled versions of predictors for the spillover analysis

Examples for each file's usage can be seen in scripts/<modelname>_surps.sh, each of which compute surprisals for a particular model over all of the subsets. 

To run the code for the Gulordava model or the RNNG, you need to both have a trained model and have loaded the corresponding submodule containing the model's source code (either Noji & Oseki (2021)'s RNNG implementation or Gulordava et al. (2018)'s LSTM implementation). This can be done by running "git submodule update --init". For the Gulordava model, you also need to download the training data/vocab to colorlessgreenRNNs/data/lm/English/. For gpt2, model loading is handled by the huggingface transformer library.


