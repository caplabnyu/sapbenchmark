## How to read the filenames

most files are named item_<subset>.pivot.<modelname>.csv, with the models considered being the Gulordava LSTM ("lstm"), GPT-2 ("gpt2"), or an RNNG ("rnng").

for the main subset, there is an additional file, item_main.pivot.<modelname>.post.csv, which only contains words in the critical region for convenience

## File Contents

Files contain the same columns as their corresponding google sheet with each row representing one word's suprisal, with the following notable columns:
    - word: the word whose suprisal is being measured
    - token: The model's representation of the word. if it was split into subword pieces, those are separated by a period ("."). make particular note of words that are not in the model's vocab which are maked as unkown tokens in angled brackets("<" and ">").
    - word_pos: Position of word in the sentence (0-indexed)
    - surprisal: The model's surprisal value at this word. If there are multiple subword pieces, this surprisal is the average of all of the component pieces.



