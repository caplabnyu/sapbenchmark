from transformers import AutoTokenizer, AutoModelForCausalLM
import torch
import torch.nn.functional as F

import argparse
import csv
import re

from util import clean, align

parser = argparse.ArgumentParser()
parser.add_argument("--cuda", action="store_true")
parser.add_argument("--aligned", action="store_true")
parser.add_argument("--input", type=str)
parser.add_argument("--output", type=str)
# TODO option for selecting subword merges to compute

# how can we combine subwords/punctuation to get one surprisal per word?
merge_fs = {"sum_":sum, "mean_": lambda x: sum(x)/len(x)}

args = parser.parse_args()

# Load models from HF transformers
tokenizer = AutoTokenizer.from_pretrained("gpt2", add_prefix_space=True)
model = AutoModelForCausalLM.from_pretrained("gpt2")

if args.cuda:
    device = torch.device("cuda")
    model.to(device)


in_f = open(args.input, "r")
stims = csv.DictReader(in_f)

out = []
for stim_row in stims:
    inputs = tokenizer("<|endoftext|> " + stim_row["Sentence"], return_tensors="pt")
    ids = inputs["input_ids"]
    tokens = tokenizer.tokenize(stim_row["Sentence"])

    # run the model
    outputs = model(**inputs, labels=ids)
    logprobs = F.log_softmax(outputs.logits[0], 1)
    
    if args.aligned:
        words = stim_row["Sentence"].split()
        piecess, breaks = align(words, tokens)
        # get surp for each word (avgd over pieces) and write it to a new row
        for i, (word, pieces) in enumerate(zip(words, piecess)):
            row = stim_row.copy() # new object, not a reference
            row["token"] = ".".join(pieces)
            row["word"] = word
            row["word_pos"] = i
            # correct for alignment difference due to initial EOS in the model input. see get_lstm.py for details
            surps = [-logprobs[j][ids[0][j+1]].item() for j in range(breaks[i], breaks[i+1])]
            for merge_fn, merge_f in merge_fs.items():
                row[merge_fn + "surprisal"] = merge_f(surps)

            out.append(row)

    else:
        # get surp for each wordpiece and write it to a new row
        for i, (token, idx) in enumerate(zip(tokens, ids[0][1:])):
            row = stim_row.copy() # new object, not a reference
            row["token"] = token
            row["word"] = clean(token)
            row["word_pos"] = i
            row["surprisal"] = -logprobs[i][idx].item()
            out.append(row)

with open(args.output, "w") as out_f:
    writer = csv.DictWriter(out_f, fieldnames = out[0].keys())
    writer.writeheader()
    writer.writerows(out)

