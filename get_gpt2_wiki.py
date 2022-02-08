from transformers import GPT2TokenizerFast, GPT2LMHeadModel
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
parser.add_argument("--model", type=str)
parser.add_argument("--tokenizer", type=str)

args = parser.parse_args()

# Load models from HF transformers
tokenizer = GPT2TokenizerFast.from_pretrained(args.tokenizer)
model = GPT2LMHeadModel.from_pretrained(args.model)

if args.cuda:
    device = torch.device("cuda")
    model.to(device)


in_f = open(args.input, "r")
stims = csv.DictReader(in_f)

out = []
for stim_row in stims:
    inputs = tokenizer(stim_row["Sentence"], return_tensors="pt")
    ids = inputs["input_ids"]
    tokens = tokenizer.tokenize(stim_row["sent"])

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
            if i == 0:
                row["surprisal"] = -1
            else:
                surps = [-logprobs[j-1][ids[0][j]].item() for j in range(breaks[i], breaks[i+1])]
                row["surprisal"] = sum(surps)/len(surps)
            out.append(row)

    else:
        # get surp for each wordpiece and write it to a new row
        for i, (token, idx) in enumerate(zip(tokens, ids[0])):
            row = stim_row.copy() # new object, not a reference
            row["token"] = token
            row["word"] = clean(token)
            row["word_pos"] = i
            row["surprisal"] = -1 if i == 0 else -logprobs[i-1][idx].item()
            out.append(row)

with open(args.output, "w") as out_f:
    writer = csv.DictWriter(out_f, fieldnames = out[0].keys())
    writer.writeheader()
    writer.writerows(out)

