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
parser.add_argument("--model_path", type=str)
parser.add_argument("--tokenizer_path", type=str)
parser.add_argument("--return_token_type_ids", action="store_true")
parser.add_argument("--end_of_text_token", default="")

# how can we combine subwords/punctuation to get one surprisal per word?
merge_fs = {"sum_":sum, "mean_": lambda x: sum(x)/len(x)}

args = parser.parse_args()

tokenizer = AutoTokenizer.from_pretrained(args.model_path, add_prefix_space=True)
tokenizer = AutoTokenizer.from_pretrained(args.model_path, add_prefix_space=True)
model = AutoModelForCausalLM.from_pretrained(args.model_path)

if args.cuda:
    device = torch.device("cuda")
    model.to(device)

in_f = open(args.input, "r")
stims = csv.DictReader(in_f)

out = []
for stim_row in stims:
    if args.return_token_type_ids:
        return_token_type_ids = True 
    else:
        return_token_type_ids = False

    sentence = stim_row["Sentence"]

    inputs = tokenizer("<|endoftext|> " + sentence, return_tensors="pt", return_token_type_ids=return_token_type_ids)

    ids = inputs["input_ids"]
    tokens = tokenizer.tokenize("<|endoftext|> " + sentence)

    # run the model
    outputs = model(**inputs, labels=ids)
    logprobs = F.log_softmax(outputs.logits[0], 1)

    
    if args.aligned:
        # print(sentence)
        # print(type(sentence))
        words = sentence.split()
        piecess, breaks = align(words, tokens)
        # get surp for each word (avgd over pieces) and write it to a new row

        assert len(words) == len(piecess)
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

