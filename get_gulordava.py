import torch
import torch.nn as nn
import torch.nn.functional as F

import numpy as np

import argparse

import sys
sys.path.insert(0, "./gulordava/colorlessgreenRNNs/src/language_models")
from dictionary_corpus import Dictionary
from model import RNNModel
import pandas as pd
import csv

def indexify(word):
    """ Convert word to an index into the embedding matrix """
    if word not in dictionary.word2idx:
        print("Warning: {} not in vocab".format(word))
    return dictionary.word2idx[word] if word in dictionary.word2idx else dictionary.word2idx["<unk>"]

def tokenize(sent):
    # respect commas as a token
    sent = " ,".join(sent.split(","))

    # split on 's
    sent = " '".join(sent.split("\\'"))

    # do not respect EOS . to follow gpt2 tokenizer
    sent = sent.replace(".", "")
    return sent.split()

parser = argparse.ArgumentParser()
parser.add_argument("--seed", type=int, default=1)
parser.add_argument("--model", type=str, required=True)
parser.add_argument("--data", type=str, required=True)
parser.add_argument("--input", type=str, required=True)
parser.add_argument("--output", type=str, required=True)
parser.add_argument("--batch_size", type=int, default=1)
parser.add_argument("--cuda", action="store_true")

args = parser.parse_args()

# Make it reproduceable
torch.manual_seed(args.seed)
torch.backends.cudnn.deterministic = True
torch.backends.cudnn.benchmark = False
np.random.seed(args.seed)

# Load models from comma-separated arg
model_fns = args.model.split(",")
models = []
for model_fn in model_fns:
    model_ = torch.load(model_fn)

    # rebuild for pytorch 1.x
    model = RNNModel("LSTM", 50001, 650, 650, 2, 0.2, False)
    model.load_state_dict(model_.state_dict())

    if args.cuda:
        model = model.cuda()
    else:
        model = model.cpu()

    model.eval()
    models.append(model)

# Load vocab
dictionary = Dictionary(args.data)

# Load experimental data csv
in_f = open(args.input, "r")
inp = csv.DictReader(in_f)

out_rows = []
for model_num, model in enumerate(models):
    with torch.no_grad():
        for row in inp:
            sentence = tokenize(row["sent"])
            input = torch.LongTensor([indexify(w) for w in sentence])
            
            if args.cuda:
                input = input.cuda()

            out, _ = model(input.view(-1, 1), model.init_hidden(1))

            for i, (word_idx, word) in enumerate(zip(input, sentence)):
                new_row = row.copy() # new object, not a reference to the iterator
                new_row["surprisal"] = -1 if i == 0 else -F.log_softmax(out[i-1], dim=-1).view(-1)[word_idx].item()
                new_row["word"] = word
                new_row["word_pos"] = i 
                out_rows.append(new_row)

# write out to csv
with open(args.output, "w") as out_f:
    writer = csv.DictWriter(out_f, fieldnames = out_rows[0].keys())
    writer.writeheader()
    writer.writerows(out_rows)
