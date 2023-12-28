import torch
import torch.nn as nn
import torch.nn.functional as F

import numpy as np

import argparse

import sys
sys.path.insert(0, "./colorlessgreenRNNs/src/language_models")
from dictionary_corpus import Dictionary
from model import RNNModel
import pandas as pd
import csv

from util import align

# Supress warnings since we fix the model loading anyway
import warnings
from torch.serialization import SourceChangeWarning
warnings.filterwarnings("ignore", category=SourceChangeWarning)

def indexify(word):
    """ Convert word to an index into the embedding matrix """
    if word not in dictionary.word2idx:
        print("Warning: {} not in vocab".format(word))
    return dictionary.word2idx[word] if word in dictionary.word2idx else dictionary.word2idx["<unk>"]

def tokenize(sent):
    sent = sent.strip()
    if sent == "": return []

    # respect commas as a token
    sent = " ,".join(sent.split(","))

    # same w/ EOS punctuation (but not . in abbreviations)
    if sent[-1] in  [".", "?", "!"]:
        sent = sent[:-1] + " " + sent[-1]

    if ("." in sent) & (sent[-1] != "."):
        print(sent)

    # split on 's
    sent = " 's".join(sent.split("'s"))

    # split on n't
    sent = " n't".join(sent.split("n't"))

    return sent.split()

parser = argparse.ArgumentParser()
parser.add_argument("--seed", type=int, default=1)
parser.add_argument("--model", type=str, required=True)
parser.add_argument("--vocab_path", type=str)
parser.add_argument("--input", type=str, required=True)
parser.add_argument("--output", type=str, required=True)
parser.add_argument("--batch_size", type=int, default=1)
parser.add_argument("--aligned", action="store_true")
parser.add_argument("--uncased", action="store_true")
# TODO option for selecting subword merges to compute

args = parser.parse_args()

# how can we combine subwords/punctuation to get one surprisal per word?
merge_fs = {"sum_":sum, "mean_": lambda x: sum(x)/len(x)}


# Make it reproduceable
torch.manual_seed(args.seed)
torch.backends.cudnn.deterministic = True
torch.backends.cudnn.benchmark = False
np.random.seed(args.seed)

# Select device
device = torch.device('cpu')
if torch.cuda.is_available():
    device = torch.device('cuda')
elif torch.backends.mps.is_available() and torch.backends.mps.is_built():
    device = torch.device('mps')

# Load models from comma-separated arg
model_fns = args.model.split(",")
models = []
for model_fn in model_fns:

    model = RNNModel("LSTM", 50001, 
                     650, 650, 2, 0.2, False)
    print(model.rnn_type, model.encoder.num_embeddings, model.nhid, 
          model.nhid, model.nlayers, 0.2, False)

    # the model files are now the state_dicts, not the model itself!
    # see extract_gulordava.py to convert the released model .pt to 
    # a state dict (must be done in pytorch < 1.4.0 bc of LSTM changes!)
    state_dict = torch.load(model_fn, map_location=device) 
    model.load_state_dict(state_dict)

    model = model.to(device)

    model.eval()
    models.append(model)

# Load vocab
dictionary = Dictionary(args.vocab_path)


# Load experimental data csv
in_f = open(args.input, "r")
inp = csv.DictReader(in_f)

out_rows = []
with torch.no_grad():
    for row in inp:
        sentence = ["<eos>"] + tokenize(row["Sentence"]) # EOS prepend
        input = torch.LongTensor([indexify(w.lower() if args.uncased else w) for w in sentence])
        
        input = input.to(device)
        

        out, _ = model(input.view(-1, 1), model.init_hidden(1))

        if args.aligned:
            words = row["Sentence"].split() 
            piecess, breaks = align(words, sentence[1:]) # drop EOS in sentence
            for i, (word, pieces) in enumerate(zip(words, piecess)):
                new_row = row.copy() # new object, not a reference to the iterator
                for j, model in enumerate(models):
                    tag = "_m{}".format(j) if len(models) > 1 else ""

                    # Note that since the beginning-of-sentence <eos> is in out/input, but was dropped from breaks, we need to
                    # correct for misalignment (thus out[k] rather than out[k-1], input[k+1] instead of input[k]).
                    surps = [-F.log_softmax(out[k], dim=-1).view(-1)[input[k+1]].item() 
                             for k in range(breaks[i], breaks[i+1])]
                    surps_b2 = [surp/np.log(2.0) for surp in surps]
                    for merge_fn, merge_f in merge_fs.items():
                        new_row[merge_fn +  "surprisal" + tag] = merge_f(surps)
                        
                        # Surprisal in bits from base-change formula
                        new_row[merge_fn +  "surprisal_base2" + tag] = merge_f(surps_b2)
                new_row["token"] = ".".join([w if w in dictionary.word2idx 
                                             else "<UNK>" for w in pieces])
                new_row["word"] = word
                new_row["word_pos"] = i 
                out_rows.append(new_row)
                
        else:
            for i, (word_idx, word) in enumerate(zip(input, sentence[1:])): # drop EOS
                new_row = row.copy() # new object, not a reference to the iterator
                for j, model in enumerate(models):
                    tag = "_m{}".format(j) if len(models) > 1 else ""
                    new_row["surprisal" + tag] = -F.log_softmax(out[i], dim=-1).view(-1)[word_idx].item()
                    new_row["surprisal_b2" + tag] = -F.log_softmax(out[i], dim=-1).view(-1)[word_idx].item()/np.log(2.0)
                new_row["token"] = word if word in dictionary.w2idx else "<UNK>"
                new_row["word"] = word
                new_row["word_pos"] = i 
                out_rows.append(new_row)

# write out to csv
with open(args.output, "w") as out_f:
    writer = csv.DictWriter(out_f, fieldnames = out_rows[0].keys())
    writer.writeheader()
    writer.writerows(out_rows)
