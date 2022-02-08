import torch
import torch.nn as nn
import torch.nn.functional as F

import numpy as np

import argparse

import sys
sys.path.insert(0, "./rnng-pytorch/")
import preprocess 
import beam_search
import pandas as pd
import csv

from util import align

from nltk.tokenize import TreebankWordTokenizer

parser = argparse.ArgumentParser()
parser.add_argument("--seed", type=int, default=1)
parser.add_argument("--model", type=str, required=True)
parser.add_argument("--vocab_path", type=str)
parser.add_argument("--input", type=str, required=True)
parser.add_argument("--output", type=str, required=True)
parser.add_argument("--batch_size", type=int, default=1)
parser.add_argument("--cuda", action="store_true")
parser.add_argument("--aligned", action="store_true")
parser.add_argument("--uncased", action="store_true")
# TODO option for selecting subword merges to compute

args = parser.parse_args()

# how can we combine subwords/punctuation to get one surprisal per word?
merge_fs = {"sum":sum, "mean": lambda x: sum(x)/len(x)}


# Load experimental data csv
in_f = open(args.input, "r")
inp = csv.DictReader(in_f)

# Define intermediate files
sent_list_fn = args.input.split(".")[0] + ".txt"
surp_fn = args.input.split(".")[0] + ".surps.tab"



# extract sents to rnng input files
sents = [" ".join(TreebankWordTokenizer().tokenize(row["Sentence"].replace("\\","")))
         for row in inp]
sents_str = "\n".join(sents)
with open(sent_list_fn, "w") as out_f:
    out_f.write(sents_str)

# Manually run their beam_search script
bs_args = argparse.Namespace()

## important params
bs_args.test_file = sent_list_fn
bs_args.model_file = args.model
bs_args.lm_output_file = surp_fn
bs_args.batch_size = args.batch_size
bs_args.beam_size = 100
bs_args.word_beam_size = 10
bs_args.shift_size = 1
bs_args.block_size = 1000
bs_args.gpu = 0
## replicate defaults
bs_args.batch_token_size = 300
bs_args.stack_size_bound = -1
bs_args.device = "cuda"
bs_args.seed = args.seed
bs_args.max_length_diff = 20
bs_args.fp16 = False
bs_args.dump_beam = False
bs_args.delay_word_ll = False
bs_args.particle_filter = False

beam_search.main(bs_args)

in_f = open(args.input, "r")
inp = csv.DictReader(in_f)
orig_lines = [row for row in inp]
out = []
with open(surp_fn, "r") as surp_f:
    surp_bysent = {}
    for row in surp_f:
        try:
            sent_i, t_i, orig_t, mod_t, s, _ = row.split("\t")
            surp_bysent[int(sent_i)] = surp_bysent.get(int(sent_i), []) + [{"sent_num":int(sent_i),
                                                              "token":mod_t,
                                                              "word":orig_t,
                                                              "word_pos":int(t_i),
                                                              "surprisal":float(s)}]
        except:
            break

    for sent_num, rnng_rows in surp_bysent.items():
        tokens = [row["token"] for row in rnng_rows]
        words =  orig_lines[sent_num]["Sentence"].split()
        piecess, breaks = align(words, tokens)
        for i, (word, pieces) in enumerate(zip(words, piecess)):
            row = orig_lines[sent_num].copy() 
            row["token"] = ".".join(pieces)
            row["word"] = word
            row["word_pos"] = i
            surps = [rnng_rows[j]["surprisal"] for j in range(breaks[i], breaks[i+1])]
            for merge_fn, merge_f in merge_fs.items():
                row[merge_fn + "surprisal"] = merge_f(surps)
            out.append(row)


with open(args.output, "w") as out_f:
    writer = csv.DictWriter(out_f, fieldnames = out[0].keys())
    writer.writeheader()
    writer.writerows(out)
    



