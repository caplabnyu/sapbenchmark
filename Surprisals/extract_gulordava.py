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

parser = argparse.ArgumentParser()
parser.add_argument("--seed", type=int, default=1)
parser.add_argument("--model", type=str, required=True)
parser.add_argument("--vocab_path", type=str)
parser.add_argument("--output", type=str, required=True)
parser.add_argument("--batch_size", type=int, default=1)
parser.add_argument("--cuda", action="store_true")
parser.add_argument("--aligned", action="store_true")
parser.add_argument("--uncased", action="store_true")
# TODO option for selecting subword merges to compute

args = parser.parse_args()

# Load models from comma-separated arg
model_fns = args.model.split(",")
models = []
for model_fn in model_fns:
    if args.cuda:
        model_ = torch.load(model_fn)
    else:
        model_ = torch.load(model_fn, map_location=torch.device('cpu'))

    # rebuild for pytorch 1.x
    model = RNNModel(model_.rnn_type, model_.encoder.num_embeddings, 
                     model_.nhid, model_.nhid, model_.nlayers, 0.2, False)
    print(model_.rnn_type, model_.encoder.num_embeddings, model_.nhid, model_.nhid, model_.nlayers, 0.2, False)
    torch.save(model_.state_dict(), args.output)


