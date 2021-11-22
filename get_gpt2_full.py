from transformers import AutoTokenizer, AutoModelForCausalLM
import torch
import torch.nn.functional as F

import argparse
import csv
import re

parser = argparse.ArgumentParser()
parser.add_argument("--cuda", action="store_true")
parser.add_argument("--input", type=str)
parser.add_argument("--output", type=str)

args = parser.parse_args()

# Load models from HF transformers
tokenizer = AutoTokenizer.from_pretrained("gpt2")
model = AutoModelForCausalLM.from_pretrained("gpt2")

if args.cuda:
    device = torch.device("cuda")
    model.to(device)


in_f = open(args.input, "r")
stims = csv.DictReader(in_f)

out = []
for stim_row in stims:
    inputs = tokenizer(stim_row["sent"], return_tensors="pt")
    ids = inputs["input_ids"]
    tokens = tokenizer.tokenize(stim_row["sent"])

    # run the model
    outputs = model(**inputs, labels=ids)
    logprobs = F.log_softmax(outputs.logits[0], 1)
    
    # get surp for each word and write it to a new row
    for i, (token, idx) in enumerate(zip(tokens, ids[0])):
        row = stim_row.copy() # new object, not a reference
        row["token"] = token
        row["word"] = re.sub("[^a-zA-Z*]", "", token) # filter out the non-initial indicator character gpt-2 uses
        row["word_pos"] = i
        row["surprisal"] = -1 if i == 0 else -logprobs[i-1][idx].item()
        out.append(row)

with open(args.output, "w") as out_f:
    writer = csv.DictWriter(out_f, fieldnames = out[0].keys())
    writer.writeheader()
    writer.writerows(out)

