import csv
import os
import argparse
import numpy as np
import re

parser = argparse.ArgumentParser()
parser.add_argument("--path", type=str)
parser.add_argument("--freqs", type=str)

args = parser.parse_args()

all_factors = {"sum_surprisal":[], "logfreq":[], "length":[]}

freqs_f = open(args.freqs, "r")
freqs = {}
reader = csv.DictReader(freqs_f)
for row in reader:
    freqs[row["word"]] = int(row["count"])

files = os.listdir(args.path)
for file in files:
    if file.split(".")[-1] != "csv": continue
    print(file)
    with open(args.path + file, "r") as surp_f:
        reader = csv.DictReader(surp_f)
        for row in reader:
            all_factors["sum_surprisal"].append(float(row["sum_surprisal"]))
            freq = freqs.get(re.sub("[.,?!;:]", "", row["word"].lower()), 0)
            if freq > 0: all_factors["logfreq"].append(np.log(freq))
            length = len(row["word"])
            all_factors["length"].append(length)

means = {}

for key in all_factors:
    means[key] = (np.mean(all_factors[key]), np.std(all_factors[key]))

for file in files:
    if file.split(".")[-1] != "csv": continue

    out = []
    with open(args.path + file, "r") as surp_f:
        reader = csv.DictReader(surp_f)
        for row in reader:
            row["sum_surprisal_s"] = (float(row["sum_surprisal"]) - means["sum_surprisal"][0])/means["sum_surprisal"][1]
            freq = freqs.get(re.sub("[.,?!;:]", "", row["word"].lower()), 0)
            row["logfreq"] = np.log(freq) if freq > 0 else np.nan
            row["logfreq_s"] = (row["logfreq"] - means["logfreq"][0])/means["logfreq"][1]
            length = len(row["word"])
            row["length"] = length
            row["length_s"] = (row["length"] - means["length"][0])/means["length"][1]

            out.append(row)

    with open(args.path + file + ".scaled", "w") as surp_f:
        writer = csv.DictWriter(surp_f, fieldnames = out[0].keys())
        writer.writeheader()
        writer.writerows(out)

