import csv
import argparse

parser = argparse.ArgumentParser()

parser.add_argument("--input", type=str)
parser.add_argument("--output", type=str)
parser.add_argument("--spillover", type=int, default=2)

args = parser.parse_args()

# filter out non-critical/spillover words.
out = []
with open(args.input, "r") as in_f:
    reader = csv.DictReader(in_f)
    for row in reader:
        if (int(row["word_pos"]) - int(row["disambPosition_0idx"])) in list(range(args.spillover + 1)):
            out.append(row)

with open(args.output, "w") as out_f:
    writer = csv.DictWriter(out_f, fieldnames = out[0].keys())
    writer.writeheader()
    writer.writerows(out)
