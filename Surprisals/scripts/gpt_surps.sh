python preprocess_csv.py

python get_gpt2_full.py --input data/items_ClassicGP.pivot.csv --output data/gpt2/items_ClassicGP.gpt2.csv --aligned
python postprocess_csv.py --input data/gpt2/items_ClassicGP.gpt2.csv --output data/gpt2/items_ClassicGP.gpt2.post.csv

python get_gpt2_full.py --input data/items_Agreement.pivot.csv --output data/gpt2/items_Agreement.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_AttachmentAmbiguity.pivot.csv --output data/gpt2/items_AttachmentAmbiguity.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_RelativeClause.pivot.csv --output data/gpt2/items_RelativeClause.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_filler.pivot.csv --output data/gpt2/items_filler.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_plaus.pivot.csv --output data/gpt2/items_plaus.gpt2.csv --aligned

python rescale.py --path data/gpt2/ --freqs analysis/freqs_coca.csv

