python preprocess_csv.py

python get_gpt2_full.py --input data/items_main.pivot.csv --output data/gpt2/items_main.gpt2.csv --aligned
python postprocess_csv.py --input data/gpt2/items_main.gpt2.csv --output data/gpt2/items_main.gpt2.post.csv

python get_gpt2_full.py --input data/items_agreement.pivot.csv --output data/gpt2/items_agreement.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_attach.pivot.csv --output data/gpt2/items_attach.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_orc.pivot.csv --output data/gpt2/items_orc.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_filler.pivot.csv --output data/gpt2/items_filler.gpt2.csv --aligned

python get_gpt2_full.py --input data/items_plaus.pivot.csv --output data/gpt2/items_plaus.gpt2.csv --aligned
