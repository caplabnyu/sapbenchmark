python preprocess_csv.py
python get_gpt2_full.py --input data/items_main.pivot.csv --output data/items_main.gpt2.csv
python postprocess_csv.py --input data/items_main.gpt2.csv --output data/items_main.gpt2.post.csv
