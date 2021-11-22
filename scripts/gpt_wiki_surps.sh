python preprocess_csv.py
python get_gpt2_full.py --input data/items_main.pivot.csv --output data/items_main.gpt2wiki.csv --tokenizer ../../models/wikitext-103_tokenizer/ --model ../../models/gpt2_40m_12-768-1024_a_02/ --cuda
python postprocess_csv.py --input data/items_main.gpt2wiki.csv --output data/items_main.gpt2.post.csv
