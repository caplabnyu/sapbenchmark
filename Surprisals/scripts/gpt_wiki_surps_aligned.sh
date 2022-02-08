python preprocess_csv.py
python get_gpt2_wiki.py --input data/items_main.pivot.csv --output data/items_main.gpt2wiki.aligned.csv --tokenizer ./gpt2-wiki/models/wikitext-103_tokenizer/ --model ./gpt2-wiki/models/gpt2_40m_12-768-1024_a_02/checkpoint-31000 --aligned
python postprocess_csv.py --input data/items_main.gpt2wiki.aligned.csv --output data/items_main.gpt2wiki.aligned.post.csv
