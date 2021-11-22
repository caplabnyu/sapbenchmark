python preprocess_csv.py
python get_gulordava.py --input data/items_main.pivot.csv --out data/items_main.lstm.csv --data gulordava/colorlessgreenRNNs/data/lm/English --model gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda
python postprocess_csv.py --input data/items_main.lstm.csv --output data/items_main.lstm.post.csv
