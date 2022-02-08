python preprocess_csv.py

python get_lstm.py --input data/items_main.pivot.csv --out data/gulordava/items_main.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned
python postprocess_csv.py --input data/gulordava/items_main.lstm.csv --output data/gulordava/items_main.lstm.post.csv

python get_lstm.py --input data/items_agreement.pivot.csv --out data/gulordava/items_agreement.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_attach.pivot.csv --out data/gulordava/items_attach.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_orc.pivot.csv --out data/gulordava/items_orc.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_filler.pivot.csv --out data/gulordava/items_filler.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_plaus.pivot.csv --out data/gulordava/items_plaus.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned
