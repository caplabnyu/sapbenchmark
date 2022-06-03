python preprocess_csv.py

python get_lstm.py --input data/items_ClassicGP.pivot.csv --out data/gulordava/items_ClassicGP.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned
python postprocess_csv.py --input data/gulordava/items_ClassicGP.lstm.csv --output data/gulordava/items_ClassicGP.lstm.post.csv

python get_lstm.py --input data/items_Agreement.pivot.csv --out data/gulordava/items_Agreement.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_AttachmentAmbiguity.pivot.csv --out data/gulordava/items_AttachmentAmbiguity.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_RelativeClause.pivot.csv --out data/gulordava/items_RelativeClause.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_filler.pivot.csv --out data/gulordava/items_filler.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned

python get_lstm.py --input data/items_plaus.pivot.csv --out data/gulordava/items_plaus.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/gulordava/models/hidden650_batch128_dropout0.2_lr20.0.pt --cuda --aligned