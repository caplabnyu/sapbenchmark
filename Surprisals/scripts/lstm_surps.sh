python preprocess_csv.py

python get_lstm.py --input data/items_ClassicGP.pivot.csv --out data/lstm/items_ClassicGP.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/lstm/models/gulordava_pytorch2.0.pt  --aligned 
python postprocess_csv.py --input data/lstm/items_ClassicGP.lstm.csv --output data/lstm/items_ClassicGP.lstm.post.csv

python get_lstm.py --input data/items_Agreement.pivot.csv --out data/lstm/items_Agreement.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/lstm/models/gulordava_pytorch2.0.pt  --aligned 

python get_lstm.py --input data/items_AttachmentAmbiguity.pivot.csv --out data/lstm/items_AttachmentAmbiguity.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/lstm/models/gulordava_pytorch2.0.pt  --aligned 


python get_lstm.py --input data/items_RelativeClause.pivot.csv --out data/lstm/items_RelativeClause.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/lstm/models/gulordava_pytorch2.0.pt  --aligned 


python get_lstm.py --input data/items_filler.pivot.csv --out data/lstm/items_filler.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/lstm/models/gulordava_pytorch2.0.pt  --aligned 


python get_lstm.py --input data/items_plaus.pivot.csv --out data/lstm/items_plaus.lstm.csv --vocab_path colorlessgreenRNNs/data/lm/English --model ../../models_pt/lstm/models/gulordava_pytorch2.0.pt  --aligned 


python rescale.py --path data/lstm/ --freqs analysis/freqs_coca.csv
