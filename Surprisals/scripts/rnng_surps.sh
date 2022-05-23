python preprocess_csv.py

python get_rnng.py --input data/items_ClassicGP.pivot.csv --output data/rnng/items_ClassicGP.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 
python postprocess_csv.py --input data/rnng/items_ClassicGP.rnng.csv --output data/rnng/items_ClassicGP.rnng.post.csv

python get_rnng.py --input data/items_Agreement.pivot.csv --output data/rnng/items_Agreement.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

python get_rnng.py --input data/items_AttachmentAmbiguity.pivot.csv --output data/rnng/items_AttachmentAmbiguity.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

python get_rnng.py --input data/items_RelativeClause.pivot.csv --output data/rnng/items_RelativeClause.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

python get_rnng.py --input data/items_filler.pivot.csv --output data/rnng/items_filler.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

python get_rnng.py --input data/items_plaus.pivot.csv --output data/rnng/items_plaus.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 
