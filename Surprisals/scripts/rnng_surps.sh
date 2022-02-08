python preprocess_csv.py

optirun python get_rnng.py --input data/items_main.pivot.csv --output data/rnng/items_main.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 
python postprocess_csv.py --input data/rnng/items_main.rnng.csv --output data/rnng/items_main.rnng.post.csv

optirun python get_rnng.py --input data/items_agreement.pivot.csv --output data/rnng/items_agreement.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

optirun python get_rnng.py --input data/items_attach.pivot.csv --output data/rnng/items_attach.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

optirun python get_rnng.py --input data/items_orc.pivot.csv --output data/rnng/items_orc.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

optirun python get_rnng.py --input data/items_filler.pivot.csv --output data/rnng/items_filler.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 

optirun python get_rnng.py --input data/items_plaus.pivot.csv --output data/rnng/items_plaus.rnng.csv --model ../../models_pt/rnng/rnng-e80.pt --seed 0 
