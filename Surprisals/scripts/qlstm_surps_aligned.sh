python preprocess_csv.py
for FILE in ./QuantityLSTMs/*.pt; do 
    echo $(basename $FILE .pt);
    echo "main"
    python get_lstm.py --input data/items_main.pivot.csv --out data/quantity/items_main.$(basename $FILE .pt).aligned.csv --vocab_path QuantityLSTMs/ --model $FILE --cuda --aligned --uncased;
    python postprocess_csv.py --input data/quantity/items_main.$(basename $FILE .pt).aligned.csv --output data/quantity/items_main.$(basename $FILE .pt).aligned.post.csv;

    echo "agreement"
    python get_lstm.py --input data/items_agreement.pivot.csv --out data/quantity/items_agreement.$(basename $FILE .pt).aligned.csv --vocab_path QuantityLSTMs/ --model $FILE --cuda --aligned --uncased;
    python postprocess_csv.py --input data/quantity/items_agreement.$(basename $FILE .pt).aligned.csv --output data/quantity/items_agreement.$(basename $FILE .pt).aligned.post.csv;

    echo "attachment"
    python get_lstm.py --input data/items_attach.pivot.csv --out data/quantity/items_attach.$(basename $FILE .pt).aligned.csv --vocab_path QuantityLSTMs/ --model $FILE --cuda --aligned --uncased;
    python postprocess_csv.py --input data/quantity/items_attach.$(basename $FILE .pt).aligned.csv --output data/quantity/items_attach.$(basename $FILE .pt).aligned.post.csv;

    echo "src/orc"
    python get_lstm.py --input data/items_orc.pivot.csv --out data/quantity/items_orc.$(basename $FILE .pt).aligned.csv --vocab_path QuantityLSTMs/ --model $FILE --cuda --aligned --uncased;

    echo "filler"
    python get_lstm.py --input data/items_filler.pivot.csv --out data/quantity/items_filler.$(basename $FILE .pt).aligned.csv --vocab_path QuantityLSTMs/ --model $FILE --cuda --aligned --uncased;
done;
