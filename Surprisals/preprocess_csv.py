import pandas

# Filenames to preprocess
# TODO: Maybe automate gathering this, or put this in a shared config accessable by the scripts?
main_name = "data/items_ClassicGP"
other_names = ["data/items_Agreement", "data/items_AttachmentAmbiguity", "data/items_RelativeClause", "data/items_filler", "data/items_plaus"]

def clean_sent(s):
    """ Strip the sentences in the datafile of any weird escape characters so they match what participants saw """
    if type(s) == str: return s.replace('\\', '')
    return ""

# Special processing for classic subset, since the ambig/unambig stimuli are in the same row
main_df = pandas.read_csv(main_name + ".csv")
main_df = main_df.melt(id_vars=["item", "condition", "disambPositionAmb", "disambPositionUnamb"], value_vars=["ambiguous", "unambiguous"], 
             var_name="ambiguity", value_name="Sentence")
main_df["disambPosition_0idx"] = [ambpos-1 if cond == "ambiguous" else unambpos-1 #0 indexing  
                             for (ambpos, unambpos, cond) 
                             in zip(main_df["disambPositionAmb"], main_df["disambPositionUnamb"], main_df["ambiguity"])]

main_df = main_df[main_df["Sentence"] != ""] 
main_df["Sentence"] = main_df["Sentence"].apply(clean_sent)

main_df.to_csv(main_name + ".pivot.csv")

# For others, filter out empty rows and get a 0idx'd version of disambPosition if it exists for that dataset

for name in other_names:
    df = pandas.read_csv(name + ".csv")
    df = df[df["Sentence"] != ""] 
    df["Sentence"] = df["Sentence"].apply(clean_sent)
    if "disambPosition" in df:
        df["disambPosition_0idx"] = df["disambPosition"]
    df.to_csv(name + ".pivot.csv")

   
