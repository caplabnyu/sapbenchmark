import pandas

main_name = "data/items_main"

main_df = pandas.read_csv(main_name + ".csv")
main_df = main_df.melt(id_vars=["item", "condition", "disambPositionAmb", "disambPositionUnamb"], value_vars=["ambiguous", "unambiguous"], 
             var_name="ambiguity", value_name="Sentence")
main_df["disambPosition_0idx"] = [ambpos-1 if cond == "ambiguous" else unambpos-1 #0 indexing  
                             for (ambpos, unambpos, cond) 
                             in zip(main_df["disambPositionAmb"], main_df["disambPositionUnamb"], main_df["ambiguity"])]

main_df = main_df[main_df["Sentence"] != ""] 

main_df.to_csv(main_name + ".pivot.csv")


other_names = ["data/items_agreement", "data/items_attach", "data/items_orc", "data/items_filler", "data/items_plaus"]

for name in other_names:
     
    df = pandas.read_csv(name + ".csv")
    df = df[df["Sentence"] != ""] 
    if "disambPosition" in df:
        df["disambPosition_0idx"] = df["disambPosition"]
    df.to_csv(name + ".pivot.csv")

   
