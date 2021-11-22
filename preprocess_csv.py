import pandas

main_name = "data/items_main"

main_df = pandas.read_csv(main_name + ".csv")
main_df = main_df.melt(id_vars=["item", "condition", "disambPositionAmb", "disambPositionUnamb"], value_vars=["ambiguous", "unambiguous"], 
             var_name="ambiguity", value_name="sent")
main_df["disambPosition_0idx"] = [ambpos-1 if cond == "ambiguous" else unambpos-1 #0 indexing  
                             for (ambpos, unambpos, cond) 
                             in zip(main_df["disambPositionAmb"], main_df["disambPositionUnamb"], main_df["ambiguity"])]

main_df = main_df[main_df["sent"] != ""] 

main_df.to_csv(main_name + ".pivot.csv")

