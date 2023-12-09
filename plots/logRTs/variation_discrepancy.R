sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_lstm)/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="LSTM",]$mean
#0.16
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_lstm)/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="LSTM",]$mean
#0.35
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_lstm)/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="LSTM",]$mean
#0.36
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_lstm)/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="LSTM",]$mean
#-0.82
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_lstm)/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="LSTM",]$mean
#0.96
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_lstm)/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="LSTM",]$mean
#1.57
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_lstm)/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="LSTM",]$mean
#0.35

(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_lstm)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_lstm)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_lstm)^2))^(1/2)
#0.16
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_lstm)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_lstm)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_lstm)^2))^(1/2)
#0.33
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_lstm)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_lstm)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_lstm)^2))^(1/2)
#0.34
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_lstm)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_lstm)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_lstm)^2))^(1/2)
#0.63
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_lstm)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_lstm)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_lstm)^2))^(1/2)
#0.69
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_lstm)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_lstm)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_lstm)^2))^(1/2)
#0.84
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_lstm)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_lstm)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_lstm)^2))^(1/2)
#0.33




sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_gpt2)/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="GPT2",]$mean
#0.60
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_gpt2)/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="GPT2",]$mean
#0.70
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_gpt2)/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="GPT2",]$mean
#0.34
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_gpt2)/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="GPT2",]$mean
#-1.2
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_gpt2)/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="GPT2",]$mean
#0.38
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_gpt2)/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="GPT2",]$mean
#0.9
sd(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_gpt2)/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="GPT2",]$mean
#0.16


(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_gpt2)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_gpt2)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="MV/RRC",]$mean_gpt2)^2))^(1/2)
#0.52
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_gpt2)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_gpt2)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="DO/Sent",]$mean_gpt2)^2))^(1/2)
#0.58
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_gpt2)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_gpt2)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="T/I",]$mean_gpt2)^2))^(1/2)
#0.33
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_gpt2)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_gpt2)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="RC",]$mean_gpt2)^2))^(1/2)
#0.77
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_gpt2)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_gpt2)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="HIGH",]$mean_gpt2)^2))^(1/2)
#0.36
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_gpt2)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_gpt2)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="LOW",]$mean_gpt2)^2))^(1/2)
#0.67
(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_gpt2)/(var(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_gpt2)+mean(by_item[by_item$max_ROI==TRUE&by_item$EOI=="AGREE",]$mean_gpt2)^2))^(1/2)
#0.16





#lower against upper (extremely conservative)  3.4 - 24.7 folds
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="LSTM",]$upper
#24.7
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="GPT2",]$upper
#19.6
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="LSTM",]$upper
#4.7
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="GPT2",]$upper
#4.2
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="LSTM",]$upper
#15.1
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="GPT2",]$upper
#10.2
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="LSTM",]$upper
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="GPT2",]$upper
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="LSTM",]$upper
#4.3
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="GPT2",]$upper
#3.4
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="LSTM",]$upper
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="GPT2",]$upper
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="LSTM",]$upper
#5.2
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="GPT2",]$upper
#3.4



#mean against mean  4.9 - 30.8 folds
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="LSTM",]$mean
#30.8
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="GPT2",]$mean
#28.3
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="LSTM",]$mean
#10.4
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="GPT2",]$mean
#11.7
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="LSTM",]$mean
#23.8
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="GPT2",]$mean
#17.4
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="LSTM",]$mean
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="GPT2",]$mean
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="LSTM",]$mean
#10.3
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="GPT2",]$mean
#7.2
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="LSTM",]$mean
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="GPT2",]$mean
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="LSTM",]$mean
#8.1
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="GPT2",]$mean
#4.9






#lower against upper (extremely conservative)  0.24 - 21.13 folds
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="LSTM",]$upper
#21.13
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="GPT2",]$upper
#12.86
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="LSTM",]$upper
#4.27
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="GPT2",]$upper
#2.58
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="LSTM",]$upper
#6.84
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="GPT2",]$upper
#3.11
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="LSTM",]$upper
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="GPT2",]$upper
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="LSTM",]$upper
#5.23
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="GPT2",]$upper
#2.64
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="LSTM",]$upper
#0.40
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="GPT2",]$upper
#0.24
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="LSTM",]$upper
#4.22
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$lower/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="GPT2",]$upper
#1.97



#mean against mean  2.69 - 27.4 folds
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="LSTM",]$mean
#27.41
by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="MV/RRC"&by_construction$type=="GPT2",]$mean
#20.68
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="LSTM",]$mean
#9.07
by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="DO/Sent"&by_construction$type=="GPT2",]$mean
#7.26
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="LSTM",]$mean
#12.68
by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="T/I"&by_construction$type=="GPT2",]$mean
#6.10
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="LSTM",]$mean
by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==2&by_construction$EOI=="RC"&by_construction$type=="GPT2",]$mean
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="LSTM",]$mean
#10.42
by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="HIGH"&by_construction$type=="GPT2",]$mean
#4.82
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="LSTM",]$mean
#8.41
by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="LOW"&by_construction$type=="GPT2",]$mean
#4.65
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="LSTM",]$mean
#6.06
by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="Empirical",]$mean/by_construction[by_construction$ROI==1&by_construction$EOI=="AGREE"&by_construction$type=="GPT2",]$mean
#2.69


