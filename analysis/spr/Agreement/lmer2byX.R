library(lme4)

lstm_model <- readRDS("./models/agreement_lmodel_prior1_lstm_predicted_agr.rds")


f_unGram <- fixef(lstm_model)['pGram.coded']
f_pos1 <- fixef(lstm_model)['position.coded.1']
f_pos2 <- fixef(lstm_model)['position.coded.2']
  
se_unGram <- coef(summary(lstm_model))["pGram.coded", "Std. Error"]
se_pos1 <- coef(summary(lstm_model))["position.coded.1", "Std. Error"]
se_pos2 <- coef(summary(lstm_model))["position.coded.2", "Std. Error"]

by_construction <- expand.grid(ROI=c(0,1,2), coef=c("Agr_Error")) 
by_construction$mean <- c(f_unGram + (0.5 * f_pos1), f_unGram + (0.5 * f_pos2), f_unGram - (0.5 * (f_pos1 + f_pos2)))
by_construction$lower <- c(f_unGram - (2 * se_unGram) + (0.5 * (f_pos1 - (2 * se_pos1))), 
                           f_unGram - (2 * se_unGram) + (0.5 * (f_pos2 - (2 * se_pos2))), 
                           f_unGram - (2 * se_unGram) - (0.5 * (f_pos1 - (2 * se_pos1) + f_pos2 - (2 * se_pos2))))
by_construction$upper <- c(f_unGram + (2 * se_unGram) + (0.5 * (f_pos1 + (2 * se_pos1))), 
                           f_unGram + (2 * se_unGram) + (0.5 * (f_pos2 + (2 * se_pos2))), 
                           f_unGram + (2 * se_unGram) - (0.5 * (f_pos1 + (2 * se_pos1) + f_pos2 + (2 * se_pos2))))
by_construction$type = "LSTM"
saveRDS(by_construction, "../../../plots/spr/Agreement/by_construction_lmer_lstm.rds")

by_item <- expand.grid(item=1:18, ROI=c(0,1,2), coef=c("Agr_Error")) 

r_unGram <- ranef(lstm_model)[['item']]$pGram.coded
r_pos1 <- ranef(lstm_model)[['item']]$position.coded.1
r_pos2 <- ranef(lstm_model)[['item']]$position.coded.2
pos_1 <- ((f_unGram + r_unGram) + (0.5 * (f_pos1 + r_pos1))) 
pos_2 <- ((f_unGram + r_unGram) + (0.5 * (f_pos2 + r_pos2))) 
pos_3 <- ((f_unGram + r_unGram) - (0.5 * (f_pos2 + r_pos2 + f_pos1 + r_pos1))) 
by_item$mean <- c(pos_1, pos_2, pos_3)
by_item$type = "LSTM"
saveRDS(by_item, "../../../plots/spr/Agreement/by_item_lmer_lstm.rds")

gpt2_model <- readRDS("./models/agreement_lmodel_prior1_gpt2_predicted_agr.rds")

f_unGram <- fixef(gpt2_model)['pGram.coded']
f_pos1 <- fixef(gpt2_model)['position.coded.1']
f_pos2 <- fixef(gpt2_model)['position.coded.2']
se_unGram <- coef(summary(gpt2_model))["pGram.coded", "Std. Error"]
se_pos1 <- coef(summary(gpt2_model))["position.coded.1", "Std. Error"]
se_pos2 <- coef(summary(gpt2_model))["position.coded.2", "Std. Error"]

by_construction <- expand.grid(ROI=c(0,1,2), coef=c("Agr_Error")) 
by_construction$mean <- c(f_unGram + (0.5 * f_pos1), f_unGram + (0.5 * f_pos2), f_unGram - (0.5 * (f_pos1 + f_pos2)))
by_construction$lower <- c(f_unGram - (2 * se_unGram) + (0.5 * (f_pos1 - (2 * se_pos1))), 
                           f_unGram - (2 * se_unGram) + (0.5 * (f_pos2 - (2 * se_pos2))), 
                           f_unGram - (2 * se_unGram) - (0.5 * (f_pos1 - (2 * se_pos1) + f_pos2 - (2 * se_pos2))))
by_construction$upper <- c(f_unGram + (2 * se_unGram) + (0.5 * (f_pos1 + (2 * se_pos1))), 
                           f_unGram + (2 * se_unGram) + (0.5 * (f_pos2 + (2 * se_pos2))), 
                           f_unGram + (2 * se_unGram) - (0.5 * (f_pos1 + (2 * se_pos1) + f_pos2 + (2 * se_pos2))))
by_construction$type = "GPT2"
saveRDS(by_construction, "../../../plots/spr/Agreement/by_construction_lmer_gpt2.rds")

by_item <- expand.grid(item=1:18, ROI=c(0,1,2), coef=c("Agr_Error")) 

r_unGram <- ranef(gpt2_model)[['item']]$pGram.coded
r_pos1 <- ranef(gpt2_model)[['item']]$position.coded.1
r_pos2 <- ranef(gpt2_model)[['item']]$position.coded.2
pos_1 <- ((f_unGram + r_unGram) + (0.5 * (f_pos1 + r_pos1))) 
pos_2 <- ((f_unGram + r_unGram) + (0.5 * (f_pos2 + r_pos2))) 
pos_3 <- ((f_unGram + r_unGram) - (0.5 * (f_pos2 + r_pos2 + f_pos1 + r_pos1))) 
by_item$mean <- c(pos_1, pos_2, pos_3)
by_item$type = "GPT2"
saveRDS(by_item, "../../../plots/spr/Agreement/by_item_lmer_gpt2.rds")
