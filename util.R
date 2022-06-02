library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(posterior)
library(tidybayes)
library(tidyverse)

reshape_item_dat <- function(fit, rand_name){
  
   samples <- as_draws_df(fit)  %>%
     gather_variables() 
  
  fixef_samples <- samples %>%
    filter(str_detect(.variable,'b_')) %>%
    spread(.variable, .value)
  
  rand_regex <- paste('r_', rand_name, '\\[', sep='')
  
  post_samples <- samples %>%
    filter(str_detect(.variable, rand_regex))  %>%
    mutate(.variable = str_replace(.variable, rand_regex, ''))  %>%
    separate(.variable, c('item', 'coef'), sep= ',') %>%
    mutate(coef = str_replace(coef, '\\]', ''),
           coef = paste('r_', coef, sep='')) %>%
    spread(coef, .value) %>%
    merge(fixef_samples, by = c('.chain', '.draw', '.iteration'))
  
  return(post_samples)
}
###
Plot_empirical_construction_level <- function(fixedeffcts_df,subset_name, axistitle.size=14, axistext.size=14,legendtitle.size=14,legendtext.size=14,ROIcolor=c("royalblue3","tan2","forestgreen")){
  if(subset_name=="RelativeClause"){
    ggplot(data=fixedeffcts_df, aes(x=ROI, y=mean, fill=ROI)) +
      geom_bar(stat="identity",position=position_dodge())+
      scale_fill_manual(values = ROIcolor)+
      #scale_fill_discrete(labels=c("Verb","Det","Noun"))+
      geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
      xlab("Human_RelativeClause Subset")+
      ylab("Mean Relative Clause Effect")+
      theme(axis.title=element_text(size=axistitle.size,face="bold"),
      axis.text = element_text(size=axistext.size,face="bold"),
      legend.title = element_text(size=legendtitle.size),
      legend.text = element_text(size=legendtext.size))
  }else{
  if(subset_name=="AttachmentAmbiguity"){
    ggplot(data=fixedeffcts_df, aes(x=coef, y=mean, fill=ROI)) +
      geom_bar(stat="identity",position=position_dodge())+
      scale_fill_manual(values = ROIcolor)+
      geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
      xlab("Human_AttachmentAmbiguity Subset")+
      ylab("Attachment Site Effect")+
      theme(axis.title=element_text(size=axistitle.size,face="bold"),
            axis.text = element_text(size=axistext.size,face="bold"),
            legend.title = element_text(size=legendtitle.size),
            legend.text = element_text(size=legendtext.size))
  }else{
    if(subset_name=="Agreement"){
      ggplot(data=fixedeffcts_df, aes(x=coef, y=mean, fill=ROI)) +
        geom_bar(stat="identity",position=position_dodge())+
        scale_fill_manual(values = ROIcolor)+
        geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
        xlab("Human_Agreement Subset")+
        ylab("Perceived Ungrammaticality Effect")+
        theme(axis.title=element_text(size=axistitle.size,face="bold"),
              axis.text = element_text(size=axistext.size,face="bold"),
              legend.title = element_text(size=legendtitle.size),
              legend.text = element_text(size=legendtext.size))
    }else{
      if(subset_name=="ClassicGP"){
      ggplot(data=fixedeffcts_df, aes(x=coef, y=mean, fill=ROI)) +
        geom_bar(stat="identity",position=position_dodge())+
        scale_fill_manual(values = ROIcolor)+
        geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
        xlab("Human_ClassicGardenPath Subset")+
        ylab("Mean Garden Path Effect")+
        theme(axis.title=element_text(size=axistitle.size,face="bold"),
              axis.text = element_text(size=axistext.size,face="bold"),
              legend.title = element_text(size=legendtitle.size),
              legend.text = element_text(size=legendtext.size))
      }else{
          print("subset_name can only be 'ClassicGP', 'RelativeClause' ,'Agreement', or 'AttachmentAmbiguity'")
        }
      }
    }
  }
}
###
Plot_itemwise_by_magnitude <- function(byitem_df,subset_name,ROI_index,axistitle.size=14, axistext.size=14,facetlabel.size=14){
  byitem_df <- byitem_df%>%group_by(ROI,coef)%>%mutate(RANK=rank(mean))%>%ungroup()
  if(subset_name=="ClassicGP"){
  ggplot(byitem_df[byitem_df$ROI==ROI_index,],aes(x=RANK,y=mean))+
    facet_wrap(~coef)+
    geom_point()+
    geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
    ggtitle(paste0('ROI=',ROI_index))+
    xlab("Magnitude Rank")+
    ylab("Item-wise Garden Path Effects")+
    theme(axis.title=element_text(size=axistitle.size,face="bold"),
          axis.text = element_text(size=axistext.size),
        strip.text.x=element_text(size=facetlabel.size,face="bold"))
  }else{
    if(subset_name=="RelativeClause"){
      #ROI_class <- ifelse(ROI_index==0,"Verb",ifelse(ROI_index==1,"Det","Noun"))
      ggplot(byitem_df[byitem_df$ROI==ROI_index,],aes(x=RANK,y=mean))+
        facet_wrap(~coef)+
        geom_point()+
        geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
        ggtitle(paste0('ROI=',ROI_class))+
        xlab("Magnitude Rank")+
        ylab("Item-wise Relative Clause Effects")+
        theme(axis.title=element_text(size=axistitle.size,face="bold"),
              axis.text = element_text(size=axistext.size),
              strip.text.x=element_text(size=facetlabel.size,face="bold"))
    }else{
      if(subset_name=="AttachmentAmbiguity"){
        ggplot(byitem_df[byitem_df$ROI==ROI_index,],aes(x=RANK,y=mean))+
          facet_wrap(~coef)+
          geom_point()+
          geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
          ggtitle(paste0('ROI=',ROI_index))+
          xlab("Magnitude Rank")+
          ylab("Item-wise Attachment Site Effects")+
          theme(axis.title=element_text(size=axistitle.size,face="bold"),
                axis.text = element_text(size=axistext.size),
                strip.text.x=element_text(size=facetlabel.size,face="bold"))
      }else{
        if(subset_name=="Agreement"){
          ggplot(byitem_df[byitem_df$ROI==ROI_index,],aes(x=RANK,y=mean))+
            facet_wrap(~coef)+
            geom_point()+
            geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
            ggtitle(paste0('ROI=',ROI_index))+
            xlab("Magnitude Rank")+
            ylab("Item-wise Perceived Ungrammaticality Effects")+
            theme(axis.title=element_text(size=axistitle.size,face="bold"),
                  axis.text = element_text(size=axistext.size),
                  strip.text.x=element_text(size=facetlabel.size,face="bold"))
        }else{
          print("subset_name can only be 'ClassicGP', 'RelativeClause' ,'Agreement', or 'AttachmentAmbiguity'")
        }
      }
    }
  }
}
###
merge_surprisal <- function(rt.data_df, byitem_df, subsetname){
  surp_files <- c(paste0('./Surprisals/data/gulordava/items_',subsetname,'.lstm.csv'),
                  paste0('./Surprisals/data/gpt2/items_',subsetname,'.gpt2.csv'),
                  paste0('./Surprisals/data/rnng/items_',subsetname,'.rnng.csv'))
  surp_list <- list()
  i <- 1
  for(fname in surp_files){
    model_name <- strsplit(fname, '.', fixed=TRUE)[[1]][2]
    curr_surp <- read.csv(fname) %>%
    mutate(model = model_name,
           #surprisal = ifelse(surprisal == -1, NA, surprisal),
           word_pos = word_pos + 1)
  
    surp_list[[i]] <- curr_surp
    i <- i +1
  }
  surps_subset <- dplyr::bind_rows(surp_list)
  
  nrow(rt.data_df)
  rt.data.merged <- merge(x=rt.data_df[rt.data_df$ROI%in%c(0,1,2),], y=surps_subset, 
                          by.x=c("Sentence", "WordPosition"), by.y=c("Sentence", "word_pos"))
  nrow(rt.data.merged)
  
  surp.diff <- rt.data.merged %>%
    select(item.x, Type, sum_surprisal, ROI, participant, model) %>%
    group_by(item.x, Type, ROI, model) %>%
    summarize(sum_surprisal = mean(sum_surprisal))%>%
    ungroup()%>%arrange(model,ROI)
  if(subsetname=="ClassicGP"){
  surp.diff <- data.frame(item=surp.diff$item.x[seq(1,nrow(surp.diff),by=2)],coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),(nrow(surp.diff)/2/length(unique(byitem_df$coef)))),ROI=surp.diff$ROI[seq(1,nrow(surp.diff),by=2)],model=surp.diff$model[seq(1,nrow(surp.diff),by=2)],surprisal_diff=surp.diff$sum_surprisal[seq(1,nrow(surp.diff),by=2)]-surp.diff$sum_surprisal[seq(2,nrow(surp.diff),by=2)])
  }else{
    if(subsetname=="RelativeClause"){
      surp.diff <- data.frame(item=surp.diff$item.x[seq(1,nrow(surp.diff),by=2)],coef=rep(c("RC"),(nrow(surp.diff)/2/length(unique(byitem_df$coef)))),ROI=surp.diff$ROI[seq(1,nrow(surp.diff),by=2)],model=surp.diff$model[seq(1,nrow(surp.diff),by=2)],surprisal_diff=surp.diff$sum_surprisal[seq(1,nrow(surp.diff),by=2)]-surp.diff$sum_surprisal[seq(2,nrow(surp.diff),by=2)])    
    }else{
      if(subsetname=="AttachmentAmbiguity"){
        surp.diff <- data.frame(item=rep(surp.diff$item.x[seq(1,nrow(surp.diff),by=3)],2),coef=rep(c("High_Cost","Low_Cost"),each=(nrow(surp.diff)/length(unique(surp.diff$Type)))),ROI=rep(surp.diff$ROI[seq(1,nrow(surp.diff),by=3)],2),model=rep(surp.diff$model[seq(1,nrow(surp.diff),by=3)],2),surprisal_diff=c(surp.diff$sum_surprisal[seq(1,nrow(surp.diff),by=3)]-surp.diff$sum_surprisal[seq(3,nrow(surp.diff),by=3)],surp.diff$sum_surprisal[seq(2,nrow(surp.diff),by=3)]-surp.diff$sum_surprisal[seq(3,nrow(surp.diff),by=3)]))    
      }
    }
  }
  byitem_surprisalmerged <- merge(byitem_df, surp.diff, by = c('ROI', 'item','coef'))
  return(byitem_surprisalmerged)
}
###
Plot_humanresults_surprisaldiff_correlation <- function(merged_df,ROI_index,axistitle.size=14, axistext.size=12,facetlabel.size=10){
  dat_text <- data.frame(coef=rep(unique(merged_df$coef),length(unique(merged_df$model))),
                         model = rep(unique(merged_df$model),each=length(unique(merged_df$coef))))
  for(i in 1:nrow(dat_text)){
    dat_text$corr[i] <- round(cor.test(merged_df$mean[merged_df$ROI==ROI_index&merged_df$coef==dat_text[i,'coef']&merged_df$model==dat_text[i,'model']],
                                 merged_df$surprisal_diff[merged_df$ROI==ROI_index&merged_df$coef==dat_text[i,'coef']&merged_df$model==dat_text[i,'model']])$estimate,2)
  }
  dat_text$label <- sprintf(
    "corr = %s",
    dat_text$corr
  )
ggplot(merged_df[merged_df$ROI==ROI_index,],aes(x=surprisal_diff,y=mean))+
  geom_point()+
  facet_grid(model~coef)+
  xlab(paste0("Surprisal difference (ROI=",ROI_index,")"))+
  ylab(paste0("Structural effects (ROI=",ROI_index,")"))+
  theme(axis.title=element_text(size=axistitle.size,face="bold"),
        axis.text.x = element_text(size=axistext.size),
        axis.text.y = element_text(size=axistext.size),
        strip.text = element_text(size=facetlabel.size,face="bold"))+
  theme(aspect.ratio = 1)+
  geom_text(
  size    = 3.5,
  data    = dat_text,
  mapping = aes(x = Inf, y = Inf, label = label),
  hjust   = 2,
  vjust   = 1.5
  )
}

Predicting_RT_with_spillover <- function(rt.data_df,subsetname){
  print("This will take a while.")
  freqs <- read.csv("freqs.csv")
  surps.lstm <- read.csv(paste0('./Surprisals/data/gulordava/items_',subsetname,'.lstm.csv'))
  surps.gpt2 <- read.csv(paste0('./Surprisals/data/gpt2/items_',subsetname,'.gpt2.csv'))
  #surps.rnng <- read.csv(paste0('./Surprisals/data/rnng/items_',subsetname,'.rnng.csv'))
  surps.lstm$word_pos = surps.lstm$word_pos + 1# adjust to 1-indexing
  surps.gpt2$word_pos = surps.gpt2$word_pos + 1# adjust to 1-indexing
  #surps.rnng$word_pos = surps.rnng$word_pos + 1# adjust to 1-indexing
  rt.data.freqs <- merge(x=rt.data_df, y=freqs, by.x="word", by.y="word", all.x=TRUE)
  rt.data.freqs.surps <- merge(x=rt.data.freqs, y=surps.lstm[,c('Sentence','word_pos','sum_surprisal')], 
                           by.x=c("Sentence" ,"WordPosition"), by.y=c("Sentence" ,"word_pos"), all.x=TRUE) %>% rename(surprisal_lstm=sum_surprisal)
  rt.data.freqs.surps <- merge(x=rt.data.freqs.surps, y=surps.gpt2[,c('Sentence','word_pos','sum_surprisal')], 
                           by.x=c("Sentence", "WordPosition"), by.y=c("Sentence", "word_pos"), all.x=TRUE) %>% rename(surprisal_gpt2=sum_surprisal)
  #rt.data.freqs.surps <- merge(x=rt.data.freqs.surps, y=surps.rnng[,c('Sentence','word_pos','sum_surprisal')], 
  #                         by.x=c("Sentence", "WordPosition"), by.y=c("Sentence", "word_pos"), all.x=TRUE) %>% rename(surprisal_rnng=sum_surprisal)
  rt.data.with_lags <-  rt.data.freqs.surps %>% group_by_at(vars(item, participant)) %>%
    mutate(RT_p1 = lag(RT), 
           RT_p2 = lag(RT_p1), 
           RT_p3 = lag(RT_p2),
           length = nchar(EachWord),
           length_p1 = lag(length), 
           length_p2 = lag(length_p1),
           length_p3 = lag(length_p2),
           logfreq = log(count),
           logfreq_p1 = lag(logfreq), 
           logfreq_p2 = lag(logfreq_p1),
           logfreq_p3 = lag(logfreq_p2),
           surprisal_lstm_p1 = lag(surprisal_lstm),
           surprisal_lstm_p2 = lag(surprisal_lstm_p1),
           surprisal_lstm_p3 = lag(surprisal_lstm_p2),
           surprisal_gpt2_p1 = lag(surprisal_gpt2),
           surprisal_gpt2_p2 = lag(surprisal_gpt2_p1),
           surprisal_gpt2_p3 = lag(surprisal_gpt2_p2))
           #surprisal_rnng_p1 = lag(surprisal_rnng),
           #surprisal_rnng_p2 = lag(surprisal_rnng_p1),
           #surprisal_rnng_p3 = lag(surprisal_rnng_p2))
  rt.data.drop.lstm <- subset(rt.data.with_lags, !is.na(surprisal_lstm) & !is.na(surprisal_lstm_p1) & 
                                  !is.na(surprisal_lstm_p2) & !is.na(surprisal_lstm_p3) &
                                  !is.na(logfreq) & !is.na(logfreq_p1) &
                                  !is.na(logfreq_p2) & !is.na(logfreq_p3))
  rt.data.drop.gpt2 <- subset(rt.data.with_lags, !is.na(surprisal_gpt2) & !is.na(surprisal_gpt2_p1) & 
                                  !is.na(surprisal_gpt2_p2) & !is.na(surprisal_gpt2_p3) &
                                  !is.na(logfreq) & !is.na(logfreq_p1) &
                                  !is.na(logfreq_p2) & !is.na(logfreq_p3))
  #rt.data.drop.rnng <- subset(rt.data.with_lags, !is.na(surprisal_rnng) & !is.na(surprisal_rnng_p1) & 
  #                               !is.na(surprisal_rnng_p2) & !is.na(surprisal_rnng_p3) &
  #                               !is.na(logfreq) & !is.na(logfreq_p1) &
  #                               !is.na(logfreq_p2) & !is.na(logfreq_p3))
  rt.data.drop.lstm$surprisal_lstm_s <- scale(rt.data.drop.lstm$surprisal_lstm)
  surp_center <- attributes(rt.data.drop.lstm$surprisal_lstm_s)$`scaled:center`
  surp_scale <- attributes(rt.data.drop.lstm$surprisal_lstm_s)$`scaled:scale`
  rt.data.drop.lstm$surprisal_lstm_p1_s <- (rt.data.drop.lstm$surprisal_lstm_p1 - surp_center)/surp_scale
  rt.data.drop.lstm$surprisal_lstm_p2_s <- (rt.data.drop.lstm$surprisal_lstm_p2 - surp_center)/surp_scale
  rt.data.drop.lstm$surprisal_lstm_p3_s <- (rt.data.drop.lstm$surprisal_lstm_p3 - surp_center)/surp_scale
  rt.data.drop.lstm$length_s <- scale(rt.data.drop.lstm$length)
  surp_center <- attributes(rt.data.drop.lstm$length_s)$`scaled:center`
  surp_scale <- attributes(rt.data.drop.lstm$length_s)$`scaled:scale`
  rt.data.drop.lstm$length_p1_s <- (rt.data.drop.lstm$length_p1 - surp_center)/surp_scale
  rt.data.drop.lstm$length_p2_s <- (rt.data.drop.lstm$length_p2 - surp_center)/surp_scale
  rt.data.drop.lstm$length_p3_s <- (rt.data.drop.lstm$length_p3 - surp_center)/surp_scale
  rt.data.drop.lstm$logfreq_s <- scale(rt.data.drop.lstm$logfreq)
  surp_center <- attributes(rt.data.drop.lstm$logfreq_s)$`scaled:center`
  surp_scale <- attributes(rt.data.drop.lstm$logfreq_s)$`scaled:scale`
  rt.data.drop.lstm$logfreq_p1_s <- (rt.data.drop.lstm$logfreq_p1 - surp_center)/surp_scale
  rt.data.drop.lstm$logfreq_p2_s <- (rt.data.drop.lstm$logfreq_p2 - surp_center)/surp_scale
  rt.data.drop.lstm$logfreq_p3_s <- (rt.data.drop.lstm$logfreq_p3 - surp_center)/surp_scale
  rt.data.drop.gpt2$surprisal_gpt2_s <- scale(rt.data.drop.gpt2$surprisal_gpt2)
  surp_center <- attributes(rt.data.drop.gpt2$surprisal_gpt2_s)$`scaled:center`
  surp_scale <- attributes(rt.data.drop.gpt2$surprisal_gpt2_s)$`scaled:scale`
  rt.data.drop.gpt2$surprisal_gpt2_p1_s <- (rt.data.drop.gpt2$surprisal_gpt2_p1 - surp_center)/surp_scale
  rt.data.drop.gpt2$surprisal_gpt2_p2_s <- (rt.data.drop.gpt2$surprisal_gpt2_p2 - surp_center)/surp_scale
  rt.data.drop.gpt2$surprisal_gpt2_p3_s <- (rt.data.drop.gpt2$surprisal_gpt2_p3 - surp_center)/surp_scale
  rt.data.drop.gpt2$length_s <- scale(rt.data.drop.gpt2$length)
  surp_center <- attributes(rt.data.drop.gpt2$length_s)$`scaled:center`
  surp_scale <- attributes(rt.data.drop.gpt2$length_s)$`scaled:scale`
  rt.data.drop.gpt2$length_p1_s <- (rt.data.drop.gpt2$length_p1 - surp_center)/surp_scale
  rt.data.drop.gpt2$length_p2_s <- (rt.data.drop.gpt2$length_p2 - surp_center)/surp_scale
  rt.data.drop.gpt2$length_p3_s <- (rt.data.drop.gpt2$length_p3 - surp_center)/surp_scale
  rt.data.drop.gpt2$logfreq_s <- scale(rt.data.drop.gpt2$logfreq)
  surp_center <- attributes(rt.data.drop.gpt2$logfreq_s)$`scaled:center`
  surp_scale <- attributes(rt.data.drop.gpt2$logfreq_s)$`scaled:scale`
  rt.data.drop.gpt2$logfreq_p1_s <- (rt.data.drop.gpt2$logfreq_p1 - surp_center)/surp_scale
  rt.data.drop.gpt2$logfreq_p2_s <- (rt.data.drop.gpt2$logfreq_p2 - surp_center)/surp_scale
  rt.data.drop.gpt2$logfreq_p3_s <- (rt.data.drop.gpt2$logfreq_p3 - surp_center)/surp_scale
  #rt.data.drop.rnng$surprisal_rnng_s <- scale(rt.data.drop.rnng$surprisal_rnng)
  #surp_center <- attributes(rt.data.drop.rnng$surprisal_rnng_s)$`scaled:center`
  #surp_scale <- attributes(rt.data.drop.rnng$surprisal_rnng_s)$`scaled:scale`
  #rt.data.drop.rnng$surprisal_rnng_p1_s <- (rt.data.drop.rnng$surprisal_rnng_p1 - surp_center)/surp_scale
  #rt.data.drop.rnng$surprisal_rnng_p2_s <- (rt.data.drop.rnng$surprisal_rnng_p2 - surp_center)/surp_scale
  #rt.data.drop.rnng$surprisal_rnng_p3_s <- (rt.data.drop.rnng$surprisal_rnng_p3 - surp_center)/surp_scale
  #rt.data.drop.rnng$length_s <- scale(rt.data.drop.rnng$length)
  #surp_center <- attributes(rt.data.drop.rnng$length_s)$`scaled:center`
  #surp_scale <- attributes(rt.data.drop.rnng$length_s)$`scaled:scale`
  #rt.data.drop.rnng$length_p1_s <- (rt.data.drop.rnng$length_p1 - surp_center)/surp_scale
  #rt.data.drop.rnng$length_p2_s <- (rt.data.drop.rnng$length_p2 - surp_center)/surp_scale
  #rt.data.drop.rnng$length_p3_s <- (rt.data.drop.rnng$length_p3 - surp_center)/surp_scale
  #rt.data.drop.rnng$logfreq_s <- scale(rt.data.drop.rnng$logfreq)
  #surp_center <- attributes(rt.data.drop.rnng$logfreq_s)$`scaled:center`
  #surp_scale <- attributes(rt.data.drop.rnng$logfreq_s)$`scaled:scale`
  #rt.data.drop.rnng$logfreq_p1_s <- (rt.data.drop.rnng$logfreq_p1 - surp_center)/surp_scale
  #rt.data.drop.rnng$logfreq_p2_s <- (rt.data.drop.rnng$logfreq_p2 - surp_center)/surp_scale
  #rt.data.drop.rnng$logfreq_p3_s <- (rt.data.drop.rnng$logfreq_p3 - surp_center)/surp_scale
  rt.data.drop.lstm$predicted <- predict(models.filler.lstm, newdata=rt.data.drop.lstm, allow.new.levels = TRUE)
  rt.data.drop.gpt2$predicted <- predict(models.filler.gpt2, newdata=rt.data.drop.gpt2, allow.new.levels = TRUE)
  #rt.data.drop.rnng$predicted <- predict(models.filler.rnng, newdata=rt.data.drop.rnng, allow.new.levels = TRUE)
  return(list(rt.data.freqs.surps,rt.data.drop.lstm,rt.data.drop.gpt2))
}




Predicting_RT_with_spillover_refactored <- function(rt.data_df,subsetname, models = c('gpt2', 'lstm')){
  print("This will take a while.")
  freqs <- read.csv("freqs.csv")
  
  rt.data.freqs <- merge(x=rt.data_df, y=freqs, by.x="word", by.y="word", all.x=TRUE)
  
  pred_list <- list()
  
  i <- 1
  
  for(model in models){
    print(paste('Processing model', model))
    
    ## NOTE THIS ASSUMES THE FOLDER NAME FOR LSTM MODELS IS lstm  AND NOT gulordava
    surps <- read.csv(paste0('./Surprisals/data/', model,'/items_',subsetname,'.', model, '.csv')) %>%
      mutate(word_pos = word_pos + 1,
             model = model) %>% #adjust to 1-indexing
      select(Sentence, word_post, sum_surprisal)
    
    filler.model <- readRDS(paste0('somefilepath/', model))  ## ADD APPROPRIATE FILEPATH HERE
    
    rt.data.freqs.surps <- merge(x = rt.data.freqs,
                                 y = surps,
                                 by.x=c("Sentence" ,"WordPosition"),
                                 by.y=c("Sentence" ,"word_pos"),
                                 all.x=TRUE)
    
    rt.data.with_lags <-  rt.data.freqs.surps %>% group_by_at(vars(item, participant)) %>%
      mutate(RT_p1 = lag(RT), 
             RT_p2 = lag(RT_p1), 
             RT_p3 = lag(RT_p2),
             length = nchar(EachWord),
             length_p1 = lag(length), 
             length_p2 = lag(length_p1),
             length_p3 = lag(length_p2),
             logfreq = log(count),
             logfreq_p1 = lag(logfreq), 
             logfreq_p2 = lag(logfreq_p1),
             logfreq_p3 = lag(logfreq_p2),
             surprisal_p1 = lag(sum_surprisal),
             surprisal_p2 = lag(sum_surprisal_p1),
             surprisal_p3 = lag(sum_surprisal_p2))
    
    rt.data.drop <- subset(rt.data.with_lags, !is.na(sum_surprisal) & !is.na(sum_surprisal_p1) & 
                            !is.na(sum_surprisal_p2) & !is.na(sum_surprisal_p3) &
                            !is.na(logfreq) & !is.na(logfreq_p1) &
                            !is.na(logfreq_p2) & !is.na(logfreq_p3))
    
    rt.data.drop$predicted <- predict(models.filler, newdata=rt.data.drop, allow.new.levels = TRUE)
  
    pred_list[[i]] <- rt.data.drop
    
    i <- i + 1
    
  }
  
  pred_dat <- dplyr::bind_rows(pred_list)

  return(pred_dat)
}



Plot_surprisal_construction_level <- function(coef_surprisal_lstm,coef_surprisal_gpt2,subset_name, axistitle.size=14, axistext.size=14,facetlabel.size=10){
  if(subset_name=="ClassicGP"){
    coef_surprisal <- rbind(coef_surprisal_lstm,coef_surprisal_gpt2) %>% mutate(model=rep(c("lstm","gpt2"),each=nrow(coef_surprisal_lstm)))
    coef_surprisal$mean <- c(coef_surprisal$mean[1],coef_surprisal$mean[1]+coef_surprisal$mean[2],
                             coef_surprisal$mean[1]+coef_surprisal$mean[3],
                             coef_surprisal$mean[4],coef_surprisal$mean[4]+coef_surprisal$mean[5],
                             coef_surprisal$mean[4]+coef_surprisal$mean[6])
    coef_surprisal$coef <- rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),2)
    ggplot(data=coef_surprisal, aes(x=coef, y=mean)) +
      facet_grid(~model)+
      geom_bar(stat="identity",position=position_dodge())+
      xlab("Surprisal_ClassicGP Subset")+
      ylab("Mean Garden Path Effect")+
      theme(axis.title=element_text(size=axistitle.size,face="bold"),
            axis.text = element_text(size=axistext.size,face="bold"),
            strip.text = element_text(size=facetlabel.size,face="bold"))
  }
}



