library(lme4)
library(tidyr)
library(stringr)
library(posterior)
library(tidybayes)
library(tidyverse)
library(dplyr)
library(brms, lib.loc="../../rpackages")


load_data <- function(subsetname,RTcutoffhigh=7000,RTcutofflow=0){
  id <- ifelse(subsetname=="ClassicGP","1TanAUeI1x_G0mkFsFnpru0udi8oRvxNH",
                ifelse(subsetname=="RelativeClause","1ndHGJxTV51AEpJ2MKQxpHUpmCuj-Lm-W",
                  ifelse(subsetname=="AttachmentAmbiguity","1TShRMEgba4z0tgN5zj48-k-3FB-hl5Gj",
                    ifelse(subsetname=="Agreement","1V6m9d20CbB1GeadR6SQ6yeiRyoqz3zwJ",
                           ifelse(subsetname=="Fillers","16onEVQVsFgusBZXuVPrtwcIiDaJgV6WB","")))))
  rt.data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download&confirm=t", id), header=TRUE) %>% mutate(participant=MD5)
  rt.data$RT <- ifelse(rt.data$RT>RTcutoffhigh,NA,ifelse(rt.data$RT<RTcutofflow,NA,rt.data$RT))
  rt.data$Sentence <- str_replace_all(rt.data$Sentence, "%2C", ",")
  rt.data$EachWord <- str_replace_all(rt.data$EachWord, "%2C", ",")
  rt.data$word <-  tolower(ifelse(substring(rt.data$EachWord,nchar(rt.data$EachWord),nchar(rt.data$EachWord))%in%c(".",","),substring(rt.data$EachWord,1,nchar(rt.data$EachWord)-1),rt.data$EachWord))
  return(rt.data)
}



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
    fixedeffcts_df$coef <- factor(fixedeffcts_df$coef,levels=c("RC"),labels=c("Subject / object relative clause"))
    fixedeffcts_df$ROI <- factor(fixedeffcts_df$ROI,levels=c("0","1","2"),labels=c("Verb","Det","Noun"))
    ggplot(data=fixedeffcts_df, aes(x=coef, y=mean, fill=ROI)) +
      geom_bar(stat="identity",position=position_dodge())+
      scale_fill_manual(values = ROIcolor)+
      #scale_fill_discrete(labels=c("Verb","Det","Noun"))+
      geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
      xlab("Relative clause subset")+
      ylab("Empirical relative clause effect size (ms)")+
      theme(axis.title=element_text(size=axistitle.size,face="bold"),
            axis.text = element_text(size=axistext.size,face="bold"),
            legend.title = element_text(size=legendtitle.size),
            legend.text = element_text(size=legendtext.size))
  }else{
    if(subset_name=="AttachmentAmbiguity"){
      fixedeffcts_df$coef <- factor(fixedeffcts_df$coef,levels=c("GPE_high","GPE_low"),labels=c("High attachment","Low attachment"))
      ggplot(data=fixedeffcts_df, aes(x=coef, y=mean, fill=ROI)) +
        geom_bar(stat="identity",position=position_dodge())+
        scale_fill_manual(values = ROIcolor)+
        geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
        xlab("Attachment ambiguity subset")+
        ylab("Empirical attachment site effect size (ms)")+
        theme(axis.title=element_text(size=axistitle.size,face="bold"),
              axis.text = element_text(size=axistext.size,face="bold"),
              legend.title = element_text(size=legendtitle.size),
              legend.text = element_text(size=legendtext.size))
    }else{
      if(subset_name=="Agreement"){
        fixedeffcts_df$coef <- factor(fixedeffcts_df$coef,levels=c("Agr"),labels=c("Subject-verb agreement mismatch"))
        ggplot(data=fixedeffcts_df, aes(x=coef, y=mean, fill=ROI)) +
          geom_bar(stat="identity",position=position_dodge())+
          scale_fill_manual(values = ROIcolor)+
          geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
          xlab("Agreement subset")+
          ylab("Empirical ungrammaticality effect size (ms)")+
          theme(axis.title=element_text(size=axistitle.size,face="bold"),
                axis.text = element_text(size=axistext.size,face="bold"),
                legend.title = element_text(size=legendtitle.size),
                legend.text = element_text(size=legendtext.size))
      }else{
        if(subset_name=="ClassicGP"){
          fixedeffcts_df$coef <- factor(fixedeffcts_df$coef,levels=c("GPE_MVRR","GPE_NPS","GPE_NPZ"),labels=c("Main verb/\nreduced relative clause","Direct object/\nsentential complement","Transitive/\nintranstive"))
          ggplot(data=fixedeffcts_df, aes(x=coef, y=mean, fill=ROI)) +
            geom_bar(stat="identity",position=position_dodge())+
            scale_fill_manual(values = ROIcolor)+
            geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
            xlab("Classic garden path subset")+
            ylab("Empirical garden path effect size (ms)")+
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
  surp_files <- c(paste0('../../Surprisals/data/lstm/items_',subsetname,'.lstm.csv.scaled'),
                  paste0('../../Surprisals/data/gpt2/items_',subsetname,'.gpt2.csv.scaled'),
                  paste0('../../Surprisals/data/rnng/items_',subsetname,'.rnng.csv.scaled'))
  surp_list <- list()
  i <- 1
  for(fname in surp_files){
    model_name <- strsplit(fname, '.', fixed=TRUE)[[1]][8]
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
        surp.diff <- data.frame(item=rep(surp.diff$item.x[seq(1,nrow(surp.diff),by=3)],2),coef=rep(c("GPE_high","GPE_low"),each=(nrow(surp.diff)/length(unique(surp.diff$Type)))),ROI=rep(surp.diff$ROI[seq(1,nrow(surp.diff),by=3)],2),model=rep(surp.diff$model[seq(1,nrow(surp.diff),by=3)],2),surprisal_diff=c(surp.diff$sum_surprisal[seq(1,nrow(surp.diff),by=3)]-surp.diff$sum_surprisal[seq(3,nrow(surp.diff),by=3)],surp.diff$sum_surprisal[seq(2,nrow(surp.diff),by=3)]-surp.diff$sum_surprisal[seq(3,nrow(surp.diff),by=3)]))    
      }else{
        surp.diff <- data.frame(item=surp.diff$item.x[seq(1,nrow(surp.diff),by=2)],coef=rep(c("Agr"),(nrow(surp.diff)/2/length(unique(byitem_df$coef)))),ROI=surp.diff$ROI[seq(1,nrow(surp.diff),by=2)],model=surp.diff$model[seq(1,nrow(surp.diff),by=2)],surprisal_diff=surp.diff$sum_surprisal[seq(2,nrow(surp.diff),by=2)]-surp.diff$sum_surprisal[seq(1,nrow(surp.diff),by=2)])    
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
                                       merged_df$surprisal_diff[merged_df$ROI==0&merged_df$coef==dat_text[i,'coef']&merged_df$model==dat_text[i,'model']])$estimate,2)
  }
  dat_text$label <- sprintf(
    "corr = %s",
    dat_text$corr
  )
  merged_df$surprisal_diff <- merged_df$surprisal_diff[1:(nrow(merged_df)/3)]
  ggplot(merged_df[merged_df$ROI==ROI_index,],aes(x=surprisal_diff,y=mean))+
    geom_point()+
    facet_grid(model~coef)+
    xlab("Surprisal difference (ROI=0)")+
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




Predicting_RT_with_spillover <- function(rt.data_df,subsetname, models = c('gpt2', 'lstm',"nosurp")){
  print("This will take a while.")
  
  pred_list <- list()
  
  if(subsetname=="Agreement"){
    i <- 1
    for(model in models){
      if(i!=3){
        print(paste('Processing model', model))
        
        ## NOTE THIS ASSUMES THE FOLDER NAME FOR LSTM MODELS IS lstm  AND NOT gulordava
        surps <- read.csv(paste0('../../Surprisals/data/', model,'/items_',subsetname,'.', model, '.csv.scaled')) %>%
          mutate(word_pos = word_pos + 1,
                 model = model) %>% #adjust to 1-indexing
          select(Sentence, word_pos, sum_surprisal , sum_surprisal_s,logfreq,logfreq_s,length,length_s)
        surps <- rename(surps, surprisal=sum_surprisal,surprisal_s=sum_surprisal_s)
        surps2 <- read.csv(paste0('../../Surprisals/data/', model,'/items_ClassicGP.', model, '.csv.scaled')) %>%
          filter(condition%in%c("NPZ_UAMB","NPZ_AMB")&item%in%unique(rt.data_df$item[rt.data_df$Type=="AGREE"])) %>%
          mutate(word_pos = word_pos + 1,
                 model = model) %>% #adjust to 1-indexing
          select(Sentence, word_pos, sum_surprisal , sum_surprisal_s,logfreq,logfreq_s,length,length_s)
        surps2 <- rename(surps2, surprisal=sum_surprisal,surprisal_s=sum_surprisal_s)
        surps <- rbind(surps,surps2)
        
        filler.model <- readRDS(paste0('../../Surprisals/analysis/filler_models/filler_', model, '_logadd_sum.rds')) 
        
        rt.data.freqs.surps <- merge(x = rt.data_df,
                                     y = surps,
                                     by.x=c("Sentence" ,"WordPosition"),
                                     by.y=c("Sentence" ,"word_pos"),
                                     all.x=TRUE) %>% arrange(Type,item,WordPosition,participant)
        
        rt.data.with_lags <-  rt.data.freqs.surps %>% group_by_at(vars(item, participant)) %>%
          mutate(RT_p1 = lag(RT), 
                 RT_p2 = lag(RT_p1), 
                 RT_p3 = lag(RT_p2),
                 length_p1_s = lag(length_s), 
                 length_p2_s = lag(length_p1_s),
                 length_p3_s = lag(length_p2_s),
                 logfreq_p1_s = lag(logfreq_s), 
                 logfreq_p2_s = lag(logfreq_p1_s),
                 logfreq_p3_s = lag(logfreq_p2_s),
                 surprisal_p1_s = lag(surprisal_s),
                 surprisal_p2_s = lag(surprisal_p1_s),
                 surprisal_p3_s = lag(surprisal_p2_s))
        rt.data.with_lags$sent_length <- lapply(str_split(rt.data.with_lags$Sentence," "),length)
        
        rt.data.drop <- subset(rt.data.with_lags, !is.na(surprisal_s) & !is.na(surprisal_p1_s) & 
                                 !is.na(surprisal_p2_s) & !is.na(surprisal_p3_s) &
                                 !is.na(logfreq_s) & !is.na(logfreq_p1_s) &
                                 !is.na(logfreq_p2_s) & !is.na(logfreq_p3_s)&
                                 (rt.data.with_lags$sent_length!=rt.data.with_lags$WordPosition))
        
        rt.data.drop$predicted <- predict(filler.model, newdata=rt.data.drop, allow.new.levels = TRUE)
        rt.data.drop$model <- model
        pred_list[[i]] <- rt.data.drop
        
        i <- i + 1
      }
      else{
        print(paste('Processing model', model))
        surps <- read.csv(paste0('../../Surprisals/data/lstm/items_',subsetname,'.lstm.csv.scaled')) %>%
          mutate(word_pos = word_pos + 1,
                 model = model) %>% #adjust to 1-indexing
          select(Sentence, word_pos, sum_surprisal , sum_surprisal_s,logfreq,logfreq_s,length,length_s)
        surps <- rename(surps, surprisal=sum_surprisal,surprisal_s=sum_surprisal_s)
        surps2 <- read.csv(paste0('../../Surprisals/data/lstm/items_ClassicGP.lstm.csv.scaled')) %>%
          filter(condition%in%c("NPZ_UAMB","NPZ_AMB")&item%in%unique(rt.data_df$item[rt.data_df$Type=="AGREE"])) %>%
          mutate(word_pos = word_pos + 1,
                 model = model) %>% #adjust to 1-indexing
          select(Sentence, word_pos, sum_surprisal , sum_surprisal_s,logfreq,logfreq_s,length,length_s)
        surps2 <- rename(surps2, surprisal=sum_surprisal,surprisal_s=sum_surprisal_s)
        surps <- rbind(surps,surps2)
        
        filler.model <- readRDS(paste0('../../Surprisals/analysis/filler_models/filler_nosurp_logadd_sum.rds')) 
        
        rt.data.freqs.surps <- merge(x = rt.data_df,
                                     y = surps,
                                     by.x=c("Sentence" ,"WordPosition"),
                                     by.y=c("Sentence" ,"word_pos"),
                                     all.x=TRUE) %>% arrange(Type,item,WordPosition,participant)
        
        rt.data.with_lags <-  rt.data.freqs.surps %>% group_by_at(vars(item, participant)) %>%
          mutate(RT_p1 = lag(RT), 
                 RT_p2 = lag(RT_p1), 
                 RT_p3 = lag(RT_p2),
                 length_p1_s = lag(length_s), 
                 length_p2_s = lag(length_p1_s),
                 length_p3_s = lag(length_p2_s),
                 logfreq_p1_s = lag(logfreq_s), 
                 logfreq_p2_s = lag(logfreq_p1_s),
                 logfreq_p3_s = lag(logfreq_p2_s),
                 surprisal_p1_s = lag(surprisal_s),
                 surprisal_p2_s = lag(surprisal_p1_s),
                 surprisal_p3_s = lag(surprisal_p2_s))
        rt.data.with_lags$sent_length <- lapply(str_split(rt.data.with_lags$Sentence," "),length)
        
        rt.data.drop <- subset(rt.data.with_lags, !is.na(surprisal_s) & !is.na(surprisal_p1_s) & 
                                 !is.na(surprisal_p2_s) & !is.na(surprisal_p3_s) &
                                 !is.na(logfreq_s) & !is.na(logfreq_p1_s) &
                                 !is.na(logfreq_p2_s) & !is.na(logfreq_p3_s)&
                                 (rt.data.with_lags$sent_length!=rt.data.with_lags$WordPosition))
        
        rt.data.drop$predicted <- predict(filler.model, newdata=rt.data.drop, allow.new.levels = TRUE)
        rt.data.drop$model <- model
        pred_list[[i]] <- rt.data.drop
        
        i <- i + 1
      }
    }
  }
  else{
    i <- 1
    for(model in models){
      if(i!=3){
        print(paste('Processing model', model))
        
        ## NOTE THIS ASSUMES THE FOLDER NAME FOR LSTM MODELS IS lstm  AND NOT gulordava
        surps <- read.csv(paste0('../../Surprisals/data/', model,'/items_',subsetname,'.', model, '.csv.scaled')) %>%
          mutate(word_pos = word_pos + 1,
                 model = model) %>% #adjust to 1-indexing
          select(Sentence, word_pos, sum_surprisal , sum_surprisal_s,logfreq,logfreq_s,length,length_s)
        surps <- rename(surps, surprisal=sum_surprisal,surprisal_s=sum_surprisal_s)
        
        filler.model <- readRDS(paste0('../../Surprisals/analysis/filler_models/filler_', model, '_logadd_sum.rds')) 
        
        rt.data.freqs.surps <- merge(x = rt.data_df,
                                     y = surps,
                                     by.x=c("Sentence" ,"WordPosition"),
                                     by.y=c("Sentence" ,"word_pos"),
                                     all.x=TRUE) %>% arrange(Type,item,WordPosition,participant)
        
        rt.data.with_lags <-  rt.data.freqs.surps %>% group_by_at(vars(item, participant)) %>%
          mutate(RT_p1 = lag(RT), 
                 RT_p2 = lag(RT_p1), 
                 RT_p3 = lag(RT_p2),
                 length_p1_s = lag(length_s), 
                 length_p2_s = lag(length_p1_s),
                 length_p3_s = lag(length_p2_s),
                 logfreq_p1_s = lag(logfreq_s), 
                 logfreq_p2_s = lag(logfreq_p1_s),
                 logfreq_p3_s = lag(logfreq_p2_s),
                 surprisal_p1_s = lag(surprisal_s),
                 surprisal_p2_s = lag(surprisal_p1_s),
                 surprisal_p3_s = lag(surprisal_p2_s))
        rt.data.with_lags$sent_length <- lapply(str_split(rt.data.with_lags$Sentence," "),length)
        
        rt.data.drop <- subset(rt.data.with_lags, !is.na(surprisal_s) & !is.na(surprisal_p1_s) & 
                                 !is.na(surprisal_p2_s) & !is.na(surprisal_p3_s) &
                                 !is.na(logfreq_s) & !is.na(logfreq_p1_s) &
                                 !is.na(logfreq_p2_s) & !is.na(logfreq_p3_s)&
                                 (rt.data.with_lags$sent_length!=rt.data.with_lags$WordPosition))
        
        rt.data.drop$predicted <- predict(filler.model, newdata=rt.data.drop, allow.new.levels = TRUE)
        rt.data.drop$model <- model
        pred_list[[i]] <- rt.data.drop
        
        i <- i + 1
      }
      else{
        print(paste('Processing model', model))
        
        ## NOTE THIS ASSUMES THE FOLDER NAME FOR LSTM MODELS IS lstm  AND NOT gulordava
        surps <- read.csv(paste0('../../Surprisals/data/lstm/items_',subsetname,'.lstm.csv.scaled')) %>%
          mutate(word_pos = word_pos + 1,
                 model = model) %>% #adjust to 1-indexing
          select(Sentence, word_pos, sum_surprisal , sum_surprisal_s,logfreq,logfreq_s,length,length_s)
        surps <- rename(surps, surprisal=sum_surprisal,surprisal_s=sum_surprisal_s)
        
        filler.model <- readRDS(paste0('../../Surprisals/analysis/filler_models/filler_nosurp_logadd_sum.rds')) 
        
        rt.data.freqs.surps <- merge(x = rt.data_df,
                                     y = surps,
                                     by.x=c("Sentence" ,"WordPosition"),
                                     by.y=c("Sentence" ,"word_pos"),
                                     all.x=TRUE) %>% arrange(Type,item,WordPosition,participant)
        
        rt.data.with_lags <-  rt.data.freqs.surps %>% group_by_at(vars(item, participant)) %>%
          mutate(RT_p1 = lag(RT), 
                 RT_p2 = lag(RT_p1), 
                 RT_p3 = lag(RT_p2),
                 length_p1_s = lag(length_s), 
                 length_p2_s = lag(length_p1_s),
                 length_p3_s = lag(length_p2_s),
                 logfreq_p1_s = lag(logfreq_s), 
                 logfreq_p2_s = lag(logfreq_p1_s),
                 logfreq_p3_s = lag(logfreq_p2_s),
                 surprisal_p1_s = lag(surprisal_s),
                 surprisal_p2_s = lag(surprisal_p1_s),
                 surprisal_p3_s = lag(surprisal_p2_s))
        rt.data.with_lags$sent_length <- lapply(str_split(rt.data.with_lags$Sentence," "),length)
        
        rt.data.drop <- subset(rt.data.with_lags, !is.na(surprisal_s) & !is.na(surprisal_p1_s) & 
                                 !is.na(surprisal_p2_s) & !is.na(surprisal_p3_s) &
                                 !is.na(logfreq_s) & !is.na(logfreq_p1_s) &
                                 !is.na(logfreq_p2_s) & !is.na(logfreq_p3_s)&
                                 (rt.data.with_lags$sent_length!=rt.data.with_lags$WordPosition))
        
        rt.data.drop$predicted <- predict(filler.model, newdata=rt.data.drop, allow.new.levels = TRUE)
        rt.data.drop$model <- model
        pred_list[[i]] <- rt.data.drop
        
        i <- i + 1
      }
    }
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



