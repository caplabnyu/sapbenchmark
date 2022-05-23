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
Plot_itemwise_by_magnitude <- function(byitem_df,subset_name,ROI_index,axistitle.size=14, axistext.size=14,striptext.size=14){
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
        strip.text.x=element_text(size=striptext.size,face="bold"))
  }else{
    if(subset_name=="RelativeClause"){
      #ROI_class <- ifelse(ROI_index==0,"Verb",ifelse(ROI_index==1,"Det","Noun"))
      ggplot(byitem_df[byitem_df$ROI==ROI_index,],aes(x=RANK,y=mean))+
        #facet_wrap(~coef)+
        geom_point()+
        geom_errorbar(aes(ymin=lower,ymax=upper),width=.2,position=position_dodge(.9))+
        #ggtitle(paste0('ROI=',ROI_class))+
        xlab("Magnitude Rank")+
        ylab("Item-wise Relative Clause Effects")+
        theme(axis.title=element_text(size=axistitle.size,face="bold"),
              axis.text = element_text(size=axistext.size),
              strip.text.x=element_text(size=striptext.size,face="bold"))
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
                strip.text.x=element_text(size=striptext.size,face="bold"))
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
                  strip.text.x=element_text(size=striptext.size,face="bold"))
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
    select(item.x, Type, sumsurprisal, ROI, participant, model) %>%
    group_by(item.x, Type, ROI, model) %>%
    summarize(sumsurprisal = mean(sumsurprisal))%>%
    ungroup()%>%arrange(model,ROI)
  if(subsetname=="ClassicGP"){
  surp.diff <- data.frame(item=surp.diff$item.x[seq(1,nrow(surp.diff),by=2)],coef=rep(c("GPE_MVRR","GPE_NPS","GPE_NPZ"),(nrow(surp.diff)/2/length(unique(byitem_df$coef)))),ROI=surp.diff$ROI[seq(1,nrow(surp.diff),by=2)],model=surp.diff$model[seq(1,nrow(surp.diff),by=2)],surprisal_diff=surp.diff$sumsurprisal[seq(1,nrow(surp.diff),by=2)]-surp.diff$sumsurprisal[seq(2,nrow(surp.diff),by=2)])
  }else{
    if(subsetname=="RelativeClause"){
      surp.diff <- data.frame(item=surp.diff$item.x[seq(1,nrow(surp.diff),by=2)],coef=rep(c("RC"),(nrow(surp.diff)/2/length(unique(byitem_df$coef)))),ROI=surp.diff$ROI[seq(1,nrow(surp.diff),by=2)],model=surp.diff$model[seq(1,nrow(surp.diff),by=2)],surprisal_diff=surp.diff$sumsurprisal[seq(1,nrow(surp.diff),by=2)]-surp.diff$sumsurprisal[seq(2,nrow(surp.diff),by=2)])    
    }else{
      if(subsetname=="AttachmentAmbiguity"){
        surp.diff <- data.frame(item=rep(surp.diff$item.x[seq(1,nrow(surp.diff),by=3)],2),coef=rep(c("High_Cost","Low_Cost"),each=(nrow(surp.diff)/length(unique(surp.diff$Type)))),ROI=rep(surp.diff$ROI[seq(1,nrow(surp.diff),by=3)],2),model=rep(surp.diff$model[seq(1,nrow(surp.diff),by=3)],2),surprisal_diff=c(surp.diff$sumsurprisal[seq(1,nrow(surp.diff),by=3)]-surp.diff$sumsurprisal[seq(3,nrow(surp.diff),by=3)],surp.diff$sumsurprisal[seq(2,nrow(surp.diff),by=3)]-surp.diff$sumsurprisal[seq(3,nrow(surp.diff),by=3)]))    
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
