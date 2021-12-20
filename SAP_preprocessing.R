#This is a script to preprocessing data downloaded from PCIbex farm
#The preprocessing includes: removing inattentive participants, removing trials involving typos earlier in the experiment script
#removing trials that were accidentally preceded by trials of the same construction (syntactic priming; the experiment script was later fixed)
#and marking critical word positions for each trial (this requires the "Position_Info.csv" file)

mycols = c("Time","MD5","Controller","Element","Type","Group","PennElementType","PennElementName","WordPosition","EachWord","EventTime","Sentence","Question","Answer","List","item","KeyPress","RT_Answering","EachWordRT","DontKnow","Sentenceagain","Comments")
#PCIbex added an automatically stored column on Dec 5th; data collected after that have one additional column
results_intact=read.csv("all_data_unprocessed.csv",header = 0, sep = ",", comment.char = "#", stringsAsFactors = F,col.names = mycols)
#store one row for each trial (ignoring word-by-word reading)
onerowpertrial <- results_intact[results_intact$EachWordRT=="Wait success"&results_intact$Type!="Practice",]

library(dplyr)
#accuracy (attention check) is based on fillers only
FILLERS_only <- results_intact[results_intact$PennElementName=="KP"&results_intact$Type!="Practice",] %>% filter(Type%in%c("FILLER1","FILLER2"))
FILLERS_only$correct <- ifelse(FILLERS_only$EachWord==FILLERS_only$Answer,1,0)
aggregate(FILLERS_only$correct,by=list(FILLERS_only$MD5),FUN=mean) %>% filter(x <.8)
bad_participants <- aggregate(FILLERS_only$correct,by=list(FILLERS_only$MD5),FUN=mean) %>% filter(x <.8) %>% select(Group.1)
bad_participants <- bad_participants$Group.1

#remove bad participants
results <- results_intact[!results_intact$MD5%in%bad_participants,]
#remove those participants from non-English-speaking regions (n=16)
nonNorthAmerican <- c("e6096fabb70c159e5128f57182c43ff7","25fcdacfc02a73e28a0daa96edcd2fb9","62a8e6449a6c1d29205b7eebb4014451","d2e9e7997a4710b1f5090c0ca7b2b3fd","de065e25c1ad04810f19c6b67aef4e96","53d69bb6d24b53e9c828ed0fe9718b30","9f506a52e3f03d2cb1d4f26e14270a36","31a8ac145a396b08a9f9358b3ef14808","1a03c2b5cd36c01c02cf2561a786c7b7","9d3d8ec6208556f44d561631c57c81f7","73f60f0219b4af900b5079722c9d438b","d090e1e5f5136c2732411f3b084edc6c","1c2abc27ed1d232911c2820ac49495bb","5361f24ee76265761711ae6b7c784a45","f3bbc390c08639e1d732b465814fea7b","430a54a44e7f68f50895d5460e7dad61")
results <- results[!results$MD5%in%nonNorthAmerican,]
#update the onerowpertrial
onerowpertrial <- results[results$EachWordRT=="Wait success"&results$Type!="Practice",]


###remove additional participants due to a very weird glitch on PCIbex farm
###16 of participants had wrong presentation blocks (not having agreement-error trials in the end of the experiment)
v <- vector()
for(i in seq(1,nrow(onerowpertrial),92)){
  v <- c(v, i:(i+73))
}
wrongblock <- unique(onerowpertrial[v,'MD5'][which(onerowpertrial[v,'Type']%in%c("AGREE","AGREE_UNG"))])
bad_participants <- c(bad_participants,wrongblock)
results <- results[!results$MD5%in%bad_participants,]
onerowpertrial <- results[results$EachWordRT=="Wait success"&results$Type!="Practice",]
#184000/92= 2000 subjs
#aggregate(onerowpertrial$Time,by=list(onerowpertrial$List),FUN=length)$x/92  #check if lists were equally presented

#for the first 40 participants there are some coding errors & typos (trivial; not influencing the results!)
results$Answer <- ifelse(results$item%in%c(37,39,43,45,47),
                          "0",ifelse(
                           results$item%in%c(38,40,42,44,46,48),
                            "1",ifelse(
                             results$Question=="Was the chicken disturbed by the boy's attack?",
                              "0", results$Answer)))
results$Question <- ifelse(grepl("assistant being",results$Question),"Was the assistant manager being harsh?",results$Question)
#the following study sentences and questions are revised after the first 40 participants; coded NA for those early participants
results$Sentence <- ifelse(grepl("contract another",results$Sentence),NA,results$Sentence)
results$RT_Answering <- ifelse(results$Question%in%c("What did the technician report?","Did the little girl remain calm?","Was the machine typically inefficient?","What did the corrupt politician mention?","What style was the house?"),
                               NA, results$RT_Answering)
results$Question <- ifelse(results$Question%in%c("What did the technician report?","Did the little girl remain calm?","Was the machine typically inefficient?","What did the corrupt politician mention?","What style was the house?"),
                                                 NA,results$Question)


#summarize accuracy by item
hasanswer <- onerowpertrial[onerowpertrial$Answer!="NA ",]
hasanswer$correct <- ifelse(hasanswer$EachWord==hasanswer$Answer,1,0)
byitem_accuracy <- arrange(aggregate(hasanswer$correct,by=list(hasanswer$Sentence,hasanswer$Question,hasanswer$item,hasanswer$Type),FUN=mean),x)


#some items have no definite answers (ambiguous-attachment sentences)
noanswer <- onerowpertrial[onerowpertrial$Answer=="NA ",]
aggregate(as.numeric(noanswer$EachWord),by=list(noanswer$Sentence,noanswer$item),FUN=mean)


#check if there were repeated IP addresses (MD5)
MD5_ID <- results[which(results$WordPosition=="prolific"),c('MD5','EachWord')]
colnames(MD5_ID) <- c('MD5','worker_ID')
#"True" means there are no repeated IP addresses
length(unique(MD5_ID$MD5))==length(unique(MD5_ID$worker_ID))

onerowpertrial$CONSTRUCTION <- ifelse(onerowpertrial$Type %in% c("AGREE_UNG","AGREE"),"NP/Z Agreement",
                                      ifelse(onerowpertrial$Type %in% c("NPS_UAMB","NPS_AMB"),"NPS",
                                             ifelse(onerowpertrial$Type %in% c("NPZ_UAMB","NPZ_AMB"),"NPZ",
                                                    ifelse(onerowpertrial$Type%in%c("MVRR_UAMB","MVRR_AMB"),"MVRR",ifelse(
                                                      onerowpertrial$Type%in%c("RC_Subj","RC_Obj"),"RelativeClause",ifelse(
                                                        onerowpertrial$Type%in%c("AttachMulti","AttachHigh","AttachLow"),"Attachment",onerowpertrial$Type
                                                      ))))))
###identify the pseudorandom order error
###(This was an error about random trial ordering in the script early in the experiment)
###sometimes, for example, NPS_UAMB would be immediately followed by NPS_AMB, potentially causing syntactic priming
###it was fixed later;  note only a small proportion of the total trials were affected
###these trials did not enter the analysis
for(i in 1:nrow(onerowpertrial)){
  if(i==1){
    onerowpertrial$consec[i]="no"
      }else{
    if(onerowpertrial$CONSTRUCTION[i]==onerowpertrial$CONSTRUCTION[i-1]){
      onerowpertrial$consec[i]="yes"
        }else{
          onerowpertrial$consec[i]="no"
    }
  }
}
#it's ok for fillers to occur consecutively
onerowpertrial[onerowpertrial$Type%in%c("FILLER1","FILLER2")&onerowpertrial$consec=="yes",'consec'] <- "no"
aggregate(onerowpertrial$EachWord,by=list(onerowpertrial$Type,onerowpertrial$consec),FUN=length)
#unique(onerowpertrial$Time)
firstbatch <- onerowpertrial[onerowpertrial$Time<1634584174,]
#first 358 had pseudorandom issues
#they only accounted for a small proportion of the toal trials
#aggregate(firstbatch$Time[firstbatch$consec=="yes"],by=list(firstbatch$MD5[firstbatch$consec=="yes"]),FUN=length)
#aggregate(firstbatch$Time[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION!="NP/Z Agreement"],by=list(firstbatch$MD5[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION!="NP/Z Agreement"]),FUN=length)
#aggregate(firstbatch$Time[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("NPS","NPZ","MVRR")],by=list(firstbatch$MD5[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("NPS","NPZ","MVRR")],firstbatch$Type[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("NPS","NPZ","MVRR")]),FUN=length)
#aggregate(firstbatch$Time[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("Attachment")],by=list(firstbatch$MD5[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("Attachment")],firstbatch$Type[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("Attachment")]),FUN=length)
#aggregate(firstbatch$Time[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("RelativeClause")],by=list(firstbatch$MD5[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("RelativeClause")],firstbatch$Type[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("RelativeClause")]),FUN=length)
#aggregate(firstbatch$Time[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("NP/Z Agreement")],by=list(firstbatch$MD5[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("NP/Z Agreement")],firstbatch$Type[firstbatch$consec=="yes"&firstbatch$CONSTRUCTION%in%c("NP/Z Agreement")]),FUN=length)
secondbatch <- onerowpertrial[onerowpertrial$Time>=1634584174,]
#the rest have no same-consturction-in-a-row issue
which(secondbatch$consec=="yes")


results_corrected <- results
#Position_Infor.csv stores information about which words are the target word for each sentence
position_info <- read.csv("Position_Info.csv")
results_corrected <- full_join(results_corrected,position_info)
results_corrected <- results_corrected %>% filter(!WordPosition%in%c("_Trial_","prolific","consent","PressedKey","debrief1","debrief2","debrief3"))
#filter out practice trials
results_corrected <- results_corrected %>% filter(item<1000)


results_corrected <- left_join(results_corrected,onerowpertrial[,c('MD5','item','Type','consec','CONSTRUCTION')])
results_corrected <- left_join(results_corrected,hasanswer[,c('MD5','item','Type','correct')])
#removed consecutive trials (potentially syntactically primed)
rt.data <- results_corrected[results_corrected$consec=="no",] %>% 
  mutate(WordPosition = as.numeric(WordPosition),
         ROI = WordPosition - CriticalPosition) %>%
  #If ROI = 0, it's the target word, if 1 and 2, spillover 1 and spillover 2 region
  mutate(RT = as.numeric(EachWordRT),
         EachWordRT = NULL,
         Controller = NULL,
         Element = NULL,
         Group = NULL,
         PennElementType = NULL,
         PennElementName = NULL,
         KeyPress = NULL,
         DontKnow = NULL,
         Sentenceagain =NULL,
         Comments = NULL)
#revise the ROI region for relative clause conditions because it's impossible to compare the same spillover words
#instead the verb and noun within the relative clause are aligned across Subj and Obj relative clauses.
#0 = verb, 1 = determiner, 2 = noun
rt.data$ROI <- ifelse(
  rt.data$Type=="RC_Subj",ifelse(grepl("rested on",rt.data$Sentence)&rt.data$ROI==1,NA,ifelse(
    grepl("rested on", rt.data$Sentence)&rt.data$ROI==2,1,ifelse(
      grepl("rested on", rt.data$Sentence)&rt.data$ROI==3,2,rt.data$ROI
    )
  )),ifelse(rt.data$Type=="RC_Obj"&rt.data$ROI%in%c(1,2),NA,ifelse(
      rt.data$Type=="RC_Obj"&rt.data$ROI==-2,1,ifelse(
        rt.data$Type=="RC_Obj"&rt.data$ROI==-1,2, ifelse(
          rt.data$Type=="RC_Obj"&rt.data$ROI==-3,-1,ifelse(
            rt.data$Type=="RC_Obj"&rt.data$ROI==-4,-2,rt.data$ROI
          )
        )
      )
    )
  )
) %>% as.factor()

rt.data$AMBIG <- ifelse(rt.data$Type%in%c("FILLER1","FILLER2"),NA,ifelse(rt.data$Type %in% c("AGREE","NPS_UAMB","NPZ_UAMB","MVRR_UAMB","RC_Subj","AttachMulti"),"Unamb","Amb"))
rt.data$AMBUAMB <- ifelse(rt.data$AMBIG=="Unamb",0,1)
rt.data$item <- as.factor(rt.data$item)
rt.data$MD5 <- as.factor(rt.data$MD5)

#this is to average RTs across the three words as one dependent variable
ROI012 <- rt.data[rt.data$ROI==0|rt.data$ROI==1|rt.data$ROI==2,c('MD5','item','Type','RT')]
ROI012 <- ROI012[!is.na(ROI012$MD5),]
ROIcombined <- ROI012 %>% group_by(MD5,item,Type) %>% summarise(RTacross3words=mean(RT))

rt.data <- left_join(rt.data,ROIcombined[,c('MD5','item','Type','RTacross3words')])
#write.csv(rt.data,"N2000_allconditions_preprocessed.csv",row.names=F)
#write.csv(rt.data[rt.data$CONSTRUCTION%in%c("NPS","NPZ","MVRR"),],"ClassicGardenPathSet.csv",row.names=F)
#write.csv(rt.data[rt.data$CONSTRUCTION=="RelativeClause",],"RelativeClauseSet.csv",row.names=F)
#write.csv(rt.data[rt.data$CONSTRUCTION=="Attachment",],"AttachmentSet.csv",row.names=F)
#write.csv(rt.data[rt.data$CONSTRUCTION%in%c("NPZ","NP/Z Agreement"),],"AgreementSet.csv",row.names=F)
#write.csv(rt.data[rt.data$CONSTRUCTION%in%c("FILLER1","FILLER2"),],"Fillers.csv",row.names=F)