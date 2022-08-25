library(tidyr)
library(dplyr)
library(ggplot2)

noise.summary <- read.csv("noise_ceiling_wide.csv") %>%
  gather("Effect","Correlation",2:9) %>%
  rename("EOI" = Effect,
         "Ceiling" = Correlation)

GP.data <- readRDS("sampled_correlations_maxregion_GP.rds")
RC.data <- readRDS("sampled_correlations_maxregion_RC.rds")
AA.data <- readRDS("sampled_correlations_maxregion_AA.rds")
Agr.data <- readRDS("sampled_correlations_maxregion_Agr.rds")

all.data <- rbind(GP.data,RC.data,AA.data,Agr.data) %>% left_join(noise.summary)

cor.summary <- all.data %>%
                group_by(EOI,model) %>%
                summarize(
                  mean = mean(Correlation),
                  ceiling = mean(Ceiling),
                  lower.25 = quantile(Correlation,.25),
                  lower.10 = quantile(Correlation,.10),
                  lower.025 = quantile(Correlation,.025),
                  upper.75 = quantile(Correlation,.75),
                  upper.90 = quantile(Correlation,.90),
                  upper.975 = quantile(Correlation,.975)
                ) %>%
                rename("Model" = model)

cor.summary$EOI <- factor(cor.summary$EOI, levels = c("GPE_MVRR","GPE_NPS","GPE_NPZ","RC","GPE_high","GPE_low","Agr") )
cor.summary$Model <- factor(cor.summary$Model,levels = c("nosurp","lstm","gpt2") )



ggplot(cor.summary, aes(x=EOI, y=mean, color=Model))+
  geom_rect(inherit.aes=FALSE,
            aes(xmin=as.numeric(EOI)-.25,xmax=as.numeric(EOI)+0.25,ymin=0,ymax=ceiling), alpha=0.1)+
  geom_linerange(aes(ymin=lower.025,ymax=upper.975),lwd=.5,position = position_dodge(width=.5))+
  geom_linerange(aes(ymin=lower.25,ymax=upper.75),lwd=1.25,position = position_dodge(width=.5))+
  geom_point(lwd = 2.5,position = position_dodge(width=.5))+
  ylim(-.45,1)+ylab("Correlation between model predictions and data")+
  scale_color_manual(labels = c('No surprisal baseline','Wikitext LSTM','GPT-2'),values = c("Orange","DarkGreen","NavyBlue"))+
  scale_x_discrete(labels=c("MV/RR","NP/S","T/I","RC","HIGH","LOW","AGREE"))+theme_minimal()+theme(legend.position="bottom", legend.box = "horizontal")+
  geom_text(x=1.1, y=.95, size=3.5, family = 'sans',inherit.aes = FALSE, label="Explainable variance")+
  geom_segment(inherit.aes = FALSE, x = 1, y = 0.9, xend = 1, yend = .7, arrow = arrow(length = unit(0.2,"cm")))



