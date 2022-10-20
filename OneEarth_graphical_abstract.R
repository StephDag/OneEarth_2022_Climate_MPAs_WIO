#  Aim 1: Figures for graphical abstract

rm(list=ls())

# load packages
library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)

#####################
# Load Coral data   #
#####################
CORAL <- readRDS(here("_data","OneEarth_CORAL_data.rds"))
dim(CORAL)
head(CORAL)

summary(CORAL$CORAL.cat.final)

# change climate change categories with potential impacts
CORAL$cc.traj <- plyr::revalue(CORAL$CORAL.cat.final, c("Source" = "Biodiversity loss",
                                                 "Boundary sink" = "Biodiversity reshuffle",
                                                 "Convergence" = "Biodiversity reshuffle",
                                                 "Divergence"="Biodiversity reshuffle",
                                                 "Sink"="Biodiversity reshuffle",
                                                 "Corridors"="Climate corridors",
                                                 "Slow moving"="Climate refugia"))
CORAL$cc.traj <- as.factor(CORAL$cc.traj)

# select MPA
rm(MPA.cat)
MPA.cat <- CORAL %>% filter (PA_DEF == 1) %>% 
  filter(Country.corrected != "") %>%
  group_by(Country.corrected,name_2,cc.traj) %>%
  dplyr::summarize(count = n()) %>%
  mutate(count = round(100*count/sum(count),2)) %>%
  as.data.frame() %>% 
  filter(!is.na(cc.traj)) %>%
  droplevels()
head(MPA.cat)
as.factor(MPA.cat$name_2)

# number of MPA in the WIO - 82 MPAs
MPA.cat$name_2 %>% unique() %>% length()

# total # of cells
dim.CORAL.clean <- dim(CORAL)[1]

###################################################################################################
#      Figure 1a. Proportion of protected and unprotected reef cells in climate categories        #
###################################################################################################

# Dataframe summarizing the proportion of protected and unprotected coral reef cells in each climate change categories
rm(Cells.cat.MPA)
Cells.cat.MPA <- CORAL %>%
  group_by(cc.traj,PA_DEF) %>%
  dplyr::summarize(count = n()) %>%
  mutate(count = round(100*count/dim.CORAL.clean[1],2)) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  as_data_frame()
Cells.cat.MPA

# Figure 1a - Proportion of protected and unprotected coral reef cells in each climate change categories
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Cells.cat.MPA$alpha <- c("FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE")
Cells.cat.MPA$color <- c("Grey","Grey","#009E73","#009E73","#D55E00","#D55E00","#CC79A7","#CC79A7")

Cells.cat.Nairobi <- ggplot(data=Cells.cat.MPA,aes(x = reorder(cc.traj,-count), y = count)) + 
  geom_bar(position="stack",stat="identity",aes(fill=interaction(color,alpha),color=color,fill=color),alpha=c(0.5,1,0.5,1,0.5,1,0.5,1)) +
  scale_fill_manual(values=c("#009E73","#CC79A7","#D55E00","grey","#009E73","#CC79A7","#D55E00","grey"))+
  scale_color_manual(values=c("#009E73","#CC79A7","#D55E00","grey","#009E73","#CC79A7","#D55E00","grey"))+
  labs(x="Climate change potential impacts",y="Proportion of cells in the WIO (%)") +
  theme_bw() +
  ggpubr::rotate_x_text() +
  #scale_fill_manual(values=c("lightgrey","grey39"),name = "", labels = c("Open access", "MPA")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size=26,face="bold"),axis.text.y = element_text(size=26),
        axis.title.x = element_text(size = 28,face="bold"),axis.title.y = element_text(size = 28,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size=26)) +
  #labs(fill = "MPA") + 
  theme(legend.position = "none") 
Cells.cat.Nairobi 

ggsave(here("_Figures","GraphAbstract.Climate_Impacts.png"),Cells.cat.Nairobi,width=10,height=10,dpi=300)

