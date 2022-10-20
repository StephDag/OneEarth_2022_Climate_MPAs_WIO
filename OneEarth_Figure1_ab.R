# Aim 1: Proportion climate categories in reefs (protected and unprotected) (Figure 1a & Table 1)
# Aim 2: Number of MPAs in each climate category combination of climate categories (Figure 1b)

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
# select MPA
rm(MPA.cat)
MPA.cat <- CORAL %>% filter (PA_DEF == 1) %>% 
  filter(Country.corrected != "") %>%
  group_by(Country.corrected,name_2,CORAL.cat.final) %>%
  dplyr::summarize(count = n()) %>%
  mutate(count = round(100*count/sum(count),2)) %>%
  as.data.frame() %>% 
  filter(!is.na(CORAL.cat.final)) %>%
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
  group_by(CORAL.cat.final,PA_DEF) %>%
  dplyr::summarize(count = n()) %>%
  mutate(count = round(100*count/dim.CORAL.clean[1],2)) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  as_data_frame()
Cells.cat.MPA

# Figure 1a - Proportion of protected and unprotected coral reef cells in each climate change categories
Cells.cat.Nairobi <- ggplot(data=Cells.cat.MPA,aes(x = reorder(CORAL.cat.final,-count), y = count,fill=PA_DEF)) + 
  geom_bar(position="stack",stat="identity") +
  labs(x="Climate change categories",y="Proportion of cells in the WIO (%)") +
  theme_bw() +
  ggpubr::rotate_x_text() +
  scale_fill_manual(values=c("lightgrey","grey39"),name = "", labels = c("Open access", "MPA")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size=22,face="bold"),axis.text.y = element_text(size=22),
        axis.title.x = element_text(size = 26,face="bold"),axis.title.y = element_text(size = 26,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size=26)) +
  labs(fill = "MPA") + 
  theme(legend.position = c(0.80, 0.85),legend.direction = "vertical")  
Cells.cat.Nairobi 

###################################################################################################
#      Figure 1b.Combination of climate change categories in MPAs - no Maldives and Chagos        #
###################################################################################################

# Dataframe summarizing the number of MPAs and percentage of protected cells for each climate trajectory categories (or combinations of)
MPA.cat.wide.Nairobi %>% rm()
MPA.cat.wide.Nairobi <- MPA.cat %>% 
  group_by(Country.corrected,name_2) %>%
  dplyr::summarise(comb = paste(CORAL.cat.final, collapse=", ")) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(comb) %>%
  dplyr::summarize(n = n()) %>% 
  dplyr::mutate(perc = 100*round(n/82,3)) %>%
  as.data.frame() 

# check number of MPAs: should be 82 MPAs
sum(MPA.cat.wide.Nairobi$n)

# check percentage of cells = should be 100%
sum(MPA.cat.wide.Nairobi$perc) %>% round()

# Figure 1b - Climate change trajectories categories inside MPAs
MPA.cat.comb.Nairobi <- MPA.cat.wide.Nairobi %>%
  mutate(comb = fct_relevel(comb,"Source","Boundary sink","Corridors","Convergence",
                            "Divergence","Slow moving","Sink","Boundary sink, Source","Corridors, Source","Convergence, Source",
                            "Divergence, Source","Slow moving, Source",
                            "Convergence, Sink",
                            "Boundary sink, Slow moving, Source")) %>%
  ggplot(aes(x = comb, y = n)) + 
  geom_bar(stat="identity") +
  labs(x="Climate change categories in MPAs",y="Number of MPAs") +
  geom_text(aes(label = perc, x = comb, y = n), position = position_dodge(width = 0.5), vjust = 1.25,color="white",fontface = "bold") +
  theme_bw() +
  ggpubr::rotate_x_text()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size=22,face="bold"),axis.text.y = element_text(size=22),
        axis.title.x = element_text(size = 26,face="bold"),axis.title.y = element_text(size = 26,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
MPA.cat.comb.Nairobi 


# Figure 1a&b together
cells.cc <- ggpubr::ggarrange(Cells.cat.Nairobi , MPA.cat.comb.Nairobi, 
                      labels = c("A", "B"),ncol = 2, nrow = 1,align="hv",font.label = list(size = 20))
cells.cc
ggsave(here("_Figures","Fig.1ab_Climate_Combination_MPA_nonMPA_WIO.png"),cells.cc,width=16,height=12,dpi=300)

###########################################
#      Table 1.Proportion of cells        #
###########################################

## save proportion of cells in protected and unprotected coral reefs
rm(table.Cells.cat.MPA)
table.Cells.cat.MPA <- Cells.cat.MPA %>% spread(key=PA_DEF,value=count) %>% as.data.frame()
colnames(table.Cells.cat.MPA) <- c("Climate categories","Fished","MPA")
table.Cells.cat.MPA$Total <- round(rowSums(table.Cells.cat.MPA[,c(2:3)]),1)
table.Cells.cat.MPA <-rbind(table.Cells.cat.MPA,c("Total",round(colSums(table.Cells.cat.MPA[,c(2:3)]),1),"100"))
table.Cells.cat.MPA$Fished <- round(as.numeric(table.Cells.cat.MPA$Fished),1)
table.Cells.cat.MPA$MPA <- round(as.numeric(table.Cells.cat.MPA$MPA),1)

write.csv(table.Cells.cat.MPA,here("_Figures","Table1_Cells.cat.MPA.csv"))


