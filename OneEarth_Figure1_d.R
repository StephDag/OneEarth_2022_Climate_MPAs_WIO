# Aim : Figure 1d. Proportion of reef cells by country ranked by food security level

rm(list=ls())

# packages
library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(kableExtra)
library(formattable)
library(tidytext)
library(cowplot)

# load heatmap packages
library(devtools)
install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)

#####################
# Load Coral data   #
#####################

rm(CORAL)
CORAL <- readRDS(here("_data","OneEarth_CORAL_data.rds"))

#CORAL <- CORAL %>%
#  select(-Country.corrected) 

#################################################
# Load food security data Taylor et al., 2020   #
#################################################
rm(FS)
FS <- read.csv(here("_data","Food_sec_ctry.csv"),check.names = F)

# correct colnames
colnames(FS)[1] <- "TERRITORY.corrected"
colnames(FS)[6] <- "Fisheries.dependency.economy"
colnames(FS)[7] <- "Fisheries.dependency.employment"
colnames(FS)[8] <- "Fisheries.dependency.food.security"
colnames(FS)[12] <- "Fisheries.dependency.true.subsistence"
colnames(FS)[13] <- "Fisheries.dependency.true.subsistence.CAT"

# correct territory names
FS$TERRITORY.corrected <- plyr::revalue(FS$TERRITORY.corrected, c("R\x8eunion" = "Reunion",
                                                                  "British Indian Ocean Territory" = "Chagos Archipelago",
                                                                  "Comoro Islands" = "Comoros",
                                                                  "Ile Europa"="Europa Island"))

# modify country names: small french islands to "France", Zanzibar to "Tanzania
FS$Country.corrected <- c("Chagos Archipelago","Comoros","Djibouti","France",   
 "France","France","France","Kenya",              
 "Madagascar","Maldives","Mauritius","Mayotte",            
"Mozambique","Reunion","Seychelles","Somalia","South Africa","Tanzania","Tanzania")

# join food security data with main coral reef database
    rm(CORAL.V1)
    CORAL.V1 <- left_join(CORAL,FS,by=c("Country.corrected"="TERRITORY.corrected"))
    CORAL.V1$Country.corrected <- as.factor(CORAL.V1$Country.corrected)
    head(CORAL.V1)
    dim(CORAL.V1)
## dataframe - Percentage cells per climate change trajectories and countries
rm(coral_wide)
coral_wide <- CORAL.V1 %>%
      filter(Country.corrected != "") %>%
      dplyr::select(Country.corrected,CORAL.cat.final) %>%
      group_by(Country.corrected,CORAL.cat.final) %>%
      dplyr::summarize(n=n()) %>%
      mutate(perc.count = round(100*n/4222,2)) %>%
      as.data.frame() %>%
      drop_na(Country.corrected) %>%
      dplyr::select(-n) %>%
      spread(Country.corrected, perc.count)
    
## adapt coral_wide dataframe for circular plot
    coral_wide_circ <- coral_wide
    coral_wide_circ
## Table S2 -  Percentage cells per climate change trajectories and countries - replace NAs with 0
 coral_wide[is.na(coral_wide)] <- 0
 rownames(coral_wide) <- coral_wide$CORAL.cat.final
 coral_wide$CORAL.cat.final <- NULL
 coral_wide <- round(coral_wide,1)
 #coral_wide <- rbind(coral_wide,colSums(coral_wide))
 #rownames(coral_wide)[8] <- "Total"
 coral_wide

## order country by food security levels
rm(order.ctry)
order.ctry <- c(FS[which(FS$Fisheries.dependency.true.subsistence.CAT == "HIGH"),"Country.corrected"],
                FS[which(FS$Fisheries.dependency.true.subsistence.CAT == "MEDIUM"),"Country.corrected"],
                FS[which(FS$Fisheries.dependency.true.subsistence.CAT == "LOW"),"Country.corrected"],
                FS[which(is.na(FS$Fisheries.dependency.true.subsistence.CAT)),"Country.corrected"])
order.ctry <- order.ctry[!is.element(order.ctry, c("Maldives","Chagos Archipelago"))]
order.ctry <- c("Comoros","Madagascar","Tanzania",         
"Kenya","South Africa","Mauritius","Mozambique",        
"Seychelles","France","Mayotte","Reunion","Somalia")

## split group of countries based on food security level - HIGH, MEDIUM, LOW, NAs
rm(split)
split<- as.factor(c(rep("HIGH",3),rep("MEDIUM",2),rep("LOW",3),rep("V.LOW",3),rep("No data",1)))
split<- factor(split, levels = c("HIGH", "MEDIUM", "LOW","V.LOW","NA"))

  # crty.split 
fs <- c(rep("HIGH",3),rep("MEDIUM",2),rep("LOW",3),rep("V.LOW",3),rep("No data",1))
rm(ctry.split)
ctry.split <- cbind(order.ctry,fs)
ctry.split <- as.data.frame(ctry.split)
head(ctry.split)

  # order climate change trajectories
order.cat <- c("Source","Convergence","Corridors","Boundary sink","Divergence","Slow moving","Sink") 
coral_wide <- coral_wide[order.cat,order.ctry]
coral_wide_circ <- rownames_to_column(coral_wide)
colnames(coral_wide_circ)[1] <- "climate_cat"
coral_wide_circ

  # wide to long format for circular plot
rm(coral_wide_circ_long)
coral_wide_circ_long <- coral_wide_circ %>%
  gather(key="Country",value="Perc",-climate_cat)
coral_wide_circ_long <- left_join(coral_wide_circ_long,ctry.split,by=c("Country"="order.ctry"))
coral_wide_circ_long <- as.data.frame(coral_wide_circ_long)
coral_wide_circ_long$fs <- as.factor(coral_wide_circ_long$fs)
coral_wide_circ_long$Country <- as.factor(coral_wide_circ_long$Country)
coral_wide_circ_long$climate_cat <- as.factor(coral_wide_circ_long$climate_cat)
coral_wide_circ_long$fs <- factor(coral_wide_circ_long$fs,levels=c("HIGH","MEDIUM","LOW","V.LOW","No data"))
order.ctry <- c("Madagascar","Tanzania","Comoros","Kenya","South Africa","Mozambique","Seychelles","Mauritius","Mayotte","France","Reunion","Somalia")
coral_wide_circ_long$Country <- factor(coral_wide_circ_long$Country,levels=order.ctry)
coral_wide_circ_long <- coral_wide_circ_long %>% arrange(fs,Country,desc(Perc))

  data <- coral_wide_circ_long
  head(data)
  
#############################
# Figure 1d - Circulat plot #
#############################

rm(data)
data <- coral_wide_circ_long

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 1
nObsType <- nlevels(as.factor(data$climate_cat))

rm(to_add)
to_add <- data.frame(matrix(NA, empty_bar*nlevels(data$fs)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$fs <- rep(levels(data$fs), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(fs, Country)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
data$fs <- factor(data$fs, levels = c("HIGH", "MEDIUM", "LOW","V.LOW","No data"))
data$Country <- as.factor(data$Country)
data$climat_cat <- as.factor(data$climate_cat)
summary(data)

# Get the name and the y position of each label
rm(label_data)
label_data <- data %>% dplyr::group_by(fs,id, Country) %>% summarize(tot=sum(Perc)) %>%
  mutate(id = as.numeric(id))
number_of_bar <- nrow(label_data) 
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
tail(label_data)
# prepare a data frame for base lines
rm(base_data)
base_data <- data %>% 
  mutate(id = as.numeric(id)) %>%
  dplyr::group_by(fs) %>% 
  dplyr::summarize(start=min(id)- empty_bar/2, end=max(id) - empty_bar/2) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
#grid_data$start <- grid_data$start - 1
#grid_data <- grid_data[-1,]

# Make the plot
summary(data$Perc)
data <- data %>%
  mutate(id = as.numeric(id))

# Make the plot - color blind gradient
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rm(p)
p <- ggplot(data,aes(reorder_within(data,Country,Perc,fs),x=as.factor(id), y=Perc, fill=climate_cat)) +      
  # Add the stacked bar
  geom_bar(stat="identity", alpha=0.8) +
  #scale_fill_viridis(discrete=TRUE) +
  scale_fill_manual(values=okabe) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 30, xend = start, yend =30), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 10, 20, 30, 40), label = c("0", "10", "20", "30", "40") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-30,max(label_data$tot, na.rm=T)+10) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.85), # move legend to the center
    plot.margin = unit(c(-2,-1,-2,-2), "cm"),     # This remove unnecessary margin around plot
    legend.title.align = 0.5,
    legend.direction = "vertical",
    legend.box.just = "center",
    legend.text=element_text(size=12),
    legend.title=element_text(size=14)
  ) +
  coord_polar() +
  labs(fill='Climate change trajectories',size=12)  +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+0.5, label=Country, hjust=hjust), color="black", fontface="bold",alpha=1, size=8, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -1, xend = end, yend = -1), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE ) +
  geom_text(data=base_data, aes(x = title, y = -10, label=fs), hjust=c(0,0.3,0.5,1,0.5), colour = "black", alpha=0.8, size=6, fontface="bold", inherit.aes = FALSE) +
  theme(legend.text = element_text(size=28),legend.title = element_text(size=30,face="bold"),legend.position=c(0.35, 0.83)) 
p

# Save Figure 1d
ggsave(here("_Figures","Fig1d_CircularPlot_CTR.tiff"),plot=p, width = 14, height = 14)





