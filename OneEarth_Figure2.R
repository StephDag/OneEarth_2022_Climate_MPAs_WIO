# Aim : Figure 2 and Supplemental Figure S5

rm(list=ls())

# packages
library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggpubr)

#####################
# Load Coral data   #
#####################

rm(CORAL)
CORAL <- readRDS(here("_data","OneEarth_CORAL_data.rds"))
dim(CORAL)

#####################
# Load Fish data   #
#####################
rm(FISH)
FISH<- readRDS(here("_data","OneEarth_FISH_data.rds"))
dim(FISH)

###############
#    CORAL    #
###############

# Coral biodiversity hotspots matrix
  CORAL.V1.congr %>% rm()
  CORAL.V1.congr <- CORAL %>%
    # 99%
    mutate(hp.sp.99 = ifelse(Nb_sp > quantile(Nb_sp,.99,na.rm=T) ,1,0)) %>%
    mutate(hp.fric.99 = ifelse(FRic > quantile(FRic, .99,na.rm=T),1,0)) %>%
    mutate(hp.PD.99 = ifelse(PD > quantile(PD, .99,na.rm=T),1,0)) %>%
    mutate(congr_99 = ifelse((hp.sp.99+hp.fric.99+hp.PD.99) == 3,1,0)) %>%
    mutate(congr_99 = as.factor(congr_99)) %>%
    # 97.5%
    mutate(hp.sp.97 = ifelse(Nb_sp > quantile(Nb_sp,.975,na.rm=T) ,1,0)) %>%
    mutate(hp.fric.97 = ifelse(FRic > quantile(FRic, .975,na.rm=T),1,0)) %>%
    mutate(hp.PD.97 = ifelse(PD > quantile(PD, .975,na.rm=T),1,0)) %>%
    mutate(congr_97 = ifelse((hp.sp.97+hp.fric.97+hp.PD.97) == 3,1,0)) %>%
    mutate(congr_97 = as.factor(congr_97)) %>%
  # 95%
    mutate(hp.sp.95 = ifelse(Nb_sp > quantile(Nb_sp,.95,na.rm=T) ,1,0)) %>%
    mutate(hp.fric.95 = ifelse(FRic > quantile(FRic, .95,na.rm=T),1,0)) %>%
    mutate(hp.PD.95 = ifelse(PD > quantile(PD, .95,na.rm=T),1,0)) %>%
    mutate(congr_95 = ifelse((hp.sp.95+hp.fric.95+hp.PD.95) == 3,1,0)) %>%
    mutate(congr_95 = as.factor(congr_95)) %>%
    # 92.5%
    mutate(hp.sp.92 = ifelse(Nb_sp > quantile(Nb_sp,.925,na.rm=T) ,1,0)) %>%
    mutate(hp.fric.92 = ifelse(FRic > quantile(FRic, .925,na.rm=T),1,0)) %>%
    mutate(hp.PD.92 = ifelse(PD > quantile(PD, .925,na.rm=T),1,0)) %>%
    mutate(congr_92 = ifelse((hp.sp.92+hp.fric.92+hp.PD.92) == 3,1,0)) %>%
    mutate(congr_92 = as.factor(congr_92)) %>%
    # 90%
    mutate(hp.sp.90 = ifelse(Nb_sp >= quantile(Nb_sp,.9,na.rm=T),1,0)) %>%
    mutate(hp.fric.90 = ifelse(FRic >= quantile(FRic, .9,na.rm=T),1,0)) %>%
    mutate(hp.PD.90 = ifelse(PD >= quantile(PD, .9,na.rm=T),1,0)) %>%
    mutate(congr_90 = ifelse((hp.sp.90+hp.fric.90+hp.PD.90) == 3,1,0)) %>%
    mutate(congr_90 = as.factor(congr_90)) %>%
    # 87.5%
    mutate(hp.sp.87 = ifelse(Nb_sp >= quantile(Nb_sp,.875,na.rm=T),1,0)) %>%
    mutate(hp.fric.87 = ifelse(FRic >= quantile(FRic, .875,na.rm=T),1,0)) %>%
    mutate(hp.PD.87 = ifelse(PD >= quantile(PD, .875,na.rm=T),1,0)) %>%
    mutate(congr_87 = ifelse((hp.sp.87+hp.fric.87+hp.PD.87) == 3,1,0)) %>%
    mutate(congr_87 = as.factor(congr_87)) %>%
    # 85%
    mutate(hp.sp.85 = ifelse(Nb_sp >= quantile(Nb_sp,.85,na.rm=T),1,0)) %>%
    mutate(hp.fric.85 = ifelse(FRic >= quantile(FRic, .85,na.rm=T),1,0)) %>%
    mutate(hp.PD.85 = ifelse(PD >= quantile(PD, .85,na.rm=T),1,0)) %>%
    mutate(congr_85 = ifelse((hp.sp.85+hp.fric.85+hp.PD.85) == 3,1,0)) %>%
    mutate(congr_85 = as.factor(congr_85)) %>%
    # 82.5%
    mutate(hp.sp.82 = ifelse(Nb_sp >= quantile(Nb_sp,.825,na.rm=T),1,0)) %>%
    mutate(hp.fric.82 = ifelse(FRic >= quantile(FRic, .825,na.rm=T),1,0)) %>%
    mutate(hp.PD.82 = ifelse(PD >= quantile(PD, .825,na.rm=T),1,0)) %>%
    mutate(congr_82 = ifelse((hp.sp.82+hp.fric.82+hp.PD.82) == 3,1,0)) %>%
    mutate(congr_82 = as.factor(congr_82)) %>%
    # 80%
    mutate(hp.sp.80 = ifelse(Nb_sp >= quantile(Nb_sp,.8,na.rm=T),1,0)) %>%
    mutate(hp.fric.80 = ifelse(FRic >= quantile(FRic, .8,na.rm=T),1,0)) %>%
    mutate(hp.PD.80 = ifelse(PD >= quantile(PD, .8,na.rm=T),1,0)) %>%
    mutate(congr_80 = ifelse((hp.sp.80+hp.fric.80+hp.PD.80) == 3,1,0)) %>%
    mutate(congr_80 = as.factor(congr_80)) %>%
    # 77.5%
    mutate(hp.sp.77 = ifelse(Nb_sp >= quantile(Nb_sp,.775,na.rm=T),1,0)) %>%
    mutate(hp.fric.77 = ifelse(FRic >= quantile(FRic, .775,na.rm=T),1,0)) %>%
    mutate(hp.PD.77 = ifelse(PD >= quantile(PD, .775,na.rm=T),1,0)) %>%
    mutate(congr_77 = ifelse((hp.sp.77+hp.fric.77+hp.PD.77) == 3,1,0)) %>%
    mutate(congr_77 = as.factor(congr_77)) %>%
    # 75%
    mutate(hp.sp.75 = ifelse(Nb_sp >= quantile(Nb_sp,.75,na.rm=T),1,0)) %>%
    mutate(hp.fric.75 = ifelse(FRic >= quantile(FRic, .75,na.rm=T),1,0)) %>%
    mutate(hp.PD.75 = ifelse(PD >= quantile(PD, .75,na.rm=T),1,0)) %>%
    mutate(congr_75 = ifelse((hp.sp.75+hp.fric.75+hp.PD.75) == 3,1,0)) %>%
    mutate(congr_75 = as.factor(congr_75)) %>%
    # 72.5%
    mutate(hp.sp.72 = ifelse(Nb_sp >= quantile(Nb_sp,.725,na.rm=T),1,0)) %>%
    mutate(hp.fric.72 = ifelse(FRic >= quantile(FRic, .725,na.rm=T),1,0)) %>%
    mutate(hp.PD.72 = ifelse(PD >= quantile(PD, .725,na.rm=T),1,0)) %>%
    mutate(congr_72 = ifelse((hp.sp.72+hp.fric.72+hp.PD.72) == 3,1,0)) %>%
    mutate(congr_72 = as.factor(congr_72)) %>%
  # 70%
    mutate(hp.sp.70 = ifelse(Nb_sp >= quantile(Nb_sp,.7,na.rm=T),1,0)) %>%
    mutate(hp.fric.70 = ifelse(FRic >= quantile(FRic, .7,na.rm=T),1,0)) %>%
    mutate(hp.PD.70 = ifelse(PD >= quantile(PD, .7,na.rm=T),1,0)) %>%
    mutate(congr_70 = ifelse((hp.sp.70+hp.fric.70+hp.PD.70) == 3,1,0)) %>%
    mutate(congr_70 = as.factor(congr_70)) %>%
    # 67.5%
    mutate(hp.sp.67 = ifelse(Nb_sp >= quantile(Nb_sp,.675,na.rm=T),1,0)) %>%
    mutate(hp.fric.67 = ifelse(FRic >= quantile(FRic, .675,na.rm=T),1,0)) %>%
    mutate(hp.PD.67 = ifelse(PD >= quantile(PD, .675,na.rm=T),1,0)) %>%
    mutate(congr_67 = ifelse((hp.sp.67+hp.fric.67+hp.PD.67) == 3,1,0)) %>%
    mutate(congr_67 = as.factor(congr_67)) %>%
  # 65%
    mutate(hp.sp.65 = ifelse(Nb_sp >= quantile(Nb_sp,.65,na.rm=T),1,0)) %>%
    mutate(hp.fric.65 = ifelse(FRic >= quantile(FRic, .65,na.rm=T),1,0)) %>%
    mutate(hp.PD.65 = ifelse(PD >= quantile(PD, .65,na.rm=T),1,0)) %>%
    mutate(congr_65 = ifelse((hp.sp.65+hp.fric.65+hp.PD.65) == 3,1,0)) %>%
    mutate(congr_65 = as.factor(congr_65)) %>%
    # 62.5%
    mutate(hp.sp.62 = ifelse(Nb_sp >= quantile(Nb_sp,.625,na.rm=T),1,0)) %>%
    mutate(hp.fric.62 = ifelse(FRic >= quantile(FRic, .625,na.rm=T),1,0)) %>%
    mutate(hp.PD.62 = ifelse(PD >= quantile(PD, .625,na.rm=T),1,0)) %>%
    mutate(congr_62 = ifelse((hp.sp.62+hp.fric.62+hp.PD.62) == 3,1,0)) %>%
    mutate(congr_62 = as.factor(congr_62)) %>%
  # 60%
    mutate(hp.sp.60 = ifelse(Nb_sp >= quantile(Nb_sp,.6,na.rm=T),1,0)) %>%
    mutate(hp.fric.60 = ifelse(FRic >= quantile(FRic, .6,na.rm=T),1,0)) %>%
    mutate(hp.PD.60 = ifelse(PD >= quantile(PD, .6,na.rm=T),1,0)) %>%
    mutate(congr_60 = ifelse((hp.sp.60+hp.fric.60+hp.PD.60) == 3,1,0)) %>%
    mutate(congr_60 = as.factor(congr_60)) %>%
    # 57.5%
    mutate(hp.sp.57 = ifelse(Nb_sp >= quantile(Nb_sp,.575,na.rm=T),1,0)) %>%
    mutate(hp.fric.57 = ifelse(FRic >= quantile(FRic, .575,na.rm=T),1,0)) %>%
    mutate(hp.PD.57 = ifelse(PD >= quantile(PD, .575,na.rm=T),1,0)) %>%
    mutate(congr_57 = ifelse((hp.sp.57+hp.fric.57+hp.PD.57) == 3,1,0)) %>%
    mutate(congr_57 = as.factor(congr_57)) %>%
  # 55%
    mutate(hp.sp.55 = ifelse(Nb_sp >= quantile(Nb_sp,.55,na.rm=T),1,0)) %>%
    mutate(hp.fric.55 = ifelse(FRic >= quantile(FRic, .55,na.rm=T),1,0)) %>%
    mutate(hp.PD.55 = ifelse(PD >= quantile(PD, .55,na.rm=T),1,0)) %>%
    mutate(congr_55 = ifelse((hp.sp.55+hp.fric.55+hp.PD.55) == 3,1,0)) %>%
    mutate(congr_55 = as.factor(congr_55)) %>%
    # 52.5%
    mutate(hp.sp.52 = ifelse(Nb_sp >= quantile(Nb_sp,.525,na.rm=T),1,0)) %>%
    mutate(hp.fric.52 = ifelse(FRic >= quantile(FRic, .525,na.rm=T),1,0)) %>%
    mutate(hp.PD.52 = ifelse(PD >= quantile(PD, .525,na.rm=T),1,0)) %>%
    mutate(congr_52 = ifelse((hp.sp.52+hp.fric.52+hp.PD.52) == 3,1,0)) %>%
    mutate(congr_52 = as.factor(congr_52)) %>%
  # 50%
    mutate(hp.sp.50 = ifelse(Nb_sp >= quantile(Nb_sp,.5,na.rm=T),1,0)) %>%
    mutate(hp.fric.50 = ifelse(FRic >= quantile(FRic, .5,na.rm=T),1,0)) %>%
    mutate(hp.PD.50 = ifelse(PD >= quantile(PD, .5,na.rm=T),1,0)) %>%
    mutate(congr_50 = ifelse((hp.sp.50+hp.fric.50+hp.PD.50) == 3,1,0)) %>%
    mutate(congr_50 = as.factor(congr_50)) %>%
    # 47.5%
    mutate(hp.sp.47 = ifelse(Nb_sp >= quantile(Nb_sp,.475,na.rm=T),1,0)) %>%
    mutate(hp.fric.47 = ifelse(FRic >= quantile(FRic, .475,na.rm=T),1,0)) %>%
    mutate(hp.PD.47 = ifelse(PD >= quantile(PD, .475,na.rm=T),1,0)) %>%
    mutate(congr_47 = ifelse((hp.sp.47+hp.fric.47+hp.PD.47) == 3,1,0)) %>%
    mutate(congr_47 = as.factor(congr_47)) %>%
  # 45%
    mutate(hp.sp.45 = ifelse(Nb_sp >= quantile(Nb_sp,.45,na.rm=T),1,0)) %>%
    mutate(hp.fric.45 = ifelse(FRic >= quantile(FRic, .45,na.rm=T),1,0)) %>%
    mutate(hp.PD.45 = ifelse(PD >= quantile(PD, .45,na.rm=T),1,0)) %>%
    mutate(congr_45 = ifelse((hp.sp.45+hp.fric.45+hp.PD.45) == 3,1,0)) %>%
    mutate(congr_45 = as.factor(congr_45)) %>%
    # 42.5%
    mutate(hp.sp.42 = ifelse(Nb_sp >= quantile(Nb_sp,.425,na.rm=T),1,0)) %>%
    mutate(hp.fric.42 = ifelse(FRic >= quantile(FRic, .425,na.rm=T),1,0)) %>%
    mutate(hp.PD.42 = ifelse(PD >= quantile(PD, .425,na.rm=T),1,0)) %>%
    mutate(congr_42 = ifelse((hp.sp.42+hp.fric.42+hp.PD.42) == 3,1,0)) %>%
    mutate(congr_42 = as.factor(congr_42)) %>%
  # 40%
    mutate(hp.sp.40 = ifelse(Nb_sp >= quantile(Nb_sp,.4,na.rm=T),1,0)) %>%
    mutate(hp.fric.40 = ifelse(FRic >= quantile(FRic, .4,na.rm=T),1,0)) %>%
    mutate(hp.PD.40 = ifelse(PD >= quantile(PD, .4,na.rm=T),1,0)) %>%
    mutate(congr_40 = ifelse((hp.sp.40+hp.fric.40+hp.PD.40) == 3,1,0)) %>%
    mutate(congr_40 = as.factor(congr_40)) %>%
    # 37.5%
    mutate(hp.sp.37 = ifelse(Nb_sp >= quantile(Nb_sp,.375,na.rm=T),1,0)) %>%
    mutate(hp.fric.37 = ifelse(FRic >= quantile(FRic, .375,na.rm=T),1,0)) %>%
    mutate(hp.PD.37 = ifelse(PD >= quantile(PD, .375,na.rm=T),1,0)) %>%
    mutate(congr_37 = ifelse((hp.sp.37+hp.fric.37+hp.PD.37) == 3,1,0)) %>%
    mutate(congr_37 = as.factor(congr_37)) %>%
  # 35%
    mutate(hp.sp.35 = ifelse(Nb_sp >= quantile(Nb_sp,.35,na.rm=T),1,0)) %>%
    mutate(hp.fric.35 = ifelse(FRic >= quantile(FRic, .35,na.rm=T),1,0)) %>%
    mutate(hp.PD.35 = ifelse(PD >= quantile(PD, .35,na.rm=T),1,0)) %>%
    mutate(congr_35 = ifelse((hp.sp.35+hp.fric.35+hp.PD.35) == 3,1,0)) %>%
    mutate(congr_35 = as.factor(congr_35)) %>%
    # 32.5%
    mutate(hp.sp.32 = ifelse(Nb_sp >= quantile(Nb_sp,.325,na.rm=T),1,0)) %>%
    mutate(hp.fric.32 = ifelse(FRic >= quantile(FRic, .325,na.rm=T),1,0)) %>%
    mutate(hp.PD.32 = ifelse(PD >= quantile(PD, .325,na.rm=T),1,0)) %>%
    mutate(congr_32 = ifelse((hp.sp.32+hp.fric.32+hp.PD.32) == 3,1,0)) %>%
    mutate(congr_32 = as.factor(congr_32)) %>%
  # 30%
    mutate(hp.sp.30 = ifelse(Nb_sp >= quantile(Nb_sp,.3,na.rm=T),1,0)) %>%
      mutate(hp.fric.30 = ifelse(FRic >= quantile(FRic, .3,na.rm=T),1,0)) %>%
      mutate(hp.PD.30 = ifelse(PD >= quantile(PD, .3,na.rm=T),1,0)) %>%
      mutate(congr_30 = ifelse((hp.sp.30+hp.fric.30+hp.PD.30) == 3,1,0)) %>%
      mutate(congr_30 = as.factor(congr_30)) %>%
    # 27.5%
    mutate(hp.sp.27 = ifelse(Nb_sp >= quantile(Nb_sp,.275,na.rm=T),1,0)) %>%
    mutate(hp.fric.27 = ifelse(FRic >= quantile(FRic, .275,na.rm=T),1,0)) %>%
    mutate(hp.PD.27 = ifelse(PD >= quantile(PD, .275,na.rm=T),1,0)) %>%
    mutate(congr_27 = ifelse((hp.sp.27+hp.fric.27+hp.PD.27) == 3,1,0)) %>%
    mutate(congr_27 = as.factor(congr_27)) %>%
  # 25%
    mutate(hp.sp.25 = ifelse(Nb_sp >= quantile(Nb_sp,.25,na.rm=T),1,0)) %>%
    mutate(hp.fric.25 = ifelse(FRic >= quantile(FRic, .25,na.rm=T),1,0)) %>%
    mutate(hp.PD.25 = ifelse(PD >= quantile(PD, .25,na.rm=T),1,0)) %>%
    mutate(congr_25 = ifelse((hp.sp.25+hp.fric.25+hp.PD.25) == 3,1,0)) %>%
    mutate(congr_25 = as.factor(congr_25)) %>%
    # 22.5%
    mutate(hp.sp.22 = ifelse(Nb_sp >= quantile(Nb_sp,.225,na.rm=T),1,0)) %>%
    mutate(hp.fric.22 = ifelse(FRic >= quantile(FRic, .225,na.rm=T),1,0)) %>%
    mutate(hp.PD.22 = ifelse(PD >= quantile(PD, .225,na.rm=T),1,0)) %>%
    mutate(congr_22 = ifelse((hp.sp.22+hp.fric.22+hp.PD.22) == 3,1,0)) %>%
    mutate(congr_22 = as.factor(congr_22)) %>%
  # 20%
    mutate(hp.sp.20 = ifelse(Nb_sp >= quantile(Nb_sp,.2,na.rm=T),1,0)) %>%
      mutate(hp.fric.20 = ifelse(FRic >= quantile(FRic, .2,na.rm=T),1,0)) %>%
      mutate(hp.PD.20 = ifelse(PD >= quantile(PD, .2,na.rm=T),1,0)) %>%
      mutate(congr_20 = ifelse((hp.sp.20+hp.fric.20+hp.PD.20) == 3,1,0)) %>%
      mutate(congr_20 = as.factor(congr_20)) %>%
    # 17.5%
    mutate(hp.sp.17 = ifelse(Nb_sp >= quantile(Nb_sp,.175,na.rm=T),1,0)) %>%
    mutate(hp.fric.17 = ifelse(FRic >= quantile(FRic, .175,na.rm=T),1,0)) %>%
    mutate(hp.PD.17 = ifelse(PD >= quantile(PD, .175,na.rm=T),1,0)) %>%
    mutate(congr_17 = ifelse((hp.sp.17+hp.fric.17+hp.PD.17) == 3,1,0)) %>%
    mutate(congr_17 = as.factor(congr_17)) %>%
  # 15%
    mutate(hp.sp.15 = ifelse(Nb_sp >= quantile(Nb_sp,.15,na.rm=T),1,0)) %>%
    mutate(hp.fric.15 = ifelse(FRic >= quantile(FRic, .15,na.rm=T),1,0)) %>%
    mutate(hp.PD.15 = ifelse(PD >= quantile(PD, .15,na.rm=T),1,0)) %>%
    mutate(congr_15 = ifelse((hp.sp.15+hp.fric.15+hp.PD.15) == 3,1,0)) %>%
    mutate(congr_15 = as.factor(congr_15)) %>%
    # 125.5%
    mutate(hp.sp.12 = ifelse(Nb_sp >= quantile(Nb_sp,.125,na.rm=T),1,0)) %>%
    mutate(hp.fric.12 = ifelse(FRic >= quantile(FRic, .125,na.rm=T),1,0)) %>%
    mutate(hp.PD.12 = ifelse(PD >= quantile(PD, .125,na.rm=T),1,0)) %>%
    mutate(congr_12 = ifelse((hp.sp.12+hp.fric.12+hp.PD.12) == 3,1,0)) %>%
    mutate(congr_12 = as.factor(congr_12)) %>%
  # 10%
    mutate(hp.sp.10 = ifelse(Nb_sp >= quantile(Nb_sp,.1,na.rm=T),1,0)) %>%
      mutate(hp.fric.10 = ifelse(FRic >= quantile(FRic, .1,na.rm=T),1,0)) %>%
      mutate(hp.PD.10 = ifelse(PD >= quantile(PD, .1,na.rm=T),1,0)) %>%
      mutate(congr_10 = ifelse((hp.sp.10+hp.fric.10+hp.PD.10) == 3,1,0)) %>%
      mutate(congr_10 = as.factor(congr_10)) %>%
    # 7.5%
    mutate(hp.sp.07 = ifelse(Nb_sp >= quantile(Nb_sp,.075,na.rm=T),1,0)) %>%
    mutate(hp.fric.07 = ifelse(FRic >= quantile(FRic, .075,na.rm=T),1,0)) %>%
    mutate(hp.PD.07 = ifelse(PD >= quantile(PD, .075,na.rm=T),1,0)) %>%
    mutate(congr_07 = ifelse((hp.sp.07+hp.fric.07+hp.PD.07) == 3,1,0)) %>%
    mutate(congr_07 = as.factor(congr_07)) %>%
  # 5%
    mutate(hp.sp.05 = ifelse(Nb_sp >= quantile(Nb_sp,.05,na.rm=T),1,0)) %>%
    mutate(hp.fric.05 = ifelse(FRic >= quantile(FRic, .05,na.rm=T),1,0)) %>%
    mutate(hp.PD.05 = ifelse(PD >= quantile(PD, .05,na.rm=T),1,0)) %>%
    mutate(congr_05 = ifelse((hp.sp.05+hp.fric.05+hp.PD.05) == 3,1,0)) %>%
    mutate(congr_05 = as.factor(congr_05)) %>%
    # 02.5%
    mutate(hp.sp.02 = ifelse(Nb_sp >= quantile(Nb_sp,.025,na.rm=T),1,0)) %>%
    mutate(hp.fric.02 = ifelse(FRic >= quantile(FRic, .025,na.rm=T),1,0)) %>%
    mutate(hp.PD.02 = ifelse(PD >= quantile(PD, .025,na.rm=T),1,0)) %>%
    mutate(congr_02 = ifelse((hp.sp.02+hp.fric.02+hp.PD.02) == 3,1,0)) %>%
    mutate(congr_02 = as.factor(congr_02)) %>%
  # 0%
    mutate(hp.sp.00 = ifelse(Nb_sp >= quantile(Nb_sp,.0,na.rm=T),1,0)) %>%
      mutate(hp.fric.00 = ifelse(FRic >= quantile(FRic, .0,na.rm=T),1,0)) %>%
      mutate(hp.PD.00 = ifelse(PD >= quantile(PD, .0,na.rm=T),1,0)) %>%
      mutate(congr_00 = ifelse((hp.sp.00+hp.fric.00+hp.PD.00) == 3,1,0)) %>%
      mutate(congr_00 = as.factor(congr_00))
    
  CORAL.V1.congr %>% summary()
  CORAL.V1.congr %>% names()
  CORAL.V1.congr %>% dim()
  
  # Species richness hotspots
  rm(sum.congr.S.MPA)
  sum.congr.S.MPA <- CORAL.V1.congr %>% dplyr::select(hp.sp.99,hp.sp.97,hp.sp.95,hp.sp.92,hp.sp.90,hp.sp.87,hp.sp.85,hp.sp.82,hp.sp.80,hp.sp.77,hp.sp.75,hp.sp.72,hp.sp.70,hp.sp.67,
                                                      hp.sp.65,hp.sp.62,hp.sp.60,hp.sp.57,hp.sp.55,hp.sp.52,hp.sp.50,hp.sp.47,hp.sp.45,hp.sp.42,hp.sp.40,hp.sp.37,hp.sp.35,hp.sp.32,
                                                      hp.sp.30,hp.sp.27,hp.sp.25,hp.sp.22,hp.sp.20,hp.sp.17,hp.sp.15,hp.sp.12,hp.sp.10,hp.sp.07,hp.sp.05,hp.sp.02,hp.sp.00,PA_DEF) %>%
    gather(key="quant",value="perc",-PA_DEF) %>%
    mutate(perc = as.factor(perc)) %>%
    mutate(quant = as.factor(quant)) %>%
    filter(perc==1)  %>%
    group_by(quant,PA_DEF) %>%
    dplyr::summarize(count.sp = n()) %>%
    dplyr::mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
    ungroup() %>%
    filter(PA_DEF==1)  %>%
    mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
    mutate(biodiv.index = rep("S",length(perc.cell))) %>%
    as.data.frame()

  #### FRic hotspots
  rm(sum.congr.fric.MPA)
  sum.congr.fric.MPA <- CORAL.V1.congr %>% dplyr::select(hp.fric.99,hp.fric.97,hp.fric.95,hp.fric.92,hp.fric.90,hp.fric.87,hp.fric.85,hp.fric.82,hp.fric.80,hp.fric.77,hp.fric.75,hp.fric.72,hp.fric.70,hp.fric.67,
                                                     hp.fric.65,hp.fric.62,hp.fric.60,hp.fric.57,hp.fric.55,hp.fric.52,hp.fric.50,hp.fric.47,hp.fric.45,hp.fric.42,hp.fric.40,hp.fric.37,hp.fric.35,hp.fric.32,
                                                     hp.fric.30,hp.fric.27,hp.fric.25,hp.fric.22,hp.fric.20,hp.fric.17,hp.fric.15,hp.fric.12,hp.fric.10,hp.fric.07,hp.fric.05,hp.fric.02,hp.fric.00,PA_DEF) %>%
    gather(key="quant",value="perc",-PA_DEF) %>%
    mutate(perc = as.factor(perc)) %>%
    mutate(quant = as.factor(quant)) %>%
    filter(perc==1)  %>%
    group_by(quant,PA_DEF) %>%
    dplyr::summarize(count.sp = n()) %>%
    dplyr::mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
    ungroup() %>%
    filter(PA_DEF==1)  %>%
    mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
    mutate(biodiv.index = rep("FRic",length(perc.cell))) %>%
    as.data.frame()

  ##### Phylogenetic diverrsity hotspots
  rm(sum.congr.PD.MPA)
  sum.congr.PD.MPA <- CORAL.V1.congr %>% dplyr::select(hp.PD.99,hp.PD.97,hp.PD.95,hp.PD.92,hp.PD.90,hp.PD.87,hp.PD.85,hp.PD.82,hp.PD.80,hp.PD.77,hp.PD.75,hp.PD.72,hp.PD.70,hp.PD.67,
                                                      hp.PD.65,hp.PD.62,hp.PD.60,hp.PD.57,hp.PD.55,hp.PD.52,hp.PD.50,hp.PD.47,hp.PD.45,hp.PD.42,hp.PD.40,hp.PD.37,hp.PD.35,hp.PD.32,
                                                      hp.PD.30,hp.PD.27,hp.PD.25,hp.PD.22,hp.PD.20,hp.PD.17,hp.PD.15,hp.PD.12,hp.PD.10,hp.PD.07,hp.PD.05,hp.PD.02,hp.PD.00,PA_DEF) %>%
    gather(key="quant",value="perc",-PA_DEF) %>%
    mutate(perc = as.factor(perc)) %>%
    mutate(quant = as.factor(quant)) %>%
    filter(perc==1)  %>%
    group_by(quant,PA_DEF) %>%
    dplyr::summarize(count.sp = n()) %>%
    dplyr::mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
    ungroup() %>%
    filter(PA_DEF==1)  %>%
    mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
    mutate(biodiv.index = rep("PD",length(perc.cell))) %>%
    as.data.frame()

  ##### 3 index together
  rm(sum.congr.all.MPA)
  sum.congr.all.MPA <- CORAL.V1.congr %>% dplyr::select(congr_99,congr_97,congr_95,congr_92,congr_90,congr_87,congr_85,congr_82,congr_80,congr_77,congr_75,congr_72,congr_70,congr_67,
                                                        congr_65,congr_62,congr_60,congr_57,congr_55,congr_52,congr_50,congr_47,congr_45,congr_42,congr_40,congr_37,congr_35,congr_32,
                                                        congr_30,congr_27,congr_25,congr_22,congr_20,congr_17,congr_15,congr_12,congr_10,congr_07,congr_05,congr_02,congr_00,PA_DEF) %>%
    gather(key="quant",value="perc",-PA_DEF) %>%
    mutate(perc = as.factor(perc)) %>%
    mutate(quant = as.factor(quant)) %>%
    filter(perc==1)  %>%
    group_by(quant,PA_DEF) %>%
    dplyr::summarize(count.sp = n()) %>%
    mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
    ungroup() %>%
    filter(PA_DEF==1)  %>%
    mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
    mutate(biodiv.index = rep("All",length(perc.cell))) %>%
    as.data.frame()

# full dataframe for COTRAL biodiverrsity hotspots
  rm(all.biodiv.coral)
  all.biodiv.coral <- rbind(sum.congr.S.MPA,sum.congr.fric.MPA,sum.congr.PD.MPA,sum.congr.all.MPA)
  all.biodiv.coral$taxa <- rep("Coral",dim(all.biodiv.coral)[1])
  all.biodiv.coral$biodiv.index <- as.factor(all.biodiv.coral$biodiv.index)
  all.biodiv.coral$biodiv.index <- factor(all.biodiv.coral$biodiv.index, levels = c("S","FRic","PD","All"))

###############
#     FISH    #
###############

# Fish biodiversity hotspots matrix
FISH.V1.congr %>% rm()
FISH.V1.congr <- FISH   %>%
  # 99%
  mutate(hp.sp.99 = ifelse(Nb_sp > quantile(Nb_sp,.99,na.rm=T) ,1,0)) %>%
  mutate(hp.fric.99 = ifelse(FRic > quantile(FRic, .99,na.rm=T),1,0)) %>%
  mutate(hp.PD.99 = ifelse(phylo_PD > quantile(phylo_PD, .99,na.rm=T),1,0)) %>%
  mutate(congr_99 = ifelse((hp.sp.99+hp.fric.99+hp.PD.99) == 3,1,0)) %>%
  mutate(congr_99 = as.factor(congr_99)) %>%
  # 97.5%
  mutate(hp.sp.97 = ifelse(Nb_sp > quantile(Nb_sp,.975,na.rm=T) ,1,0)) %>%
  mutate(hp.fric.97 = ifelse(FRic > quantile(FRic, .975,na.rm=T),1,0)) %>%
  mutate(hp.PD.97 = ifelse(phylo_PD > quantile(phylo_PD, .975,na.rm=T),1,0)) %>%
  mutate(congr_97 = ifelse((hp.sp.97+hp.fric.97+hp.PD.97) == 3,1,0)) %>%
  mutate(congr_97 = as.factor(congr_97)) %>%
  # 95%
  mutate(hp.sp.95 = ifelse(Nb_sp > quantile(Nb_sp,.95,na.rm=T) ,1,0)) %>%
  mutate(hp.fric.95 = ifelse(FRic > quantile(FRic, .95,na.rm=T),1,0)) %>%
  mutate(hp.PD.95 = ifelse(phylo_PD > quantile(phylo_PD, .95,na.rm=T),1,0)) %>%
  mutate(congr_95 = ifelse((hp.sp.95+hp.fric.95+hp.PD.95) == 3,1,0)) %>%
  mutate(congr_95 = as.factor(congr_95)) %>%
  # 92.5%
  mutate(hp.sp.92 = ifelse(Nb_sp > quantile(Nb_sp,.925,na.rm=T) ,1,0)) %>%
  mutate(hp.fric.92 = ifelse(FRic > quantile(FRic, .925,na.rm=T),1,0)) %>%
  mutate(hp.PD.92 = ifelse(phylo_PD > quantile(phylo_PD, .925,na.rm=T),1,0)) %>%
  mutate(congr_92 = ifelse((hp.sp.92+hp.fric.92+hp.PD.92) == 3,1,0)) %>%
  mutate(congr_92 = as.factor(congr_92)) %>%
  # 90%
  mutate(hp.sp.90 = ifelse(Nb_sp >= quantile(Nb_sp,.9,na.rm=T),1,0)) %>%
  mutate(hp.fric.90 = ifelse(FRic >= quantile(FRic, .9,na.rm=T),1,0)) %>%
  mutate(hp.PD.90 = ifelse(phylo_PD >= quantile(phylo_PD, .9,na.rm=T),1,0)) %>%
  mutate(congr_90 = ifelse((hp.sp.90+hp.fric.90+hp.PD.90) == 3,1,0)) %>%
  mutate(congr_90 = as.factor(congr_90)) %>%
  # 87.5%
  mutate(hp.sp.87 = ifelse(Nb_sp >= quantile(Nb_sp,.875,na.rm=T),1,0)) %>%
  mutate(hp.fric.87 = ifelse(FRic >= quantile(FRic, .875,na.rm=T),1,0)) %>%
  mutate(hp.PD.87 = ifelse(phylo_PD >= quantile(phylo_PD, .875,na.rm=T),1,0)) %>%
  mutate(congr_87 = ifelse((hp.sp.87+hp.fric.87+hp.PD.87) == 3,1,0)) %>%
  mutate(congr_87 = as.factor(congr_87)) %>%
  # 85%
  mutate(hp.sp.85 = ifelse(Nb_sp >= quantile(Nb_sp,.85,na.rm=T),1,0)) %>%
  mutate(hp.fric.85 = ifelse(FRic >= quantile(FRic, .85,na.rm=T),1,0)) %>%
  mutate(hp.PD.85 = ifelse(phylo_PD >= quantile(phylo_PD, .85,na.rm=T),1,0)) %>%
  mutate(congr_85 = ifelse((hp.sp.85+hp.fric.85+hp.PD.85) == 3,1,0)) %>%
  mutate(congr_85 = as.factor(congr_85)) %>%
  # 82.5%
  mutate(hp.sp.82 = ifelse(Nb_sp >= quantile(Nb_sp,.825,na.rm=T),1,0)) %>%
  mutate(hp.fric.82 = ifelse(FRic >= quantile(FRic, .825,na.rm=T),1,0)) %>%
  mutate(hp.PD.82 = ifelse(phylo_PD >= quantile(phylo_PD, .825,na.rm=T),1,0)) %>%
  mutate(congr_82 = ifelse((hp.sp.82+hp.fric.82+hp.PD.82) == 3,1,0)) %>%
  mutate(congr_82 = as.factor(congr_82)) %>%
  # 80%
  mutate(hp.sp.80 = ifelse(Nb_sp >= quantile(Nb_sp,.8,na.rm=T),1,0)) %>%
  mutate(hp.fric.80 = ifelse(FRic >= quantile(FRic, .8,na.rm=T),1,0)) %>%
  mutate(hp.PD.80 = ifelse(phylo_PD >= quantile(phylo_PD, .8,na.rm=T),1,0)) %>%
  mutate(congr_80 = ifelse((hp.sp.80+hp.fric.80+hp.PD.80) == 3,1,0)) %>%
  mutate(congr_80 = as.factor(congr_80)) %>%
  # 77.5%
  mutate(hp.sp.77 = ifelse(Nb_sp >= quantile(Nb_sp,.775,na.rm=T),1,0)) %>%
  mutate(hp.fric.77 = ifelse(FRic >= quantile(FRic, .775,na.rm=T),1,0)) %>%
  mutate(hp.PD.77 = ifelse(phylo_PD >= quantile(phylo_PD, .775,na.rm=T),1,0)) %>%
  mutate(congr_77 = ifelse((hp.sp.77+hp.fric.77+hp.PD.77) == 3,1,0)) %>%
  mutate(congr_77 = as.factor(congr_77)) %>%
  # 75%
  mutate(hp.sp.75 = ifelse(Nb_sp >= quantile(Nb_sp,.75,na.rm=T),1,0)) %>%
  mutate(hp.fric.75 = ifelse(FRic >= quantile(FRic, .75,na.rm=T),1,0)) %>%
  mutate(hp.PD.75 = ifelse(phylo_PD >= quantile(phylo_PD, .75,na.rm=T),1,0)) %>%
  mutate(congr_75 = ifelse((hp.sp.75+hp.fric.75+hp.PD.75) == 3,1,0)) %>%
  mutate(congr_75 = as.factor(congr_75)) %>%
  # 72.5%
  mutate(hp.sp.72 = ifelse(Nb_sp >= quantile(Nb_sp,.725,na.rm=T),1,0)) %>%
  mutate(hp.fric.72 = ifelse(FRic >= quantile(FRic, .725,na.rm=T),1,0)) %>%
  mutate(hp.PD.72 = ifelse(phylo_PD >= quantile(phylo_PD, .725,na.rm=T),1,0)) %>%
  mutate(congr_72 = ifelse((hp.sp.72+hp.fric.72+hp.PD.72) == 3,1,0)) %>%
  mutate(congr_72 = as.factor(congr_72)) %>%
  # 70%
  mutate(hp.sp.70 = ifelse(Nb_sp >= quantile(Nb_sp,.7,na.rm=T),1,0)) %>%
  mutate(hp.fric.70 = ifelse(FRic >= quantile(FRic, .7,na.rm=T),1,0)) %>%
  mutate(hp.PD.70 = ifelse(phylo_PD >= quantile(phylo_PD, .7,na.rm=T),1,0)) %>%
  mutate(congr_70 = ifelse((hp.sp.70+hp.fric.70+hp.PD.70) == 3,1,0)) %>%
  mutate(congr_70 = as.factor(congr_70)) %>%
  # 67.5%
  mutate(hp.sp.67 = ifelse(Nb_sp >= quantile(Nb_sp,.675,na.rm=T),1,0)) %>%
  mutate(hp.fric.67 = ifelse(FRic >= quantile(FRic, .675,na.rm=T),1,0)) %>%
  mutate(hp.PD.67 = ifelse(phylo_PD >= quantile(phylo_PD, .675,na.rm=T),1,0)) %>%
  mutate(congr_67 = ifelse((hp.sp.67+hp.fric.67+hp.PD.67) == 3,1,0)) %>%
  mutate(congr_67 = as.factor(congr_67)) %>%
  # 65%
  mutate(hp.sp.65 = ifelse(Nb_sp >= quantile(Nb_sp,.65,na.rm=T),1,0)) %>%
  mutate(hp.fric.65 = ifelse(FRic >= quantile(FRic, .65,na.rm=T),1,0)) %>%
  mutate(hp.PD.65 = ifelse(phylo_PD >= quantile(phylo_PD, .65,na.rm=T),1,0)) %>%
  mutate(congr_65 = ifelse((hp.sp.65+hp.fric.65+hp.PD.65) == 3,1,0)) %>%
  mutate(congr_65 = as.factor(congr_65)) %>%
  # 62.5%
  mutate(hp.sp.62 = ifelse(Nb_sp >= quantile(Nb_sp,.625,na.rm=T),1,0)) %>%
  mutate(hp.fric.62 = ifelse(FRic >= quantile(FRic, .625,na.rm=T),1,0)) %>%
  mutate(hp.PD.62 = ifelse(phylo_PD >= quantile(phylo_PD, .625,na.rm=T),1,0)) %>%
  mutate(congr_62 = ifelse((hp.sp.62+hp.fric.62+hp.PD.62) == 3,1,0)) %>%
  mutate(congr_62 = as.factor(congr_62)) %>%
  # 60%
  mutate(hp.sp.60 = ifelse(Nb_sp >= quantile(Nb_sp,.6,na.rm=T),1,0)) %>%
  mutate(hp.fric.60 = ifelse(FRic >= quantile(FRic, .6,na.rm=T),1,0)) %>%
  mutate(hp.PD.60 = ifelse(phylo_PD >= quantile(phylo_PD, .6,na.rm=T),1,0)) %>%
  mutate(congr_60 = ifelse((hp.sp.60+hp.fric.60+hp.PD.60) == 3,1,0)) %>%
  mutate(congr_60 = as.factor(congr_60)) %>%
  # 57.5%
  mutate(hp.sp.57 = ifelse(Nb_sp >= quantile(Nb_sp,.575,na.rm=T),1,0)) %>%
  mutate(hp.fric.57 = ifelse(FRic >= quantile(FRic, .575,na.rm=T),1,0)) %>%
  mutate(hp.PD.57 = ifelse(phylo_PD >= quantile(phylo_PD, .575,na.rm=T),1,0)) %>%
  mutate(congr_57 = ifelse((hp.sp.57+hp.fric.57+hp.PD.57) == 3,1,0)) %>%
  mutate(congr_57 = as.factor(congr_57)) %>%
  # 55%
  mutate(hp.sp.55 = ifelse(Nb_sp >= quantile(Nb_sp,.55,na.rm=T),1,0)) %>%
  mutate(hp.fric.55 = ifelse(FRic >= quantile(FRic, .55,na.rm=T),1,0)) %>%
  mutate(hp.PD.55 = ifelse(phylo_PD >= quantile(phylo_PD, .55,na.rm=T),1,0)) %>%
  mutate(congr_55 = ifelse((hp.sp.55+hp.fric.55+hp.PD.55) == 3,1,0)) %>%
  mutate(congr_55 = as.factor(congr_55)) %>%
  # 52.5%
  mutate(hp.sp.52 = ifelse(Nb_sp >= quantile(Nb_sp,.525,na.rm=T),1,0)) %>%
  mutate(hp.fric.52 = ifelse(FRic >= quantile(FRic, .525,na.rm=T),1,0)) %>%
  mutate(hp.PD.52 = ifelse(phylo_PD >= quantile(phylo_PD, .525,na.rm=T),1,0)) %>%
  mutate(congr_52 = ifelse((hp.sp.52+hp.fric.52+hp.PD.52) == 3,1,0)) %>%
  mutate(congr_52 = as.factor(congr_52)) %>%
  # 50%
  mutate(hp.sp.50 = ifelse(Nb_sp >= quantile(Nb_sp,.5,na.rm=T),1,0)) %>%
  mutate(hp.fric.50 = ifelse(FRic >= quantile(FRic, .5,na.rm=T),1,0)) %>%
  mutate(hp.PD.50 = ifelse(phylo_PD >= quantile(phylo_PD, .5,na.rm=T),1,0)) %>%
  mutate(congr_50 = ifelse((hp.sp.50+hp.fric.50+hp.PD.50) == 3,1,0)) %>%
  mutate(congr_50 = as.factor(congr_50)) %>%
  # 47.5%
  mutate(hp.sp.47 = ifelse(Nb_sp >= quantile(Nb_sp,.475,na.rm=T),1,0)) %>%
  mutate(hp.fric.47 = ifelse(FRic >= quantile(FRic, .475,na.rm=T),1,0)) %>%
  mutate(hp.PD.47 = ifelse(phylo_PD >= quantile(phylo_PD, .475,na.rm=T),1,0)) %>%
  mutate(congr_47 = ifelse((hp.sp.47+hp.fric.47+hp.PD.47) == 3,1,0)) %>%
  mutate(congr_47 = as.factor(congr_47)) %>%
  # 45%
  mutate(hp.sp.45 = ifelse(Nb_sp >= quantile(Nb_sp,.45,na.rm=T),1,0)) %>%
  mutate(hp.fric.45 = ifelse(FRic >= quantile(FRic, .45,na.rm=T),1,0)) %>%
  mutate(hp.PD.45 = ifelse(phylo_PD >= quantile(phylo_PD, .45,na.rm=T),1,0)) %>%
  mutate(congr_45 = ifelse((hp.sp.45+hp.fric.45+hp.PD.45) == 3,1,0)) %>%
  mutate(congr_45 = as.factor(congr_45)) %>%
  # 42.5%
  mutate(hp.sp.42 = ifelse(Nb_sp >= quantile(Nb_sp,.425,na.rm=T),1,0)) %>%
  mutate(hp.fric.42 = ifelse(FRic >= quantile(FRic, .425,na.rm=T),1,0)) %>%
  mutate(hp.PD.42 = ifelse(phylo_PD >= quantile(phylo_PD, .425,na.rm=T),1,0)) %>%
  mutate(congr_42 = ifelse((hp.sp.42+hp.fric.42+hp.PD.42) == 3,1,0)) %>%
  mutate(congr_42 = as.factor(congr_42)) %>%
  # 40%
  mutate(hp.sp.40 = ifelse(Nb_sp >= quantile(Nb_sp,.4,na.rm=T),1,0)) %>%
  mutate(hp.fric.40 = ifelse(FRic >= quantile(FRic, .4,na.rm=T),1,0)) %>%
  mutate(hp.PD.40 = ifelse(phylo_PD >= quantile(phylo_PD, .4,na.rm=T),1,0)) %>%
  mutate(congr_40 = ifelse((hp.sp.40+hp.fric.40+hp.PD.40) == 3,1,0)) %>%
  mutate(congr_40 = as.factor(congr_40)) %>%
  # 37.5%
  mutate(hp.sp.37 = ifelse(Nb_sp >= quantile(Nb_sp,.375,na.rm=T),1,0)) %>%
  mutate(hp.fric.37 = ifelse(FRic >= quantile(FRic, .375,na.rm=T),1,0)) %>%
  mutate(hp.PD.37 = ifelse(phylo_PD >= quantile(phylo_PD, .375,na.rm=T),1,0)) %>%
  mutate(congr_37 = ifelse((hp.sp.37+hp.fric.37+hp.PD.37) == 3,1,0)) %>%
  mutate(congr_37 = as.factor(congr_37)) %>%
  # 35%
  mutate(hp.sp.35 = ifelse(Nb_sp >= quantile(Nb_sp,.35,na.rm=T),1,0)) %>%
  mutate(hp.fric.35 = ifelse(FRic >= quantile(FRic, .35,na.rm=T),1,0)) %>%
  mutate(hp.PD.35 = ifelse(phylo_PD >= quantile(phylo_PD, .35,na.rm=T),1,0)) %>%
  mutate(congr_35 = ifelse((hp.sp.35+hp.fric.35+hp.PD.35) == 3,1,0)) %>%
  mutate(congr_35 = as.factor(congr_35)) %>%
  # 32.5%
  mutate(hp.sp.32 = ifelse(Nb_sp >= quantile(Nb_sp,.325,na.rm=T),1,0)) %>%
  mutate(hp.fric.32 = ifelse(FRic >= quantile(FRic, .325,na.rm=T),1,0)) %>%
  mutate(hp.PD.32 = ifelse(phylo_PD >= quantile(phylo_PD, .325,na.rm=T),1,0)) %>%
  mutate(congr_32 = ifelse((hp.sp.32+hp.fric.32+hp.PD.32) == 3,1,0)) %>%
  mutate(congr_32 = as.factor(congr_32)) %>%
  # 30%
  mutate(hp.sp.30 = ifelse(Nb_sp >= quantile(Nb_sp,.3,na.rm=T),1,0)) %>%
  mutate(hp.fric.30 = ifelse(FRic >= quantile(FRic, .3,na.rm=T),1,0)) %>%
  mutate(hp.PD.30 = ifelse(phylo_PD >= quantile(phylo_PD, .3,na.rm=T),1,0)) %>%
  mutate(congr_30 = ifelse((hp.sp.30+hp.fric.30+hp.PD.30) == 3,1,0)) %>%
  mutate(congr_30 = as.factor(congr_30)) %>%
  # 27.5%
  mutate(hp.sp.27 = ifelse(Nb_sp >= quantile(Nb_sp,.275,na.rm=T),1,0)) %>%
  mutate(hp.fric.27 = ifelse(FRic >= quantile(FRic, .275,na.rm=T),1,0)) %>%
  mutate(hp.PD.27 = ifelse(phylo_PD >= quantile(phylo_PD, .275,na.rm=T),1,0)) %>%
  mutate(congr_27 = ifelse((hp.sp.27+hp.fric.27+hp.PD.27) == 3,1,0)) %>%
  mutate(congr_27 = as.factor(congr_27)) %>%
  # 25%
  mutate(hp.sp.25 = ifelse(Nb_sp >= quantile(Nb_sp,.25,na.rm=T),1,0)) %>%
  mutate(hp.fric.25 = ifelse(FRic >= quantile(FRic, .25,na.rm=T),1,0)) %>%
  mutate(hp.PD.25 = ifelse(phylo_PD >= quantile(phylo_PD, .25,na.rm=T),1,0)) %>%
  mutate(congr_25 = ifelse((hp.sp.25+hp.fric.25+hp.PD.25) == 3,1,0)) %>%
  mutate(congr_25 = as.factor(congr_25)) %>%
  # 22.5%
  mutate(hp.sp.22 = ifelse(Nb_sp >= quantile(Nb_sp,.225,na.rm=T),1,0)) %>%
  mutate(hp.fric.22 = ifelse(FRic >= quantile(FRic, .225,na.rm=T),1,0)) %>%
  mutate(hp.PD.22 = ifelse(phylo_PD >= quantile(phylo_PD, .225,na.rm=T),1,0)) %>%
  mutate(congr_22 = ifelse((hp.sp.22+hp.fric.22+hp.PD.22) == 3,1,0)) %>%
  mutate(congr_22 = as.factor(congr_22)) %>%
  # 20%
  mutate(hp.sp.20 = ifelse(Nb_sp >= quantile(Nb_sp,.2,na.rm=T),1,0)) %>%
  mutate(hp.fric.20 = ifelse(FRic >= quantile(FRic, .2,na.rm=T),1,0)) %>%
  mutate(hp.PD.20 = ifelse(phylo_PD >= quantile(phylo_PD, .2,na.rm=T),1,0)) %>%
  mutate(congr_20 = ifelse((hp.sp.20+hp.fric.20+hp.PD.20) == 3,1,0)) %>%
  mutate(congr_20 = as.factor(congr_20)) %>%
  # 17.5%
  mutate(hp.sp.17 = ifelse(Nb_sp >= quantile(Nb_sp,.175,na.rm=T),1,0)) %>%
  mutate(hp.fric.17 = ifelse(FRic >= quantile(FRic, .175,na.rm=T),1,0)) %>%
  mutate(hp.PD.17 = ifelse(phylo_PD >= quantile(phylo_PD, .175,na.rm=T),1,0)) %>%
  mutate(congr_17 = ifelse((hp.sp.17+hp.fric.17+hp.PD.17) == 3,1,0)) %>%
  mutate(congr_17 = as.factor(congr_17)) %>%
  # 15%
  mutate(hp.sp.15 = ifelse(Nb_sp >= quantile(Nb_sp,.15,na.rm=T),1,0)) %>%
  mutate(hp.fric.15 = ifelse(FRic >= quantile(FRic, .15,na.rm=T),1,0)) %>%
  mutate(hp.PD.15 = ifelse(phylo_PD >= quantile(phylo_PD, .15,na.rm=T),1,0)) %>%
  mutate(congr_15 = ifelse((hp.sp.15+hp.fric.15+hp.PD.15) == 3,1,0)) %>%
  mutate(congr_15 = as.factor(congr_15)) %>%
  # 125.5%
  mutate(hp.sp.12 = ifelse(Nb_sp >= quantile(Nb_sp,.125,na.rm=T),1,0)) %>%
  mutate(hp.fric.12 = ifelse(FRic >= quantile(FRic, .125,na.rm=T),1,0)) %>%
  mutate(hp.PD.12 = ifelse(phylo_PD >= quantile(phylo_PD, .125,na.rm=T),1,0)) %>%
  mutate(congr_12 = ifelse((hp.sp.12+hp.fric.12+hp.PD.12) == 3,1,0)) %>%
  mutate(congr_12 = as.factor(congr_12)) %>%
  # 10%
  mutate(hp.sp.10 = ifelse(Nb_sp >= quantile(Nb_sp,.1,na.rm=T),1,0)) %>%
  mutate(hp.fric.10 = ifelse(FRic >= quantile(FRic, .1,na.rm=T),1,0)) %>%
  mutate(hp.PD.10 = ifelse(phylo_PD >= quantile(phylo_PD, .1,na.rm=T),1,0)) %>%
  mutate(congr_10 = ifelse((hp.sp.10+hp.fric.10+hp.PD.10) == 3,1,0)) %>%
  mutate(congr_10 = as.factor(congr_10)) %>%
  # 7.5%
  mutate(hp.sp.07 = ifelse(Nb_sp >= quantile(Nb_sp,.075,na.rm=T),1,0)) %>%
  mutate(hp.fric.07 = ifelse(FRic >= quantile(FRic, .075,na.rm=T),1,0)) %>%
  mutate(hp.PD.07 = ifelse(phylo_PD >= quantile(phylo_PD, .075,na.rm=T),1,0)) %>%
  mutate(congr_07 = ifelse((hp.sp.07+hp.fric.07+hp.PD.07) == 3,1,0)) %>%
  mutate(congr_07 = as.factor(congr_07)) %>%
  # 5%
  mutate(hp.sp.05 = ifelse(Nb_sp >= quantile(Nb_sp,.05,na.rm=T),1,0)) %>%
  mutate(hp.fric.05 = ifelse(FRic >= quantile(FRic, .05,na.rm=T),1,0)) %>%
  mutate(hp.PD.05 = ifelse(phylo_PD >= quantile(phylo_PD, .05,na.rm=T),1,0)) %>%
  mutate(congr_05 = ifelse((hp.sp.05+hp.fric.05+hp.PD.05) == 3,1,0)) %>%
  mutate(congr_05 = as.factor(congr_05)) %>%
  # 02.5%
  mutate(hp.sp.02 = ifelse(Nb_sp >= quantile(Nb_sp,.025,na.rm=T),1,0)) %>%
  mutate(hp.fric.02 = ifelse(FRic >= quantile(FRic, .025,na.rm=T),1,0)) %>%
  mutate(hp.PD.02 = ifelse(phylo_PD >= quantile(phylo_PD, .025,na.rm=T),1,0)) %>%
  mutate(congr_02 = ifelse((hp.sp.02+hp.fric.02+hp.PD.02) == 3,1,0)) %>%
  mutate(congr_02 = as.factor(congr_02)) %>%
  # 0%
  mutate(hp.sp.00 = ifelse(Nb_sp >= quantile(Nb_sp,.0,na.rm=T),1,0)) %>%
  mutate(hp.fric.00 = ifelse(FRic >= quantile(FRic, .0,na.rm=T),1,0)) %>%
  mutate(hp.PD.00 = ifelse(phylo_PD >= quantile(phylo_PD, .0,na.rm=T),1,0)) %>%
  mutate(congr_00 = ifelse((hp.sp.00+hp.fric.00+hp.PD.00) == 3,1,0)) %>%
  mutate(congr_00 = as.factor(congr_00))

#### Species richness hotspots
rm(sum.congr.S.MPA.fish)
sum.congr.S.MPA.fish <- FISH.V1.congr %>% dplyr::select(hp.sp.99,hp.sp.97,hp.sp.95,hp.sp.92,hp.sp.90,hp.sp.87,hp.sp.85,hp.sp.82,hp.sp.80,hp.sp.77,hp.sp.75,hp.sp.72,hp.sp.70,hp.sp.67,
                                                    hp.sp.65,hp.sp.62,hp.sp.60,hp.sp.57,hp.sp.55,hp.sp.52,hp.sp.50,hp.sp.47,hp.sp.45,hp.sp.42,hp.sp.40,hp.sp.37,hp.sp.35,hp.sp.32,
                                                    hp.sp.30,hp.sp.27,hp.sp.25,hp.sp.22,hp.sp.20,hp.sp.17,hp.sp.15,hp.sp.12,hp.sp.10,hp.sp.07,hp.sp.05,hp.sp.02,hp.sp.00,PA_DEF) %>%
  gather(key="quant",value="perc",-PA_DEF) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1)  %>%
  group_by(quant,PA_DEF) %>%
  dplyr::summarize(count.sp = n()) %>%
  dplyr::mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  ungroup() %>%
  filter(PA_DEF==1)  %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("S",length(perc.cell))) %>%
  as.data.frame()

#### FRic hotspots
rm(sum.congr.fric.MPA.fish)
sum.congr.fric.MPA.fish <- FISH.V1.congr %>% dplyr::select(hp.fric.99,hp.fric.97,hp.fric.95,hp.fric.92,hp.fric.90,hp.fric.87,hp.fric.85,hp.fric.82,hp.fric.80,hp.fric.77,hp.fric.75,hp.fric.72,hp.fric.70,hp.fric.67,
                                                       hp.fric.65,hp.fric.62,hp.fric.60,hp.fric.57,hp.fric.55,hp.fric.52,hp.fric.50,hp.fric.47,hp.fric.45,hp.fric.42,hp.fric.40,hp.fric.37,hp.fric.35,hp.fric.32,
                                                       hp.fric.30,hp.fric.27,hp.fric.25,hp.fric.22,hp.fric.20,hp.fric.17,hp.fric.15,hp.fric.12,hp.fric.10,hp.fric.07,hp.fric.05,hp.fric.02,hp.fric.00,PA_DEF) %>%
  gather(key="quant",value="perc",-PA_DEF) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1)  %>%
  group_by(quant,PA_DEF) %>%
  dplyr::summarize(count.sp = n()) %>%
  dplyr::mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  ungroup() %>%
  filter(PA_DEF==1)  %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("FRic",length(perc.cell))) %>%
  as.data.frame()

#### Phylogenetic diversity hotspots
rm(sum.congr.PD.MPA.fish)
sum.congr.PD.MPA.fish <- FISH.V1.congr  %>% dplyr::select(hp.PD.99,hp.PD.97,hp.PD.95,hp.PD.92,hp.PD.90,hp.PD.87,hp.PD.85,hp.PD.82,hp.PD.80,hp.PD.77,hp.PD.75,hp.PD.72,hp.PD.70,hp.PD.67,
                                                          hp.PD.65,hp.PD.62,hp.PD.60,hp.PD.57,hp.PD.55,hp.PD.52,hp.PD.50,hp.PD.47,hp.PD.45,hp.PD.42,hp.PD.40,hp.PD.37,hp.PD.35,hp.PD.32,
                                                          hp.PD.30,hp.PD.27,hp.PD.25,hp.PD.22,hp.PD.20,hp.PD.17,hp.PD.15,hp.PD.12,hp.PD.10,hp.PD.07,hp.PD.05,hp.PD.02,hp.PD.00,PA_DEF) %>%
  gather(key="quant",value="perc",-PA_DEF) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1)  %>%
  group_by(quant,PA_DEF) %>%
  dplyr::summarize(count.sp = n()) %>%
  dplyr::mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  ungroup() %>%
  filter(PA_DEF==1)  %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("PD",length(perc.cell))) %>%
  as.data.frame()

##### all
rm(sum.congr.all.MPA.fish)
sum.congr.all.MPA.fish <- FISH.V1.congr %>% dplyr::select(congr_99,congr_97,congr_95,congr_92,congr_90,congr_87,congr_85,congr_82,congr_80,congr_77,congr_75,congr_72,congr_70,congr_67,
                                                          congr_65,congr_62,congr_60,congr_57,congr_55,congr_52,congr_50,congr_47,congr_45,congr_42,congr_40,congr_37,congr_35,congr_32,
                                                          congr_30,congr_27,congr_25,congr_22,congr_20,congr_17,congr_15,congr_12,congr_10,congr_07,congr_05,congr_02,congr_00,PA_DEF) %>%
  gather(key="quant",value="perc",-PA_DEF) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1)  %>%
  group_by(quant,PA_DEF) %>%
  dplyr::summarize(count.sp = n()) %>%
  dplyr::mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  ungroup() %>%
  filter(PA_DEF==1)  %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("All",length(perc.cell))) %>%
  as.data.frame()

# Full Fish dataframe
rm(all.biodiv.fish)
all.biodiv.fish <- rbind(sum.congr.S.MPA.fish,sum.congr.fric.MPA.fish,sum.congr.PD.MPA.fish,sum.congr.all.MPA.fish)
all.biodiv.fish$taxa <- rep("Fish",dim(all.biodiv.fish)[1])
all.biodiv.fish$biodiv.index <- as.factor(all.biodiv.fish$biodiv.index)
all.biodiv.fish$biodiv.index <- factor(all.biodiv.fish$biodiv.index, levels = c("S","FRic","PD","All"))

# Full Coral and Fish dataframe
rm(all.biodiv)
all.biodiv <- rbind(all.biodiv.coral,all.biodiv.fish) %>% as.data.frame()
all.biodiv$taxa <- as.factor(all.biodiv$taxa)
all.biodiv$biodiv.index <- as.factor(all.biodiv$biodiv.index)
all.biodiv$biodiv.index <- factor(all.biodiv$biodiv.index, levels = c("S","FRic","PD","All"))

################################
#         Figure 2             #
################################

  #### CORAL by climate change categories

# Species richness
rm(sum.congr.S.MPA.cc)
sum.congr.S.MPA.cc <- CORAL.V1.congr %>% dplyr::select(hp.sp.99,hp.sp.97,hp.sp.95,hp.sp.92,hp.sp.90,hp.sp.87,hp.sp.85,hp.sp.82,hp.sp.80,hp.sp.77,hp.sp.75,hp.sp.72,hp.sp.70,hp.sp.67,
                                                    hp.sp.65,hp.sp.62,hp.sp.60,hp.sp.57,hp.sp.55,hp.sp.52,hp.sp.50,hp.sp.47,hp.sp.45,hp.sp.42,hp.sp.40,hp.sp.37,hp.sp.35,hp.sp.32,
                                                    hp.sp.30,hp.sp.27,hp.sp.25,hp.sp.22,hp.sp.20,hp.sp.17,hp.sp.15,hp.sp.12,hp.sp.10,hp.sp.07,hp.sp.05,hp.sp.02,hp.sp.00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("S",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

#### Functional richness
rm(sum.congr.fric.MPA.cc)
sum.congr.fric.MPA.cc <- CORAL.V1.congr %>% dplyr::select(hp.fric.99,hp.fric.97,hp.fric.95,hp.fric.92,hp.fric.90,hp.fric.87,hp.fric.85,hp.fric.82,hp.fric.80,hp.fric.77,hp.fric.75,hp.fric.72,hp.fric.70,hp.fric.67,
                                                       hp.fric.65,hp.fric.62,hp.fric.60,hp.fric.57,hp.fric.55,hp.fric.52,hp.fric.50,hp.fric.47,hp.fric.45,hp.fric.42,hp.fric.40,hp.fric.37,hp.fric.35,hp.fric.32,
                                                       hp.fric.30,hp.fric.27,hp.fric.25,hp.fric.22,hp.fric.20,hp.fric.17,hp.fric.15,hp.fric.12,hp.fric.10,hp.fric.07,hp.fric.05,hp.fric.02,hp.fric.00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("FRic",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

##### Phylogenetic diversity
rm(sum.congr.PD.MPA.cc)
sum.congr.PD.MPA.cc <- CORAL.V1.congr %>% dplyr::select(hp.PD.99,hp.PD.97,hp.PD.95,hp.PD.92,hp.PD.90,hp.PD.87,hp.PD.85,hp.PD.82,hp.PD.80,hp.PD.77,hp.PD.75,hp.PD.72,hp.PD.70,hp.PD.67,
                                                     hp.PD.65,hp.PD.62,hp.PD.60,hp.PD.57,hp.PD.55,hp.PD.52,hp.PD.50,hp.PD.47,hp.PD.45,hp.PD.42,hp.PD.40,hp.PD.37,hp.PD.35,hp.PD.32,
                                                     hp.PD.30,hp.PD.27,hp.PD.25,hp.PD.22,hp.PD.20,hp.PD.17,hp.PD.15,hp.PD.12,hp.PD.10,hp.PD.07,hp.PD.05,hp.PD.02,hp.PD.00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("PD",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

##### Coral - all biodiversity components
rm(sum.congr.all.MPA.cc)
sum.congr.all.MPA.cc <- CORAL.V1.congr %>% dplyr::select(congr_99,congr_97,congr_95,congr_92,congr_90,congr_87,congr_85,congr_82,congr_80,congr_77,congr_75,congr_72,congr_70,congr_67,
                                                      congr_65,congr_62,congr_60,congr_57,congr_55,congr_52,congr_50,congr_47,congr_45,congr_42,congr_40,congr_37,congr_35,congr_32,
                                                      congr_30,congr_27,congr_25,congr_22,congr_20,congr_17,congr_15,congr_12,congr_10,congr_07,congr_05,congr_02,congr_00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("All",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

#### FISH
# Species richness
rm(sum.congr.S.MPA.fish.cc)
sum.congr.S.MPA.fish.cc <- FISH.V1.congr %>% dplyr::select(hp.sp.99,hp.sp.97,hp.sp.95,hp.sp.92,hp.sp.90,hp.sp.87,hp.sp.85,hp.sp.82,hp.sp.80,hp.sp.77,hp.sp.75,hp.sp.72,hp.sp.70,hp.sp.67,
                                                       hp.sp.65,hp.sp.62,hp.sp.60,hp.sp.57,hp.sp.55,hp.sp.52,hp.sp.50,hp.sp.47,hp.sp.45,hp.sp.42,hp.sp.40,hp.sp.37,hp.sp.35,hp.sp.32,
                                                       hp.sp.30,hp.sp.27,hp.sp.25,hp.sp.22,hp.sp.20,hp.sp.17,hp.sp.15,hp.sp.12,hp.sp.10,hp.sp.07,hp.sp.05,hp.sp.02,hp.sp.00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("S",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

#### Functional richness
rm(sum.congr.fric.MPA.fish.cc)
sum.congr.fric.MPA.fish.cc <- FISH.V1.congr %>% dplyr::select(hp.fric.99,hp.fric.97,hp.fric.95,hp.fric.92,hp.fric.90,hp.fric.87,hp.fric.85,hp.fric.82,hp.fric.80,hp.fric.77,hp.fric.75,hp.fric.72,hp.fric.70,hp.fric.67,
                                                          hp.fric.65,hp.fric.62,hp.fric.60,hp.fric.57,hp.fric.55,hp.fric.52,hp.fric.50,hp.fric.47,hp.fric.45,hp.fric.42,hp.fric.40,hp.fric.37,hp.fric.35,hp.fric.32,
                                                          hp.fric.30,hp.fric.27,hp.fric.25,hp.fric.22,hp.fric.20,hp.fric.17,hp.fric.15,hp.fric.12,hp.fric.10,hp.fric.07,hp.fric.05,hp.fric.02,hp.fric.00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("FRic",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

##### Phylogenetic diversity
rm(sum.congr.PD.MPA.fish.cc)
sum.congr.PD.MPA.fish.cc <- FISH.V1.congr %>% dplyr::select(hp.PD.99,hp.PD.97,hp.PD.95,hp.PD.92,hp.PD.90,hp.PD.87,hp.PD.85,hp.PD.82,hp.PD.80,hp.PD.77,hp.PD.75,hp.PD.72,hp.PD.70,hp.PD.67,
                                                        hp.PD.65,hp.PD.62,hp.PD.60,hp.PD.57,hp.PD.55,hp.PD.52,hp.PD.50,hp.PD.47,hp.PD.45,hp.PD.42,hp.PD.40,hp.PD.37,hp.PD.35,hp.PD.32,
                                                        hp.PD.30,hp.PD.27,hp.PD.25,hp.PD.22,hp.PD.20,hp.PD.17,hp.PD.15,hp.PD.12,hp.PD.10,hp.PD.07,hp.PD.05,hp.PD.02,hp.PD.00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("PD",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

##### Fish - all biodiversity components
rm(sum.congr.all.MPA.fish.cc)
sum.congr.all.MPA.fish.cc <- FISH.V1.congr %>% dplyr::select(congr_99,congr_97,congr_95,congr_92,congr_90,congr_87,congr_85,congr_82,congr_80,congr_77,congr_75,congr_72,congr_70,congr_67,
                                                         congr_65,congr_62,congr_60,congr_57,congr_55,congr_52,congr_50,congr_47,congr_45,congr_42,congr_40,congr_37,congr_35,congr_32,
                                                         congr_30,congr_27,congr_25,congr_22,congr_20,congr_17,congr_15,congr_12,congr_10,congr_07,congr_05,congr_02,congr_00,PA_DEF,CORAL.cat.final) %>%
  filter(!is.na(CORAL.cat.final)) %>%
  gather(key="quant",value="perc",-PA_DEF,-CORAL.cat.final) %>%
  mutate(perc = as.factor(perc)) %>%
  mutate(quant = as.factor(quant)) %>%
  filter(perc==1 & PA_DEF==1)  %>%
  group_by(quant,PA_DEF,CORAL.cat.final) %>%
  dplyr::summarize(count.sp = n()) %>%
  mutate(freq = 100*round(count.sp/sum(count.sp),2)) %>%
  mutate(prob = freq*0.28) %>%
  mutate(PA_DEF = as.factor(PA_DEF)) %>%
  ungroup() %>%
  tidyr::complete(quant,nesting(PA_DEF,CORAL.cat.final), fill = list(count.sp=0,freq=0,prob=0)) %>%
  mutate(perc.cell = 100-as.numeric(str_sub(quant, start= -2))) %>%
  mutate(biodiv.index = rep("All",length(CORAL.cat.final))) %>%
  dplyr::select(-quant,-PA_DEF,-count.sp) %>%
  as.data.frame()

All.fish <- ggplot(sum.congr.all.MPA.fish.cc,aes(x=perc.cell,y=prob,colour=CORAL.cat.final)) +
  geom_line() +
  geom_abline(intercept=4,slope=0) +
  theme_bw()

#  Coral and Fish - all biodiversity components
  # coral
rm(all.biodiv.coral.cc)
all.biodiv.coral.cc <- rbind(sum.congr.S.MPA.cc,sum.congr.fric.MPA.cc,sum.congr.PD.MPA.cc,sum.congr.all.MPA.cc)
all.biodiv.coral.cc$taxa <- rep("Coral",dim(all.biodiv.coral.cc)[1])
all.biodiv.coral.cc$biodiv.index <- as.factor(all.biodiv.coral.cc$biodiv.index)
all.biodiv.coral.cc$biodiv.index <- factor(all.biodiv.coral.cc$biodiv.index, levels = c("S","FRic","PD","All"))
names(all.biodiv.coral.cc)[1] <- "cc.cat.final"
# fish
rm(all.biodiv.fish.cc)
all.biodiv.fish.cc <- rbind(sum.congr.S.MPA.fish.cc,sum.congr.fric.MPA.fish.cc,sum.congr.PD.MPA.fish.cc,sum.congr.all.MPA.fish.cc)
all.biodiv.fish.cc$taxa <- rep("Fish",dim(all.biodiv.fish.cc)[1])
all.biodiv.fish.cc$biodiv.index <- as.factor(all.biodiv.fish.cc$biodiv.index)
all.biodiv.fish.cc$biodiv.index <- factor(all.biodiv.fish.cc$biodiv.index, levels = c("S","FRic","PD","All"))
names(all.biodiv.fish.cc)[1] <- "cc.cat.final"
# all coral and fish together
rm(all.biodiv.cc)
all.biodiv.cc <- rbind(all.biodiv.coral.cc,all.biodiv.fish.cc) %>% as.data.frame()
all.biodiv.cc$taxa <- as.factor(all.biodiv.cc$taxa)
all.biodiv.cc$biodiv.index <- as.factor(all.biodiv.cc$biodiv.index)
all.biodiv.cc$biodiv.index <- factor(all.biodiv.cc$biodiv.index, levels = c("S","FRic","PD","All"))

#### Figure 2
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(egg)
rm(all.biodiv.CORAL.FISH.cc)
all.biodiv.CORAL.FISH.cc <- ggplot(all.biodiv.cc,aes(x=perc.cell,y=prob,colour=cc.cat.final)) +
  #geom_point() +
  geom_line(size=3) +
  geom_abline(slope=0,intercept=4) +
  scale_color_manual(values=okabe) +
  ylim(0,30) +
  facet_grid(taxa~ biodiv.index) +
  theme_bw()+
  guides(col=guide_legend("Climate change trajectories")) +
  labs(x = "Decreasing biodiversity gradients (%)",y=("Proportion of reefs in protected area (%)")) +
  theme(legend.position='bottom', 
      legend.justification='left',
      legend.direction='horizontal',
      axis.text.x = element_text(size=24),axis.text.y = element_text(size=24),
      axis.title.x = element_text(size = 28,face="bold"),axis.title.y = element_text(size = 28,face="bold"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.text = element_text(size=24),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y = element_text(size=26),
      legend.title = element_text(size=26,face="bold")) +
  scale_x_continuous("Decreasing biodiversity gradients (%)",breaks=c(0,10,25,50,75,100),labels=c("100","90","75","50","25","0")) +
  geom_vline(xintercept=10,color = "black",linetype="dashed",size=1.5)


all.biodiv.CORAL.FISH.cc <-  tag_facet(all.biodiv.CORAL.FISH.cc,open="",close="",tag_pool = LETTERS,size=10)
all.biodiv.CORAL.FISH.cc

ggsave(plot=all.biodiv.CORAL.FISH.cc,filename=here("_Figures","Fig2_Biodiversity_hotspots_CC_trajectories.jpeg"),width=22,height=16,units="in",dpi=300)

##############################
#   Supplemental Figure S5   #
##############################

# color
library(fishualize)
fishualize(n = 3, option = "Odonus_niger", end = 0.8)
col <- c(fish(3, option = "Odonus_niger", end = 0.8),"black")

# coral
all.biodiv.CORAL <- ggplot(all.biodiv.coral,aes(x=perc.cell,y=freq,colour=biodiv.index)) +
  #geom_point() +
  geom_line(size=3) +
  scale_color_manual(values=col) +
  geom_abline(slope=0,intercept=28) +
  xlab("Decreasing biodiversity gradient (%)") + ylab("Proportion of reefs in MPAs (%)") +
  ylim(0,100) +
  ggtitle("Coral") +
  theme_bw() +
  guides(col=guide_legend("Biodiversity Index")) +
  theme(legend.position='bottom', 
        legend.justification='left',
        legend.direction='horizontal') +
  theme_bw()+
  labs(x = "Decreasing biodiversity gradients (%)",y=("Proportion of reefs in protected area (%)")) +
  theme(legend.position='bottom', 
        legend.justification='left',
        legend.direction='horizontal',
        axis.text.x = element_text(size=24),axis.text.y = element_text(size=24),
        axis.title.x = element_text(size = 26,face="bold"),axis.title.y = element_text(size = 26,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size=24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=24),
        legend.title = element_text(size=24,face="bold"),
        plot.title = element_text(size=26)) +
  scale_x_continuous("Decreasing biodiversity gradients (%)",breaks=c(0,10,25,50,75,100),labels=c("100","90","75","50","25","0")) +
  geom_vline(xintercept=10,color = "black",linetype="dashed",size=1)

all.biodiv.CORAL

# fish only
all.biodiv.FISH <- ggplot(all.biodiv.fish,aes(x=perc.cell,y=freq,colour=as.factor(biodiv.index))) +
  geom_line(size=3) +
  scale_color_manual(values=col) +
  geom_abline(slope=0,intercept=28) +
  xlab("Decreasing biodiversity gradient (%)") + ylab("Proportion of reefs in MPAs (%)") +
  ylim(0,100) +
  ggtitle("Fish") +
  theme_bw() +
  guides(col=guide_legend("Biodiversity Index")) +
  theme(legend.position='bottom', 
        legend.justification='left',
        legend.direction='horizontal') +
  theme_bw()+
  guides(col=guide_legend("Climate change trajectories")) +
  labs(x = "Decreasing biodiversity gradients (%)",y=("Proportion of reefs in protected area (%)")) +
  theme(legend.position='bottom', 
        legend.justification='left',
        legend.direction='horizontal',
        axis.text.x = element_text(size=24),axis.text.y = element_text(size=24),
        axis.title.x = element_text(size = 26,face="bold"),axis.title.y = element_text(size = 26,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size=24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=24),
        legend.title = element_text(size=24,face="bold"),
        plot.title = element_text(size=26)) +
  scale_x_continuous("Decreasing biodiversity gradients (%)",breaks=c(0,10,25,50,75,100),labels=c("100","90","75","50","25","0")) +
  #geom_rect(fill = "lightgrey",xmin = 0,xmax = 10,
  #          ymin = -1,ymax = Inf, alpha = 0.001, color = "black",linetype="dashed",size=0.01) +
  geom_vline(xintercept=10,color = "black",linetype="dashed",size=1)
all.biodiv.FISH

### violin plot of travel time for 90% hotspot

# travel time data.frame for coral
rm(coral.tt.hp.90)
coral.tt.hp.90 <- CORAL.V1.congr %>% dplyr::select(tt.7k,congr_90,hp.sp.90,hp.fric.90,hp.PD.90) %>%
  tidyr::gather(key="quant",value="perc",-tt.7k) %>%
  slice(rep(row_number(), perc)) %>% 
  dplyr::select(-perc)
coral.tt.hp.90$quant <- as.factor(coral.tt.hp.90$quant)
coral.tt.hp.90$quant <- fct_relevel(coral.tt.hp.90$quant,"congr_90","hp.sp.90")
coral.tt.hp.90$quant <- as.factor(coral.tt.hp.90$quant)
coral.tt.hp.90$quant <- factor(coral.tt.hp.90$quant, levels = c("hp.sp.90","hp.fric.90","hp.PD.90","congr_90"))

# coral travel time plot
tt.90.coral <- ggplot(coral.tt.hp.90, aes(quant, tt.7k/60,fill=quant)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun=mean, geom="point", size=4, color="orange",show.legend = FALSE) +
  ylim(0,15) +
  scale_fill_manual(values=col) +
  xlab("") + ylab("Travel Time (h)") +
  scale_x_discrete(labels= c("S","FRic","PD","All")) +
  theme_bw()+
  theme(axis.text.x = element_text(size=24),axis.text.y = element_text(size=24),
        axis.title.x = element_text(size = 26,face="bold"),axis.title.y = element_text(size = 26,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size=24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=24)) +
  guides(col=guide_legend("Biodiversity Index"))
tt.90.coral

# fish tt violin plots
# travel time data.frame for fish
rm(fish.tt.hp.90)
fish.tt.hp.90 <- FISH.V1.congr %>% dplyr::select(tt.7k,congr_90,hp.sp.90,hp.fric.90,hp.PD.90) %>%
  tidyr::gather(key="quant",value="perc",-tt.7k) %>%
  slice(rep(row_number(), perc)) %>% 
  dplyr::select(-perc)
fish.tt.hp.90$quant <- as.factor(fish.tt.hp.90$quant)
summary(fish.tt.hp.90)
fish.tt.hp.90$quant <- fct_relevel(fish.tt.hp.90$quant,"congr_90","hp.sp.90")
fish.tt.hp.90$quant <- as.factor(fish.tt.hp.90$quant)
fish.tt.hp.90$quant <- factor(fish.tt.hp.90$quant, levels = c("hp.sp.90","hp.fric.90","hp.PD.90","congr_90"))

# fish travel time plot
tt.90.fish <- ggplot(fish.tt.hp.90, aes(quant, tt.7k/60,fill=quant)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun=mean, geom="point", size=4, color="orange",show.legend = FALSE) +
  ylim(0,15) +
  scale_fill_manual(values=col) +
  xlab("") + ylab("") +
  scale_x_discrete(labels= c("S","FRic","PD","All")) +
  theme_bw()+
  guides(col=guide_legend("Biodiversity Index")) +
  theme(axis.text.x = element_text(size=24),axis.text.y = element_text(size=24),
        axis.title.x = element_text(size = 26,face="bold"),axis.title.y = element_text(size = 26,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size=30),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=20)) 

tt.90.fish

# save
tt.MPA.biodiv <- ggpubr::ggarrange(all.biodiv.CORAL,all.biodiv.FISH,
                                   tt.90.coral,tt.90.fish,labels=c("A","B","C","D"),align="hv",common.legend = TRUE,legend="bottom",
                                   font.label = list(size = 30))
tt.MPA.biodiv
ggsave(filename = here("_Figures","Supplementals Figure and Tables","FigureS5_MPA_Biodiv_tt.pdf"),width=22,height=22,units="in",dpi=300)

