#LST Exploratory Analysis

#Eduardo Q Marques 15-09-2025

library(ggplot2)
library(tidyverse)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Dados")
dir()

#Secondary Forest - Primary Forest
lst_a = read.csv("LST_SecFor_Age_Annual_full.csv")
lst_b = read.csv("LST_SecFor_Age_Dry_full.csv")
lst_c = read.csv("LST_SecFor_Age_Rainy_full.csv")

lst_pri = rbind(lst_a, lst_b, lst_c)
lst_pri$test = "sf_pri"

#Secondary Forest - Pasture
lst_a = read.csv("LST_Pasture_Annual_full.csv")
lst_b = read.csv("LST_Pasture_Dry_full.csv")
lst_c = read.csv("LST_Pasture_Rainy_full.csv")

lst_past = rbind(lst_a, lst_b, lst_c)
lst_past$test = "sf_past"

#Secondary Forest - Primary Forest x AGB
lst_a = read.csv("LST_AGB_Annual_full.csv")
lst_b = read.csv("LST_AGB_Dry_full.csv")
lst_c = read.csv("LST_AGB_Rainy_full.csv")

lst_pri_agb = rbind(lst_a, lst_b, lst_c)
lst_pri_agb$test = "sf_pri_agb"

#Secondary Forest - Pasture x AGB
lst_a = read.csv("LST_AGB_Past_Annual_full.csv")
lst_b = read.csv("LST_AGB_Past_Dry_full.csv")
lst_c = read.csv("LST_AGB_Past_Rainy_full.csv")

lst_past_agb = rbind(lst_a, lst_b, lst_c)
lst_past_agb$test = "sf_past_agb"

#Filtering data ----------------------------------------------------------------
lst_pri2 = lst_pri %>% 
  filter(sf_perc >= 70)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(test, sf_age, cond)%>%
  summarise(lst=mean(delta_lst,na.rm=T))











