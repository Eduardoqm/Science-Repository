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
lst_pri$test = "a) Secondary - Primary Forest"

#Secondary Forest - Pasture
lst_a = read.csv("LST_Pasture_Annual_full.csv")
lst_b = read.csv("LST_Pasture_Dry_full.csv")
lst_c = read.csv("LST_Pasture_Rainy_full.csv")

lst_past = rbind(lst_a, lst_b, lst_c)
lst_past$test = "b) Secondary Forest - Pasture"

lst_sf = rbind(lst_pri, lst_past)

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

lst_agb = rbind(lst_pri_agb, lst_past_agb)

#Filtering data ----------------------------------------------------------------
lst_sf2 = lst_sf %>% 
  filter(sf_perc >= 70)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(test, sf_age, cond)%>%
  summarise(lst=mean(delta_lst,na.rm=T))

lst_agb2 = lst_agb %>% 
  filter(sf_perc >= 70)%>% 
  filter(agb < 401) %>% 
  mutate(agb=round(agb,0))%>%
  group_by(test, agb, cond)%>%
  summarise(lst=mean(delta_lst,na.rm=T))

#Plotting Results --------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Figures")

plt1 = ggplot(lst_sf2, aes(x=sf_age, y=lst, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ LST (C°)",
       col = "Condition")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  facet_wrap(~test, scales = "free", strip.position = "top")+
  theme_minimal()+
  theme(strip.text.x = element_text(vjust = 1, hjust = 0, margin=margin(l=0)),
        strip.text = element_text(size=13)); plt1

ggsave(plot = plt1, "Delta_LST_Results.png", dpi = 300,
       height = 10, width = 30, units = "cm")




ggplot(lst_agb2, aes(x=agb, y=lst, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ LST (C°)",
       col = "Condition")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  facet_wrap(~test, scales = "free")+
  theme_minimal()

ggsave(plot = plt2, "Delta_LST_AGB_Results.png", dpi = 300,
       height = 10, width = 15, units = "cm")




