#ET Exploratory Analysis

#Eduardo Q Marques 26-09-2025

library(ggplot2)
library(tidyverse)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Dados")
dir()

#Secondary Forest - Primary Forest
et_a = read.csv("ET_SecFor_Age_Annual_full.csv")
et_b = read.csv("ET_SecFor_Age_Dry_full.csv")
et_c = read.csv("ET_SecFor_Age_Rainy_full.csv")

et_pri = rbind(et_a, et_b, et_c)
et_pri$test = "a) Secondary - Primary Forest"

#Secondary Forest - Pasture
et_a = read.csv("ET_Pasture_Annual_full.csv")
et_b = read.csv("ET_Pasture_Dry_full.csv")
et_c = read.csv("ET_Pasture_Rainy_full.csv")

et_past = rbind(et_a, et_b, et_c)
et_past$test = "b) Secondary Forest - Pasture"

et_sf = rbind(et_pri, et_past)

#Secondary Forest - Primary Forest x AGB
et_a = read.csv("ET_AGB_Annual_full.csv")
et_b = read.csv("ET_AGB_Dry_full.csv")
et_c = read.csv("ET_AGB_Rainy_full.csv")

et_pri_agb = rbind(et_a, et_b, et_c)
et_pri_agb$test = "c) Secondary - Primary Forest x AGB"

#Secondary Forest - Pasture x AGB
et_a = read.csv("ET_AGB_Past_Annual_full.csv")
et_b = read.csv("ET_AGB_Past_Dry_full.csv")
et_c = read.csv("ET_AGB_Past_Rainy_full.csv")

et_past_agb = rbind(et_a, et_b, et_c)
et_past_agb$test = "d) Secondary Forest - Pasture x AGB"

et_agb = rbind(et_pri_agb, et_past_agb)

et_agb$grupo = cut(et_agb$agb, breaks = seq(0,10000, by = 10), labels = F)

#Filtering data ----------------------------------------------------------------
et_sf2 = et_sf %>% 
  filter(sf_perc >= 70)%>% 
  mutate(sf_age=round(sf_age,0))%>%
  group_by(test, sf_age, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))

et_agb2 = et_agb %>% 
  filter(sf_perc >= 70)%>% 
  filter(agb < 401) %>% 
  mutate(agb=round(agb,0))%>%
  group_by(test, cond, grupo)%>%
  summarise(agb = mean(agb),et=mean(delta_et,na.rm=T))

#Plotting Results --------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Figures")

plt1 = ggplot(et_sf2, aes(x=sf_age, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ et (C°)",
       col = "Condition")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  facet_wrap(~test, scales = "free", strip.position = "top")+
  theme_minimal()+
  theme(strip.text.x = element_text(vjust = 1, hjust = 0, margin=margin(l=0)),
        strip.text = element_text(size=13),
        legend.position = "none"); plt1

ggsave(plot = plt1, "Delta_et_Results.png", dpi = 300,
       height = 10, width = 30, units = "cm")


plt2 = ggplot(et_agb2, aes(x=agb, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ et (C°)",
       col = "Condition")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  facet_wrap(~test, scales = "free")+
  theme_minimal()+
  theme(strip.text.x = element_text(vjust = 1, hjust = 0, margin=margin(l=0)),
        strip.text = element_text(size=13),
        legend.position = c(0.9, 0.5)); plt2

ggsave(plot = plt2, "Delta_et_AGB_Results.png", dpi = 300,
       height = 10, width = 30, units = "cm")




