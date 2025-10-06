#LST Exploratory Analysis

#Eduardo Q Marques 15-09-2025

library(ggplot2)
library(tidyverse)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Dados")
dir()

lst_sf = read.csv("LST_SecFor_full.csv")
lst_agb = read.csv("LST_AGB_full.csv")

#Filtering data ----------------------------------------------------------------
lst_sf2 = lst_sf %>% 
  group_by(test, sf_age, cond)%>%
  summarise(lst=mean(delta_lst,na.rm=T))

lst_agb$grupo = cut(lst_agb$agb, breaks = seq(0,10000, by = 5), labels = F)
lst_agb2 = lst_agb %>% 
  group_by(test, cond, grupo)%>%
  summarise(agb = mean(agb),lst=mean(delta_lst,na.rm=T))

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
        strip.text = element_text(size=13),
        legend.position = "none"); plt1

ggsave(plot = plt1, "Delta_LST_Results.png", dpi = 300,
       height = 10, width = 30, units = "cm")


plt2 = ggplot(lst_agb2, aes(x=agb, y=lst, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ LST (C°)",
       col = "Condition")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  facet_wrap(~test, scales = "free")+
  theme_minimal()+
  theme(strip.text.x = element_text(vjust = 1, hjust = 0, margin=margin(l=0)),
        strip.text = element_text(size=13),
        legend.position = c(0.9, 0.5)); plt2

ggsave(plot = plt2, "Delta_LST_AGB_Results.png", dpi = 300,
       height = 10, width = 30, units = "cm")




