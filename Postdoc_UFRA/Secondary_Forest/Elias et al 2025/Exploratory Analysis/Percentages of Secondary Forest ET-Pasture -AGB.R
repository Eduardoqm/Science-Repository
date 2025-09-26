#Percentages of Secondary Forest (ET - Past - AGB test)

#Eduardo Q Marques 12-09-2025

library(ggplot2)
library(tidyverse)

setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Dados\\")
dir()

et_a = read.csv("ET_AGB_Past_Annual_full.csv")
et_b = read.csv("ET_AGB_Past_Dry_full.csv")
et_c = read.csv("ET_AGB_Past_Rainy_full.csv")

et_df = rbind(et_a, et_b, et_c)

df5 = et_df %>% 
  filter(sf_perc >= 5) %>% 
  mutate(agb=round(agb,0))%>%
  group_by(agb, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))

df30 = et_df %>%
  filter(sf_perc >= 30)%>% 
  mutate(agb=round(agb,0))%>%
  group_by(agb, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))

df50 = et_df%>% 
  filter(sf_perc >= 50)%>% 
  mutate(agb=round(agb,0))%>%
  group_by(agb, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))

df70 = et_df %>% 
  filter(sf_perc >= 70)%>% 
  mutate(agb=round(agb,0))%>%
  group_by(agb, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))

df100 = et_df %>% 
  filter(sf_perc == 100)%>% 
  mutate(agb=round(agb,0))%>%
  group_by(agb, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))


setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Figures\\Percentage_tests\\")
gg5 = ggplot(df5, aes(x=agb, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ Evapotranspiration (W/m²)",
       col = "Condition", title = "Secondary Forest vs. Pasture - Purity >5%")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  theme_minimal(); gg5

ggsave(plot = gg5, "Delta_ET_Past_AGB_5_perc.png", dpi = 300,
       height = 10, width = 15, units = "cm")


gg30 = ggplot(df30, aes(x=agb, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ Evapotranspiration (W/m²)",
       col = "Condition", title = "Secondary Forest vs. Pasture - Purity >30%")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  theme_minimal(); gg30

ggsave(plot = gg30, "Delta_ET_Past_AGB_30_perc.png", dpi = 300,
       height = 10, width = 15, units = "cm")


gg50 = ggplot(df50, aes(x=agb, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ Evapotranspiration (W/m²)",
       col = "Condition", title = "Secondary Forest vs. Pasture - Purity >50%")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  theme_minimal(); gg50

ggsave(plot = gg50, "Delta_ET_Past_AGB_50_perc.png", dpi = 300,
       height = 10, width = 15, units = "cm")

gg70 = ggplot(df70, aes(x=agb, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ Evapotranspiration (W/m²)",
       col = "Condition", title = "Secondary Forest vs. Pasture - Purity >70%")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  theme_minimal(); gg70

ggsave(plot = gg70, "Delta_ET_Past_AGB_70_perc.png", dpi = 300,
       height = 10, width = 15, units = "cm")


gg100 = ggplot(df100, aes(x=agb, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ Evapotranspiration (W/m²)",
       col = "Condition", title = "Secondary Forest vs. Pasture - Purity >100%")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  theme_minimal(); gg100

ggsave(plot = gg100, "Delta_ET_Past_AGB_100_perc.png", dpi = 300,
       height = 10, width = 15, units = "cm")


