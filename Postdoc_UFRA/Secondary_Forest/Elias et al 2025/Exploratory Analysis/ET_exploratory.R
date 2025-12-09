#ET Exploratory Analysis

#Eduardo Q Marques 26-09-2025

library(ggplot2)
library(tidyverse)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Dados")
dir()

et_sf = read.csv("ET_SecFor_full.csv")
et_agb = read.csv("ET_AGB_full.csv")

#Summarizing data --------------------------------------------------------------
et_sf2 = et_sf %>% 
  group_by(test, sf_age, cond)%>%
  summarise(et=mean(delta_et,na.rm=T))

et_agb$grupo = cut(et_agb$agb, breaks = seq(0,10000, by = 5), labels = F)
et_agb2 = et_agb %>% 
  group_by(test, cond, grupo)%>%
  summarise(agb = mean(agb),et=mean(delta_et,na.rm=T))

#Plotting Results --------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Figures")

plt1 = ggplot(et_sf2, aes(x=sf_age, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Secondary forest age (year)",y="Δ Evapotranspiration (W/m²/day)",
       col = "Condition")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  facet_wrap(~test, scales = "free", strip.position = "top")+
  theme_minimal()+
  theme(strip.text.x = element_text(vjust = 1, hjust = 0, margin=margin(l=0)),
        strip.text = element_text(size=13),
        legend.position = "none"); plt1

ggsave(plot = plt1, "Delta_ET_Results.png", dpi = 300,
       height = 10, width = 30, units = "cm")


plt2 = ggplot(et_agb2, aes(x=agb, y=et, col = cond))+
  geom_point(size = 3)+
  stat_smooth()+
  labs(x="Aboveground Biomass (Mg/ha)",y="Δ Evapotranspiration (W/m²/day)",
       col = "Condition")+
  scale_color_manual(values = c("#66bd63", "#fc8d59", "#67a9cf"))+
  facet_wrap(~test, scales = "free")+
  theme_minimal()+
  theme(strip.text.x = element_text(vjust = 1, hjust = 0, margin=margin(l=0)),
        strip.text = element_text(size=13),
        legend.position = c(0.9, 0.5)); plt2

ggsave(plot = plt2, "Delta_ET_AGB_Results.png", dpi = 300,
       height = 10, width = 30, units = "cm")



#===============================================================================




library(tidyverse)

setwd("C:/Users/Public/Documents/Analises_Elias/Dados/ET")
dir()

df = read_csv("ET_AGB_Past_Dry_full_2022.csv")
head(df)


df2=df%>%
  filter(sf_perc>=70)%>%
  mutate(agb=round(agb,0))%>%
  group_by(agb,cond)%>%
  summarise(delta_et=mean(delta_et,na.rm=T),
            sf_perc=mean(sf_perc,na.rm=T),
            n=n())%>%
  filter(n>200,agb<250)



head(df2)

ggplot(df2,aes(agb,delta_et))+
  geom_point()






df1 = read_csv("ET_AGB_Past_Rainy_full_2022.csv")
head(df1)


df20=df1%>%
  filter(sf_perc>=70)%>%
  mutate(agb=round(agb,0))%>%
  group_by(agb,cond)%>%
  summarise(delta_et=mean(delta_et,na.rm=T),
            sf_perc=mean(sf_perc,na.rm=T),
            n=n())%>%
  filter(n>200,agb<250)
head(df20)


dff=df20%>%
  bind_rows(df2)
head(dff)

ggplot(dff,aes(agb,delta_et,colour=cond))+
  geom_point()+
  stat_smooth()



