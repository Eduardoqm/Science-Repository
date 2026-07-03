#Fuel Humidity

#Eduardo Q Marques 07-10-2025

library(tidyverse)
library(ggpubr)

#Load data ---------------------------------------------------------------------
#setwd("G:/Meu Drive/Research/PosDoc_GCBC/Analises/In situ") #Ecostation
setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ") #leptop
dir()

df = read.csv("umidade_serrapilheira.csv")
head(df)

df = df[,c(3,4,6,7,8,9)]

df2 = df %>% 
  filter(Idade < 100) 
#filter(dif_peso_seco_e_umido > 0)

df2$peso_seco2 = df2$peso_seco*0.539 #185.5 cm² is the size of Marimon-Hey
mean(df2$peso_seco2)

mar = ggplot(df2, aes(x=Idade, y= peso_seco2))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(x = "Secondary Forest Age (Years)", y = "Accumulated fuel (Mg/ha⁻¹)")+
  theme_minimal()+
  theme(legend.position = "none"); mar

df3 = df %>% 
  filter(Idade < 100) %>% 
  filter(dif_peso_seco_e_umido > 0)

df3$humid = (df3$dif_peso_seco_e_umido/df3$peso_umido)*100

hum = ggplot(df3, aes(x=Idade, y= humid))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(x = "Secondary Forest Age (Years)", y = "Humidity (%)")+
  theme_minimal()+
  theme(legend.position = "none"); hum

#Litterfall by basket ----------------------------------------------------------
serra = read.csv("cesta_serapilheira.csv")

serra2 = serra %>% 
  filter(Idade < 100) 

serra2$Peso_Total2 = serra2$Peso_Total*0.02083 #4800 cm² is the size of the basket


cesto = ggplot(serra2, aes(x=Idade, y=Peso_Total2))+
  geom_point(aes(col = Sitio.Nomelocal), size = 3, alpha = 0.5)+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(x = "Secondary Forest Age (Years)", y = "Litter Production (Mg/ha/month)",
       col = NULL)+
  theme_minimal(); cesto

#Daily humidity variation ------------------------------------------------------
library(lubridate)

dia = read.csv("Umidade_diaria_Capoeira_UFRA.csv", check.names = F)

dia$humid = (dia$dif_peso_umi_seco/dia$peso_umido)*100

dhum = ggplot(dia, aes(x=hora2, y= humid))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth()+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(title = "a)", x = "Time (Hours)", y = "Fuel Humidity (%)")+
  theme_minimal(); dhum

#ERA5 --------------------------------------------------------------------------
era = read.csv("ERA5_Temp_RH_VPD_Precip_07-10-2025.csv")
era$hora2 = as.numeric(substr(era$system.index, 10, 11))

era2 = full_join(era, dia, by = "hora2")
era2 = era2 %>% 
  filter(hora2 %in% c(6:16))
  

dvpd= ggplot(era2, aes(x=hora2, y= VPD))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(col = "blue")+
  geom_hline(aes(yintercept=0.75), colour="black", linetype="dashed")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(title = "b)", x = "Time (Hours)", y = "VPD (kPa)")+
  theme_minimal();dvpd


derarh = ggplot(era2, aes(x=hora2, y= RH))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(col = "blue")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(title = "c)", x = "Time (Hours)", y = "Air Humidity (%)")+
  theme_minimal();derarh


vpd_fuel = ggplot(era2, aes(x=humid, y= VPD))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(col = "blue", method = "lm")+
  geom_hline(aes(yintercept=0.75), colour="black", linetype="dashed")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(title = "c)", x = "Fuel Humidity (%)", y = "VPD (kPa)")+
  theme_minimal();vpd_fuel

ggplot(era2, aes(x=humid, y= RH))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(col = "blue", method = "lm")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(x = "Fuel Humidity (%)", y = "Air Humidity (%)")+
  theme_minimal();




#PurpleAir Humidity ------------------------------------------------------------
pair = read_csv("PurpleAir_20251007.csv")

pair$time = as.numeric(substr(pair$UTCDateTime, 12, 13))
pair$current_temp_c = (pair$current_temp_f - 32) * 5/9

pair2 = pair %>% 
  group_by(time) %>% 
  filter(time > 5, time < 17) %>% 
  summarise(current_humidity = mean(current_humidity),
            current_temp_c = mean(current_temp_c))

pphum = ggplot(pair2, aes(x=time, y= current_humidity))+
  geom_point(col = "purple", size = 3, alpha = 0.5)+
  geom_smooth(col = "purple")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(x = "Time (Hours)", y = "Air Humidity (%)")+
  theme_minimal(); pphum

pptmp = ggplot(pair2, aes(x=time, y= current_temp_c))+
  geom_point(col = "purple", size = 3, alpha = 0.5)+
  geom_smooth(col = "purple")+
  stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(x = "Time (Hours)", y = "Air Temperature (Cº)")+
  theme_minimal(); pptmp



#Exporting plots ---------------------------------------------------------------
setwd("G:/Meu Drive/Research/PosDoc_GCBC/Analises/Figuras")

ggsave(plot = mar, filename = "Peso_marimonhey.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = hum, filename = "Humidity_marimonhey.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = dhum, filename = "Humidity_daily.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = dvpd, filename = "ERA_VPD_daily.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = derarh, filename = "ERA_HR_daily.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = vpd_fuel, filename = "VPDxFuel_daily.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = pphum, filename = "PPair_Humidity_daily.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = pptmp, filename = "PPair_temp_daily.png",
       width = 13, height = 10, units = "cm", dpi = 600)

ggsave(plot = cesto, filename = "Peso_cesto.png",
       width = 16, height = 10, units = "cm", dpi = 600)

  
  
  
  
  
  
  
  