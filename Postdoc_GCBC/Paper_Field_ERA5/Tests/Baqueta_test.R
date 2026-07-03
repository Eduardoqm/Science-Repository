#Baqueta

#Eduardo Q Marques 15-03-2026

#Daily humidity variation ------------------------------------------------------
library(lubridate)
library(tidyverse)
library(ggpubr)

setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/In situ") #leptop
dir()

dia = read.csv("Umidade_diaria_Reserva_Duquinha.csv", check.names = F)
head(dia)

dia$hora2 = as.numeric(dia$hora2)
dia$repeticoes2 = dia$repeticoes
dia$repeticoes2[dia$repeticoes2 %in% c("C1", "C2", "C3")] <- c("C")

litt = dia %>% 
  filter(repeticoes != "G")

gglit = ggplot(litt, aes(x=hora2, y= umidade_perc))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth()+
  #stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(title = "a)", x = "Tempo (Horas)", y = "Umidade da serrapilheira (%)")+
  theme_minimal(); gglit

ggsave(gglit, filename = "Curva_litteira_Duquinha.png",
       dpi = 600, units = "cm",
       height = 7, width = 10,
       path = "G:/My Drive/Research/PosDoc_GCBC/ERBot")

galhos = dia %>% 
  filter(repeticoes == "G")

ggbra = ggplot(galhos, aes(x=hora2, y= umidade_perc))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth()+
  #stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(title = "Stick Data", x = "Time (Hours)", y = "Fuel Humidity (%)")+
  theme_minimal(); ggbra

ggsave(ggbra, filename = "Curva_galhos_Duquinha.png",
       dpi = 300, units = "cm",
       height = 10, width = 13,
       path = "G:/My Drive/Research/PosDoc_GCBC/ERBot")


#CR1000
library(readr)

cr = read.table("CR1000XSeries_Logger_79537.dat", sep = ",",
                    header = T, skip = 1,
                    stringsAsFactors = F)

cr_fuel = as.data.frame(cr[c(-1,-2),c(1,31)])
cr_fuel = cr_fuel %>% filter(CS506_FuelM != "NAN")

cr_fuel$year = substr(cr_fuel$TIMESTAMP, 1, 4)
cr_fuel$month = substr(cr_fuel$TIMESTAMP, 6, 7)
cr_fuel$day = substr(cr_fuel$TIMESTAMP, 9, 10)
cr_fuel$hour = substr(cr_fuel$TIMESTAMP, 12, 13)
cr_fuel$minute = substr(cr_fuel$TIMESTAMP, 15, 16)
head(cr_fuel)


cr_fuel2 = cr_fuel %>% 
  filter(day == "12") %>% 
  unite("hora2", hour:minute, sep = ".", remove = F)

cr_fuel2$hour  = as.numeric(cr_fuel2$hour)
cr_fuel2$CS506_FuelM  = as.numeric(cr_fuel2$CS506_FuelM)

cr_fuel2 = cr_fuel2 %>%
  #na.omit() %>% 
  group_by(hour) %>% 
  filter(hour > 5, hour < 17) %>% 
  summarise(CS506_FuelM = mean(CS506_FuelM))

cr_fuel2$hora2 = c(6, 6.53, 8.06, 9.00, 10.00, 11.00, 11.50, 13.00, 13.41, 15.00, 16.00)

ggcr = ggplot(cr_fuel2, aes(x=hora2, y=CS506_FuelM))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth()+
  #stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  labs(title = "b)", x = "Tempo (Horas)", y = "Umidade do sensor (%)")+
  theme_minimal(); ggcr

ggsave(ggcr, filename = "Curva_StickSensor_Duquinha.png",
       dpi = 600, units = "cm",
       height = 7, width = 10,
       path = "G:/My Drive/Research/PosDoc_GCBC/ERBot")


dia2 = dia %>% 
  group_by(repeticoes2, hora2) %>% 
  summarise(umidade_perc = mean(umidade_perc)) %>% 
  pivot_wider(id_cols = hora2,
              names_from = repeticoes2,
              values_from = umidade_perc)


df_full = full_join(dia2, cr_fuel2, by = "hora2")
df_full2 = df_full %>% 
  select(hora2, C, G, CS506_FuelM) %>% 
  na.omit()

colnames(df_full2) = c("Hours", "Litterfall", "Branches", "Stick_sensor")
df_full2$Stick_sensor = as.numeric(df_full2$Stick_sensor)

summary(lm(df_full2$Litterfall~df_full2$Stick_sensor))

summary(lm(df_full2$Branches~df_full2$Stick_sensor))

ggplot(df_full2)+
  geom_point(aes(x=Hours, y=Litterfall, col = "Litterfall"))+
  geom_point(aes(x=Hours, y=Branches, col = "Sticks"))+
  geom_point(aes(x=Hours, y=Stick_sensor, col = "Stick Sensor"))+
  geom_smooth(aes(x=Hours, y=Litterfall, col = "Litterfall"))+
  geom_smooth(aes(x=Hours, y=Branches, col = "Sticks"))+
  geom_smooth(aes(x=Hours, y=Stick_sensor, col = "Stick Sensor"))+
  labs(x = "Time (Hours)", y = "Fuel Humidity (%)", col = NULL)+
  theme_minimal()

ggtest = ggplot(df_full2, aes(x=Stick_sensor, y=Litterfall))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  #stat_cor(show.legend = F, label.y.npc = 1, label.x.npc = 0.5, p.digits = 0)+
  geom_abline()+
  annotate("text", x = 78, y = 80, label = "1:1", parse = TRUE)+
  labs(title = "c)",
       x = "Umidade do sensor (%)",
       y = "Umidade da serrapilheira (%)")+
  theme_minimal(); ggtest

ggsave(ggtest, filename = "CS506_vs_Litterfall.png",
       dpi = 600, units = "cm",
       height = 7, width = 10,
       path = "G:/My Drive/Research/PosDoc_GCBC/ERBot")


ggtest2 = ggplot(df_full2, aes(x=Stick_sensor, y=Branches))+
  geom_point(col = "blue", size = 3, alpha = 0.5)+
  geom_smooth(method = "lm")+
  geom_abline()+
  labs(title = "d)",
       x = "Umidade do sensor (%)",
       y = "Umidade dos galhos (%)")+
  theme_minimal(); ggtest2

ggsave(ggtest2, filename = "CS506_vs_sticks.png",
       dpi = 600, units = "cm",
       height = 7, width = 10,
       path = "G:/My Drive/Research/PosDoc_GCBC/ERBot")






#Calibration test --------------------------------------------------------------
df_full2$diff = df_full2$Stick_sensor - df_full2$Litterfall
head(df_full2)
calib = mean(df_full2$diff)

df_full2$Stick_sensor_calib = df_full2$Stick_sensor - calib

summary(lm(df_full2$Litterfall~df_full2$Stick_sensor_calib))

ggplot(df_full2)+
  geom_point(aes(x=Hours, y=Litterfall, col = "Litterfall"))+
  geom_point(aes(x=Hours, y=Stick_sensor, col = "Stick Sensor"))+
  geom_point(aes(x=Hours, y=Stick_sensor_calib, col = "Stick Sensor Calibrated"))+
  geom_smooth(aes(x=Hours, y=Litterfall, col = "Litterfall"))+
  geom_smooth(aes(x=Hours, y=Stick_sensor, col = "Stick Sensor"))+
  geom_smooth(aes(x=Hours, y=Stick_sensor_calib, col = "Stick Sensor Calibrated"))+
  labs(x = "Time (Hours)", y = "Fuel Humidity (%)", col = NULL)+
  theme_minimal()

ggplot(df_full2, aes(x=Stick_sensor_calib, y=Litterfall))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_abline()

