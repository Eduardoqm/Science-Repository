#Test Arduino Auto-Balance

#Eduardo Q Marques 20-11-2025

library(tidyverse)
library(ggpubr)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Research/PosDoc_GCBC/Analises/In situ/Balanca_Arduino") #Ecostation
#setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/ERA5") #Laptop
dir()

aui = read.csv("Arduino_peso.csv"); head(aui)
sit = read.csv("umidade_serrapilheira.csv"); head(sit)

#Adjusts in time columns -------------------------------------------------------
aui$DataHora2 = as.POSIXct(aui$DataHora, format =  "%m/%d/%Y %H:%M")

sit = sit %>%
  unite(DataHora, Data, Hora, sep = " ")
sit$DataHora2 = as.POSIXct(sit$DataHora, format =  "%d/%m/%Y %H:%M")

#Difference and percentage of humidity -----------------------------------------
aui$dif_peso = aui$PesoAtual-aui$PesoSeco
aui$humid = (aui$dif_peso/aui$PesoAtual)*100

sit$dif_peso = sit$PesoUmido-sit$PesoSeco
sit$humid = (sit$dif_peso/sit$PesoUmido)*100

sit2 = sit %>% 
  group_by(DataHora2) %>% 
  summarise(humid = mean(humid))


ggplot(aui, aes(x=DataHora2, y=humid))+
  geom_point()+
  geom_line()

ggplot(sit2, aes(x=DataHora2, y=humid))+
  geom_point()+
  geom_line()


aui2 = aui[,c(6,8)]
aui2$cond = "Arduino"
sit2$cond = "Manual"

df = rbind(aui2, sit2)

ggplot(df, aes(x=DataHora2, y=humid, col = cond))+
  geom_point()+
  geom_line()
  #geom_smooth()


sit2$DataHora3 = substr(sit2$DataHora2, 1, 13)
aui2$DataHora3 = substr(aui2$DataHora2, 1, 13)
aui2 = aui2 %>% 
  group_by(DataHora3, cond) %>% 
  summarise(humid = mean(humid))

df2 = full_join(aui2, sit2, by = "DataHora3")
df2 = df2[,c(1,3,5)]
colnames(df2) = c("Time", "Arduino", "Marimon-Ray")

ggplot(df2, aes(x=`Marimon-Ray`, y=Arduino))+
  geom_point()+
  geom_smooth(method = "lm")

mod = lm(df2$Arduino~df2$`Marimon-Ray`)
summary(mod)


