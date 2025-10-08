#Fuel Humidity

#Eduardo Q Marques 07-10-2025

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Documentos/PosDoc_GCBC/Apresentação")
dir()

df = read.csv("umidade_serrapilheira.csv")
head(df)

df = df[,c(3,4,6,7,8,9)]

df2 = df %>% 
  filter(Idade < 100) 
  #filter(dif_peso_seco_e_umido > 0)

df2$peso_seco2 = df2$peso_seco*0.539 #185.5 cm² is the size of Marimon-Hey


ggplot(df2, aes(x=Idade, y= peso_seco2))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Secondary Forest age (Years)", y = "Accumulated fuel (Mg ha⁻¹)")

df3 = df %>% 
  filter(Idade < 100) %>% 
  filter(dif_peso_seco_e_umido > 0)

df3$humid = (df3$dif_peso_seco_e_umido/df3$peso_umido)*100

ggplot(df3, aes(x=Idade, y= humid))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Secondary Forest age (Years)", y = "Humidity (%)")



serra = read.csv("cesta_serapilheira.csv")


ggplot(serra, aes(x=Idade, y=Peso_Total))+
  geom_point()+
  geom_smooth()

  
  
  
  
  
  
  
  