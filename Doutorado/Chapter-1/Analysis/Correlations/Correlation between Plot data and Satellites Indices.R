#Correlation between Plot data and Satellites Indices

#Eduardo Q Marques 27-01-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(mgcv)
library(visreg)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Landsat =========================================================
#Data ------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')
lai = read.csv("LAI_full_tang.csv", sep = ',')
litt = read.csv("Liteira_full_tang.csv", sep = ',')

#Modify and filter date to match ---------------------------------
df$year = substr(df$year, 1,4)
df = df %>% 
  filter(year == c(2005:2017)) %>% 
  unite("id", c("treat", "year"), sep = "_")

df2 = df[,c(4,5,6)]
df2 = df2 %>%
  group_by(id, index) %>% 
  summarise(value = mean(value))

lai = lai %>% 
  filter(year != 2012) %>% 
  unite("id", c("parcela", "year"), sep = "_")

lai2 = lai[,c(3,4)]
lai2 = lai2 %>% 
  group_by(id) %>% 
  summarise(lai = mean(lai))


litt = litt %>% 
  filter(year == c(2005:2017)) %>% 
  filter(year != 2012) %>% 
  unite("id", c("parcela", "year"), sep = "_")

litt2 = litt[,c(1,5)]
litt2 = litt2 %>% 
  group_by(id) %>% 
  summarise(lit_ton_hec = mean(lit_ton_hec))

#Join Data ---------------------------------------------------------
df3 = full_join(df2, lai2, by = "id")
df4 = full_join(df3, litt2, by = "id")

#Plot correlation --------------------------------------------------
a = ggplot(df4, aes(x=value, y=lai))+
  geom_point(size = 3, col = "red")+
  geom_smooth(method="lm", se=F, col = "red")+ 
  facet_grid(cols = vars(index), scales = "free")+
  stat_cor(show.legend = F)+
  theme_bw()+
  labs(x = "", y = "LAI")+
  theme(text = element_text(family = "Times New Roman", size = 14))

b = ggplot(df4, aes(x=value, y=lit_ton_hec))+
  geom_point(size = 3, col = "orange")+
  geom_smooth(method="lm", se=F, col = "orange")+ 
  facet_grid(cols = vars(index), scales = "free")+
  stat_cor(show.legend = F)+
  theme_bw()+
  labs(x = "", y = "Liteira (T/ha)")+
  theme(text = element_text(family = "Times New Roman", size = 14))


ggarrange(a, b,
          common.legend = TRUE,
          legend="bottom",
          ncol = 1, nrow = 2)






df5 = df4
df5$lai = (df5$lai)/10

ggplot(df5)+
  geom_point(aes(x=value, y=lai), size = 3, col = "red")+
  geom_smooth(aes(x=value, y=lai), method="lm", se=F, col = "red")+ 
  stat_cor(aes(x=value, y=lai),
           show.legend = F, col = "red")+
  
  geom_point(aes(x=value, y=lit_ton_hec), size = 3, col = "orange")+
  geom_smooth(aes(x=value, y=lit_ton_hec), method="lm", se=F, col = "orange")+
  facet_wrap(~index, scales="free")+
  stat_cor(aes(x=value, y=lit_ton_hec),
           show.legend = F, label.y = 0.4, col = "orange")+
  
  theme_bw()+
  labs(x = "", y = "")+
  theme(text = element_text(family = "Times New Roman", size = 14))


  
  ggplot(df4, aes(x = value))+
    geom_point(aes(y=lai), size = 3, col = "red")+
    geom_smooth(aes(y=lai), method="lm", se=F, col = "red")+ 
    stat_cor(aes(x=value, y=lai),
             show.legend = F, col = "red")+
    
    geom_point(aes(y=lit_ton_hec), size = 3, col = "orange")+
    geom_smooth(aes(y=lit_ton_hec), method="lm", se=F, col = "orange")+
    stat_cor(aes(x=value, y=lit_ton_hec),
             show.legend = F, label.y = 4, col = "orange")+
    
    facet_wrap(~index, scales="free")+
    theme_bw()+
    labs(x = "", y = "")
  theme(text = element_text(family = "Times New Roman", size = 14))

#-----------------------------------





























df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Modify elements of dataframe
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "vig"] <- c("VIG")

df$treat[df$treat == "control"] <- c("Controle")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")










eqm = c("orange", "red", "blue") #My color palette










