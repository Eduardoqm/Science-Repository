#Heatmap of Hyperion Index all data

#Eduardo Q Marques 10-03-2020

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(viridis)

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
df$year = as.character(df$year)

df2 = df%>%
  group_by(index,parcela,year,y) %>%
  summarise(value = mean(value,na.rm=TRUE))

pssr = df2 %>% 
  filter(index == "pssr")


ggplot(pssr, aes(year, y, fill = value))+ 
  geom_tile()+
  facet_wrap(~parcela,scales="free")+
  scale_fill_viridis(discrete=FALSE)




ggplot(aes(x=y,y=`mean(value, na.rm = TRUE)`,
           col=year,
           group=treat)) +
  geom_boxplot() +
  facet_wrap(~index,scales="free")
