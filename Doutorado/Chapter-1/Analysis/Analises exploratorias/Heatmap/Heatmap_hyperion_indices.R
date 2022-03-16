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

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
df$year = as.character(df$year)

df2 = df%>%
  group_by(index,treat,year,dist) %>%
  summarise(value = mean(value,na.rm=TRUE))

psri = df2 %>% 
  filter(index == "psri")


ggplot(psri, aes(year, dist, fill = value))+ 
  geom_tile()+
  facet_wrap(~treat)+
  scale_fill_viridis(discrete=FALSE)

ggplot(psri, aes(x=year,y=value, fill=treat))+
  geom_boxplot()

evi = df2 %>% 
  filter(index == "psri") #%>% 
  #filter(treat == "b1yr")

evigg = ggplot(evi, aes((dist*100000), year, fill = value))+ 
  geom_tile()+
  facet_wrap(~treat)+
  scale_fill_viridis(discrete=FALSE)


library(rayshader)
evigg
plot_gg(evigg, width = 6, preview = TRUE)

plot_gg(evigg, multicore = TRUE, raytrace = TRUE, scale = 50, windowsize = c(900, 600), zoom = 0.6, phi = 30, theta = 30, width = 6)














