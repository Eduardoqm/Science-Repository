library(tidyverse)
library(ggplot2)

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/LAI e Liteira")
dir()

l=read.csv("1_master_liteira_area_1_jun2020.csv")
head(l)

l2=l%>%mutate(bi=ifelse(pont2=="D"|pont2=="K"|pont2=="P","Interior","Borda"))%>%
  filter(years<2020)%>%
  group_by(plot,years,bi)%>%
  summarise(media=mean(weight_1,na.rm = T),
            dp=sd(weight_1,na.rm = T))

head(l2)

ggplot(l2,aes(years,media,colour=plot))+
  geom_point()+
  geom_line()+
  facet_grid(bi~.)+
  labs(y="Peso (g)")+
  theme_minimal()


cesto = 0.8*0.6 #Area do cesto (m²)
#cesto = cesto*90 #Numero de cestos por tratamento
cesto = cesto/10000 #Converter em hectares
cesto2 = 1/0.48


l$weight_2 = l$weight_1*1000

l2x=l%>%mutate(bi=ifelse(pont2=="D"|pont2=="K"|pont2=="P","Interior","Borda"))%>%
  filter(years<2020)%>%
  group_by(plot,years,bi)%>%
  summarise(media=mean(weight_2,na.rm = T),
            dp=sd(weight_1,na.rm = T))

head(l2x)

ggplot(l2x,aes(years,media,colour=plot))+
  geom_point()+
  geom_line()+
  facet_grid(bi~.)+
  labs(y="Peso (g)")+
  theme_minimal()

l2x$media2 = l2x$media/cesto2
  head(l2x)

  ggplot(l2x,aes(years,media2,colour=plot))+
    geom_point()+
    geom_line()+
    facet_grid(bi~.)+
    labs(y="Peso (g)")+
    theme_minimal()


l3=l%>%mutate(bi=ifelse(pont2=="D"|pont2=="K"|pont2=="P","Interior","Borda"))%>%
  filter(years<2020)%>%
  group_by(plot,years,bi)%>%
  summarise(total=sum(weight_1,na.rm = T))

l3$total2 = l3$total*1000
head(l3)

#l3$total = as.numeric(l3$total)
ggplot(l3,aes(years,total2,col=plot))+
  geom_point()+
  geom_line()+
  facet_grid(bi~.)+
  labs(y="Peso (T)")+
  theme_minimal()



