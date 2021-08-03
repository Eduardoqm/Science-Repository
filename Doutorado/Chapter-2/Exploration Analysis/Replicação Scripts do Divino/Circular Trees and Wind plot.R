#Circular Trees and Wind
#Blowdown Data (AREA-1)
#Eduardo Q Marques 03-08-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(fmsb)

#Load data ------------------------------------------------------------------------------------------
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')

#df = read.csv("blowdown_full_update_2021.csv", sep = ",")
df = read.csv("blowdown_full_update_2021_B.csv", sep = ",")

#Resume data
#df = df[,c(2,7,8,9,10,11,12,13,14,15,22,23)]
df = df[,c(3,7,8,9,10,11,12,13,14,15,25,18)]

df$nt = 1
colnames(df) = c("Specie","Treatment","Line","Transect","Condition","Alt_Scar","Wind?","Direction","Damage","Alt_Broken","Alt_tree", "DAP", "Number_of_Trees")


#Summary information
summary(df)

#Plot data ------------------------------------------------------------------------------------------
#Fall direction
ggplot(df, aes(x=Direction))+
  geom_density(col = "black", fill = "darkblue", alpha = 0.5)+
  theme_minimal()+
  ggtitle("Direction the trees fell (Degrees)")

wind = df %>% 
  group_by(Direction) %>% 
  summarise(Number_of_Trees = sum(Number_of_Trees)) %>% 
  na.omit()
wind$Direction = as.factor(wind$Direction)

ggplot(wind, aes(x=Direction, y = Number_of_Trees, fill = Number_of_Trees))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())+
  coord_polar(start = 0)+
  ggtitle("Direction the trees fell (Degrees)")





ggplot(wind, aes(x=Direction, y=Number_of_Trees, col=Number_of_Trees))+
  #geom_bar(stat="identity")+
  geom_jitter()+
  theme_minimal()+
  theme(axis.text.y = element_blank(), axis.title = element_blank())+
  coord_polar(start = 0)+
  ggtitle("Direction the trees fell (Degrees)")


library(circular)
df2 = df[,8] %>% 
  na.omit()
circ <- circular(df2, units = "degrees", template = "geographics") 

















