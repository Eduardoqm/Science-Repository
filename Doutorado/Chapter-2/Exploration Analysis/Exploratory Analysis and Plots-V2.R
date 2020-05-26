#Exploratory Analysis and Plots V2
#Blowdown Data (AREA-1)
#Eduardo Q Marques 25-05-2020

library(tidyverse)
library(reshape2)
library(ggplot2)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Campo vento')

df = read.csv("storm_data_full_C.csv", sep = ",")

#Resume data
df = df[,c(3,4,5,6,7,8,9,10,11,12)]
colnames(df) = c("Specie","Treatment","Line","Transect","Scar?","Alt Scar","Wind?","Direction","Damage Type","Alt Broken")
df$Number of trees = 1

#Summary information
summary(df)

#Plot data
#Most popular plot down on wind
ggplot(df, aes(x=Treatment))+
  geom_bar(position = "dodge", fill = "darkblue", alpha = 0.5)


#Kind of damage data
ggplot(df, aes(x=Damage Type, fill = Treatment))+
  geom_bar(position = "dodge", fill = "darkblue", alpha = 0.5)

#Popular trees was broken?
#Orverview
pop_tree = df %>% 
  group_by(Specie) %>% 
  summarise(Number of trees = sum(Number of trees)) %>% 
  filter(Number of trees >= 10)

ggplot(pop_tree, aes(x=Specie, y=Number of trees))+
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkblue", alpha = 0.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45))

#By plot
plot_tree = df %>% 
  group_by(Treatment, Specie) %>% 
  summarise(Number of trees = sum(Number of trees)) %>% 
  filter(Number of trees >= 5)

ggplot(plot_tree, aes(x=Specie, y=Number of trees))+
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkblue", alpha = 0.5)+
  facet_wrap(~Treatment, scales = "free")+
  theme(axis.text.x = element_text(angle=45))

#By kind of broken
brok_tree = df %>% 
  group_by(Damage Type, Specie) %>% 
  summarise(Number of trees = sum(Number of trees)) %>% 
  filter(Number of trees >= 4)

ggplot(brok_tree, aes(x=Specie, y=Number of trees))+
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkblue", alpha = 0.5)+
  facet_wrap(~Damage Type, scales = "free")+
  theme(axis.text.x = element_text(angle=45))

#Trees with fire scar (It is the old trees) ------------------------------------------------
#How much broken? (Number of trees and percentege)
scar = df %>% 
  group_by(Scar?, Treatment) %>% 
  summarise(Number of trees = sum(Number of trees))

scar$percent = c(99,62,57,1,38,43)

ggplot(scar, aes(x=Scar?, y=Number of trees, fill = Scar?))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(x=Scar?, y=percent, label = paste0(percent,"%"))) +
  facet_wrap(~Treatment)


#Have similar alt of broken and alt of scar?
scar2 = df %>% 
  filter(Scar? == "Scar") %>% 
  filter(Damage Type == "Broken")

ggplot(scar2, aes(x=Alt Broken, y=Alt Scar))+
  geom_point()+
  stat_smooth(method = "gam")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45))

#Similarity closer to 0 is better!
scar3 = scar2 %>%
  mutate(proximity = c(Alt Broken - Alt Scar))

ggplot(scar3, aes(x=proximity))+
  geom_density(col = "black", fill = "darkblue", alpha = 0.5)+
  theme_minimal()












