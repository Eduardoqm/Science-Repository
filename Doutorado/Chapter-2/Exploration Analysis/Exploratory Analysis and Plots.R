#Exploratory Analysis and Plots
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
df$count = 1

#Summary information
summary(df)

#Plot data
#Most popular plot down on wind
ggplot(df, aes(x=parcela))+
  geom_bar(position = "dodge")


#Kind of damage data
ggplot(df, aes(x=tipo_de_dano, fill = parcela))+
  geom_bar(position = "dodge")

#Popular trees was broken?
#Orverview
pop_tree = df %>% 
  group_by(nomecomum) %>% 
  summarise(count = sum(count)) %>% 
  filter(count >= 10)

ggplot(pop_tree, aes(x=nomecomum, y=count))+
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkblue", alpha = 0.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45))
 
#By plot
plot_tree = df %>% 
  group_by(parcela, nomecomum) %>% 
  summarise(count = sum(count)) %>% 
  filter(count >= 5)

ggplot(plot_tree, aes(x=nomecomum, y=count))+
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkblue", alpha = 0.5)+
  facet_wrap(~parcela, scales = "free")+
  theme(axis.text.x = element_text(angle=45))

#By kind of broken
brok_tree = df %>% 
  group_by(tipo_de_dano, nomecomum) %>% 
  summarise(count = sum(count)) %>% 
  filter(count >= 4)

ggplot(brok_tree, aes(x=nomecomum, y=count))+
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkblue", alpha = 0.5)+
  facet_wrap(~tipo_de_dano, scales = "free")+
  theme(axis.text.x = element_text(angle=45))

#Trees with fire scar (It is the old trees) ------------------------------------------------
#How much broken? (Number of trees and percentege)
scar = df %>% 
  group_by(cicatriz, parcela) %>% 
  summarise(count = sum(count))

scar$percent = c(99,62,57,1,38,43)

ggplot(scar, aes(x=cicatriz, y=count, fill = cicatriz))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(x=cicatriz, y=percent, label = paste0(percent,"%"))) +
  facet_wrap(~parcela)


#Have similar alt of broken and alt of scar?
scar2 = df %>% 
  filter(cicatriz == "Scar") %>% 
  filter(tipo_de_dano == "Broken")

ggplot(scar2, aes(x=altura_quebra, y=alt_cic))+
  geom_point()+
  stat_smooth(method = "gam")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45))

#Similarity closer to 0 is better!
scar3 = scar2 %>%
  mutate(proximity = c(altura_quebra - alt_cic))

ggplot(scar3, aes(x=proximity))+
  geom_density(col = "black", fill = "darkblue", alpha = 0.5)+
  theme_minimal()












