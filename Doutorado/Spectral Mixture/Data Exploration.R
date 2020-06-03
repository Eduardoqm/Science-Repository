library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggridges)
library(ggpubr)


setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Fraction_Landsat/Mistura Espectral')

df = read.csv("sma.csv", sep = ",")

df$id = as.factor(df$id)
#df$id = as.numeric(df$id)
#df$value = as.numeric(df$value)

#Exploration data ============================================================================
#ggplot(df, aes(x=value, y=id, col=plot))+
# geom_boxplot()
#eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)
#eqm = c("darkred", "red", "darkblue")
#eqm = c("darkblue", "orange", "darkgreen")
eqm = c("darkred", "orange", "darkblue")

a = ggplot(df, aes(x=value, y=id, fill=plot))+
  geom_density_ridges(alpha = 0.25)+
  facet_wrap(~class, scales="free")+
  theme_minimal()

fract = ggpar(a, palette = eqm)
fract

#NPV
npv = df %>% 
  filter(class == "Non-Photosynthetic Vegetation")

#Boxplot
a = ggplot(npv, aes(x=id, y=value, fill = plot))+
  geom_boxplot(alpha = 0.35)+
  theme_minimal()

npv_box = ggpar(a, palette = eqm)
npv_box 

#Violin
a = ggplot(npv, aes(x=id, y=value, fill = plot))+
  geom_violin(alpha = 0.35)+
  theme_minimal()

npv_vio = ggpar(a, palette = eqm)
npv_vio 

#Density
a = ggplot(npv, aes(x=value, y=id, fill=plot))+
  geom_density_ridges(alpha = 0.25)+
  theme_minimal()

npv = ggpar(a, palette = eqm)
npv 

#Fire Experiment
npv2 = npv %>% 
  filter(id %in% c(2000, 2014, 2018))

a = ggplot(npv2, aes(x=value, y=id, fill=plot))+
  geom_density_ridges(alpha = 0.25)+
  theme_minimal()

npv2 = ggpar(a, palette = eqm)
npv2 
  
#Blowdown
npv3 = npv %>% 
  filter(id %in% c(2018, 2019))

a = ggplot(npv3, aes(x=value, y=id, fill=plot))+
  geom_density_ridges(alpha = 0.5)+
  facet_wrap(~plot, scales = "free")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1))
  

npv3 = ggpar(a, palette = eqm)
npv3 
