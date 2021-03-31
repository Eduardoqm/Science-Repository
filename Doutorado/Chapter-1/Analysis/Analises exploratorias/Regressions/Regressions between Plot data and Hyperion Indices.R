#Regression between Plot data and Hyperion Indices

#Eduardo Q Marques 30-03-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggvis)
library(mgcv)
library(visreg)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Landsat =========================================================
#Data ------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
lai = read.csv("LAI_full_tang.csv", sep = ',')
litt = read.csv("Liteira_full_tang.csv", sep = ',')
bmas = read.csv("Biomass_full_tang.csv", sep = ',')

#Modify and filter date to match ---------------------------------
df = df %>% 
  na.omit() %>% 
  unite("id", c("parcela", "year"), sep = "_")

df2 = df[,c(4,5,6)]
df2 = df2 %>%
  group_by(id, index) %>% 
  summarise(value = mean(value))
df2 = df2 %>%
  filter(index %in% c("pssr", "vig", "evi2", "rendvi", "ndwi", "msi"))

df2$index = as.character(df2$index)
df2$index[df2$index == "evi2"] <- c("EVI")
df2$index[df2$index == "ndwi"] <- c("NDWI")
df2$index[df2$index == "rendvi"] <- c("RENDVI")
df2$index[df2$index == "vig"] <- c("VIG")
df2$index[df2$index == "pssr"] <- c("PSSR")
df2$index[df2$index == "msi"] <- c("MSI")

lai = lai %>% 
  unite("id", c("parcela", "year"), sep = "_")

lai2 = lai[,c(3,4)]
lai2 = lai2 %>% 
  group_by(id) %>% 
  summarise(lai = mean(lai))


litt = litt %>% 
  unite("id", c("parcela", "year"), sep = "_")

litt2 = litt[,c(1,7)]
litt2 = litt2 %>% 
  group_by(id) %>% 
  summarise(lit_ton_hec = mean(lit_ton_hec))


bmas = bmas %>% 
  unite("id", c("parcela", "data"), sep = "_")

bmas2 = bmas[,c(1,4)]
bmas2 = bmas2 %>% 
  group_by(id) %>% 
  summarise(biomass = sum(biomass))

#Join Data ---------------------------------------------------------
df_lai = full_join(df2, lai2, by = "id")
df_lai = na.omit(df_lai)

df_litt = full_join(df2, litt2, by = "id")
df_litt = na.omit(df_litt)

df_bmas = full_join(df2, bmas2, by = "id")
df_bmas = na.omit(df_bmas)

#Plot correlation --------------------------------------------------
a = ggplot(df_lai, aes(x=value, y=lai))+
  geom_point(size = 3, col = "red")+
  geom_smooth(method="lm", col = "red")+ 
  facet_grid(cols = vars(index), scales = "free")+
  #facet_wrap(~index, scales = "free")+
  stat_cor(show.legend = F)+
  theme_bw()+
  theme(axis.text.y = element_blank(), axis.ticks = element_blank())+
  labs(x = "", y = "LAI")+
  theme(text = element_text(family = "Times New Roman", size = 14))

b = ggplot(df_litt, aes(x=value, y=lit_ton_hec))+
  geom_point(size = 3, col = "orange")+
  geom_smooth(method="lm", col = "orange")+ 
  facet_grid(cols = vars(index), scales = "free")+
  #facet_wrap(~index, scales = "free")+
  stat_cor(show.legend = F)+
  theme_bw()+
  theme(axis.text.y = element_blank(), axis.ticks = element_blank())+
  labs(x = "", y = "Liteira (T/ha)")+
  theme(text = element_text(family = "Times New Roman", size = 14))

c = ggplot(df_bmas, aes(x=value, y=biomass))+
  geom_point(size = 3, col = "darkgreen")+
  geom_smooth(method="lm", col = "darkgreen")+ 
  facet_grid(cols = vars(index), scales = "free")+
  #facet_wrap(~index, scales = "free")+
  stat_cor(show.legend = F)+
  theme_bw()+
  theme(axis.text.y = element_blank(), axis.ticks = element_blank())+
  labs(x = "", y = "Biomassa (T/tratamento)")+
  theme(text = element_text(family = "Times New Roman", size = 14))


ggarrange(a, b, c,
          common.legend = TRUE,
          legend="bottom",
          ncol = 1, nrow = 3)








