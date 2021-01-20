########################################################
# Landsat Time series for mean and relative difference #
#                                                      #
# Eduardo Q Marques 20-01-2021                         #
########################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)
library(visreg)

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify data =====================================================
df$year = substr(df$year, 1,4)
df$year = as.numeric(df$year)

#NDVI
ndvi = df %>% 
  filter(index == "ndvi")

eqm = c("orange", "red", "blue")

#Mean time series =================================================
ndvi_m = ndvi %>% 
  group_by(year, treat) %>% 
  summarise(value = mean(value))
colnames(ndvi_m) = c("Data", "Tratamento", "Valor")

ggplot(ndvi_m, aes(x=Data, y=Valor, color = Tratamento))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.005)+
  annotate("text", x = 2007.5, y = 0.85, size = 4, label = "Fire period")+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.7)+
  theme_bw()+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI")

#Diference by sbtraction ===========================================
#Calculate difference in relation of control
ndvi_crt = filter(ndvi_m, Tratamento == "control")
ndvi_b3yr = filter(ndvi_m, Tratamento == "b3yr")
ndvi_b1yr = filter(ndvi_m, Tratamento == "b1yr")

ndvi_b3yr$Valor = ndvi_b3yr$Valor - ndvi_crt$Valor
ndvi_b1yr$Valor = ndvi_b1yr$Valor - ndvi_crt$Valor
ndvi_diff = rbind(ndvi_b3yr, ndvi_b1yr)

ggplot(ndvi_diff, aes(x=Data, y=Valor, color = Tratamento))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.01)+
  annotate("text", x = 2007.5, y = 0.01, size = 4, label = "Fire experiment period")+
  geom_line(aes(group = Tratamento), size = 1.5, alpha = 0.8)+
  theme_light()+
  geom_hline(yintercept = 0)+
  scale_color_manual(Valors = eqm)+
  ggtitle("NDVI-difference by control")
#theme(axis.text.x = element_text(angle = 90))
