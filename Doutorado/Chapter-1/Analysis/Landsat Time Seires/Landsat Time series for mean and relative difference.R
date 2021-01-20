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

eqm = c("red", "orange", "blue")

#Mean time series =================================================
ndvi_m = ndvi %>% 
  group_by(year, treat) %>% 
  summarise(value = mean(value))

ggplot(ndvi_m, aes(x=year, y=value, color = treat))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.005)+
  annotate("text", x = 2007.5, y = 0.85, size = 4, label = "Fire experiment period")+
  geom_line(aes(group = treat), size = 1.5, alpha = 0.8)+
  theme_light()+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI-mean")
#theme(axis.text.x = element_text(angle = 90))

#Diference by sbtraction ===========================================
#Calculate difference in relation of control
ndvi_crt = filter(ndvi_m, treat == "control")
ndvi_b3yr = filter(ndvi_m, treat == "b3yr")
ndvi_b1yr = filter(ndvi_m, treat == "b1yr")

ndvi_b3yr$value = ndvi_b3yr$value - ndvi_crt$value
ndvi_b1yr$value = ndvi_b1yr$value - ndvi_crt$value
ndvi_diff = rbind(ndvi_b3yr, ndvi_b1yr)

ggplot(ndvi_diff, aes(x=year, y=value, color = treat))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "blue", color = NA, alpha = 0.01)+
  annotate("text", x = 2007.5, y = 0.01, size = 4, label = "Fire experiment period")+
  geom_line(aes(group = treat), size = 1.5, alpha = 0.8)+
  theme_light()+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI-difference by control")
#theme(axis.text.x = element_text(angle = 90))

