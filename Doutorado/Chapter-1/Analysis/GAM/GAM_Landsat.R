######################################################
# Generalized Additive Models to Landsat time series #
#                                                    #
# Eduardo Q Marques 08-12-2020                       #
######################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)

#Data ========================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify data =================================================================================
df$year = substr(df$year, 1,4)

#Visualize data =============================================================================
eqm = c("red", "orange", "blue")

#Boxplot
ggplot(df, aes(x=year, y=value, fill = treat))+
  geom_boxplot()+
  facet_wrap(~index, scales = "free")+
  theme_light()+
  scale_fill_manual(values = eqm)+
  theme(axis.text.x = element_text(angle = 90))

#Smooth with ggplot default GAM
ggplot(df, aes(x=year, y=value, color = treat))+
  geom_smooth(aes(group = treat))+
  facet_wrap(~index, scales = "free")+
  theme_light()+
  scale_color_manual(values = eqm)+
  theme(axis.text.x = element_text(angle = 90))


#Running a Generalized Additive Model =======================================================
df$year = as.numeric(df$year)
ndvi = df %>% 
  filter(index == "ndvi")

#land = gam(value~s(year, k=16), data = ndvi, method = "REML")
#land = gam(value~s(year), data = ndvi, method = "REML")
land = gam(value~s(year, by = treat), data = ndvi, method = "REML")


summary(land)
plot(land, pages = 1)
#plot(land, residuals = T, pch = 1, cex = 1)

#REML = Restricted Maximum Likelihood 
#K =  numbers of basis function to make smooth

  
  
  
  
  
  