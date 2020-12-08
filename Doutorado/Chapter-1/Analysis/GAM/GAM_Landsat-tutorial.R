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
#NDVI
df$year = as.numeric(df$year)
ndvi = df %>% 
  filter(index == "ndvi")

land = gam(value~s(year, by = treat), data = ndvi, method = "REML")
#land = gam(value~s(year, k = 15, by = treat), data = ndvi, method = "REML")
#---------------------------------------------------------
#REML = Restricted Maximum Likelihood 
#K =  numbers of basis function to make smooth
#---------------------------------------------------------

summary(land)
#---------------------------------------------------------
#Parametric coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.7777836  0.0001578    4930   <2e-16 *** 

#Approximate significance of smooth terms:
#                       edf Ref.df       F p-value    
#s(year):treatb1yr    8.895  8.997 1151.65  <2e-16 ***
#s(year):treatb3yr    8.951  8.999 1390.55  <2e-16 ***
#s(year):treatcontrol 8.857  8.994   97.41  <2e-16 ***

#edf values high is smoother and low is more linear.
#The Ref.df and F are test used in an ANOVA to test overall significance of the smooth.
#p-value response of this model are significant.
#---------------------------------------------------------

#Plots tests
x11()
plot(land, residuals = T, pch = 1, cex = 1, pages = 1)
plot(land, pages = 1)
plot(land, pages = 1, shade = T)
plot(land, pages = 1, shade = T, shade.col = "orange")
plot(land, pages = 1, shade = T, shade.col = "orange", seWithMean = T)
plot(land, pages = 1, shade = T, shade.col = "orange",
     shift = coef(land)[1]) #With values of ndvi













  
  
  
  
  
  