######################################################
# Generalized Additive Models tutorial and tests     #
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


#Running a Generalized Additive Model just to NDVI ==========================================
df$year = as.numeric(df$year)
ndvi = df %>% 
  filter(index == "ndvi")

land = gam(value~s(year, by = treat), data = ndvi, method = "REML")
#land = gam(value~s(year, k = 30, by = treat), data = ndvi, method = "REML")
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

#Plots tests ===============================================================================
x11()
plot(land, residuals = T, pch = 1, cex = 1, pages = 1)
plot(land, pages = 1)
plot(land, pages = 1, shade = T)
plot(land, pages = 1, shade = T, shade.col = "orange")
plot(land, pages = 1, shade = T, shade.col = "orange", seWithMean = T)
plot(land, pages = 1, shade = T, shade.col = "orange",
     shift = coef(land)[1]) #With values of ndvi


#Model check with gam,check ================================================================
gam.check(land)
#--------------------------------------------------------
#Basis dimension (k) checking results. Low p-value (k-index<1) may
#indicate that k is too low, especially if edf is close to k'.

#                       k'  edf k-index p-value    
#s(year):treatb1yr    9.00 8.90    0.92  <2e-16 ***
#s(year):treatb3yr    9.00 8.95    0.92  <2e-16 ***
#s(year):treatcontrol 9.00 8.86    0.92  <2e-16 ***

#Here the p-value need to be >0.05, so I need to increase the k value of my model
#--------------------------------------------------------

#Update GAM with increase k value
land = gam(value~s(year, k = 33, by = treat), data = ndvi, method = "REML")

X11()
plot(land, pages = 1, shade = T, shade.col = "orange",
     shift = coef(land)[1]) #With values of ndvi

summary(land)

gam.check(land)
#--------------------------------------------------------
#Basis dimension (k) checking results. Low p-value (k-index<1) may
#indicate that k is too low, especially if edf is close to k'.

#                       k'  edf k-index p-value
#s(year):treatb1yr    32.0 31.7    1.01    0.66
#s(year):treatb3yr    32.0 31.8    1.01    0.70
#s(year):treatcontrol 32.0 31.3    1.01    0.68

#Now p-value is >0.05
#--------------------------------------------------------
  
#Checking concurvity ======================================================================
concurvity(land, full = T)
concurvity(land, full = F)
#--------------------------------------------------------
#First we test with full = TRUE, if some value be over 0.8
#we use full = FALSE to see the matrix

#Just be atention in the interpretion for models with values over 0.8
#--------------------------------------------------------

#Test new plots ============================================================================
#First plot form
X11()
plot(land, pages = 1, shade = T, shade.col = "orange",
     shift = coef(land)[1]) #With values of ndvi

#New form
library(visreg)

visreg(land)
visreg(land, overlay = T)
visreg(land, "year", overlay = T)
visreg(land, "year", by = "treat")
visreg(land, "year", by = "treat", overlay = T)

visreg(land, "year", by = "treat", overlay = T,
       partial = F)

visreg(land, "year", by = "treat", overlay = T,
       gg = T, partial = F)

visreg(land, "year", by = "treat", overlay = T,
       gg = T, partial = T)+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)


visreg(land, "year", by = "treat", overlay = T,
           partial = F, line = list(col=c("red", "orange", "blue")))


visreg(land, "year", by = "treat", overlay = T,
       partial = F,
       line = list(col=eqm))

visreg(land, "year", by = "treat", overlay = T,
       partial = F,
       line = list(col=eqm),
       fill = list(col="gray"))


#Transform in a GGPLOT =====================================================================
g = visreg(land, "year", by = "treat", overlay = T,
           partial = F)

ggplot(g$fit, aes(year, visregFit, col = treat, fill = treat))+
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.1) +
  geom_line(size = 1, aplha = 0.5)+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  ggtitle("NDVI")+
  ylab("Index")+
  theme_light()
  
  
#Comparioson GAM and GGPLOT GAM ============================================================
ggplot(ndvi, aes(x=year, y=value, color = treat))+
  geom_smooth(aes(group = treat))+
  theme_light()+
  scale_color_manual(values = eqm)+
  theme(axis.text.x = element_text(angle = 90))











