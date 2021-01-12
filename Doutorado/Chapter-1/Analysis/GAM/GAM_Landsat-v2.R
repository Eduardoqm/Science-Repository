######################################################
# Generalized Additive Models to Landsat time series #
#                                                    #
# Eduardo Q Marques 09-12-2020                       #
######################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)
library(visreg)

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify data =====================================================
df$year = substr(df$year, 1,4)
df$year = as.numeric(df$year)

#NDVI
ndvi = df %>% 
  filter(index == "ndvi")

evi = df %>% 
  filter(index == "evi2")

ndii = df %>% 
  filter(index == "ndii")

vig = df %>% 
  filter(index == "vig")

#Running a Generalized Additive Model just to NDVI ================
ndvig = gam(value~s(year, k = 20, by = treat), data = ndvi, method = "REML", family = binomial("identity"))
evig = gam(value~s(year, k = 30, by = treat), data = evi, method = "REML")
ndiig = gam(value~s(year, k = 25, by = treat), data = ndii, method = "REML")
vigg = gam(value~s(year, k = 30, by = treat), data = vig, method = "REML")

summary(ndvig)
summary(evig)
summary(ndiig)
summary(vigg)
#p-value: <2e-16 

gam.check(ndvig)
gam.check(evig)
gam.check(ndiig)
gam.check(vigg)

#Plot GAM =========================================================
eqm = c("red", "orange", "blue")

a = visreg(ndvig, "year", by = "treat", overlay = T,
           partial = F)

ndvi_plt = ggplot(a$fit, aes(year, visregFit, col = treat, fill = treat))+
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.1) +
  geom_line(size = 1, aplha = 0.5)+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  ggtitle("NDVI")+
  ylab("Index")+
  theme_light()

a = visreg(evig, "year", by = "treat", overlay = T,
           partial = F)

evi_plt = ggplot(a$fit, aes(year, visregFit, col = treat, fill = treat))+
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.1) +
  geom_line(size = 1, aplha = 0.5)+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  ggtitle("EVI")+
  ylab("Index")+
  theme_light()

a = visreg(ndiig, "year", by = "treat", overlay = T,
           partial = F)

ndii_plt = ggplot(a$fit, aes(year, visregFit, col = treat, fill = treat))+
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.1) +
  geom_line(size = 1, aplha = 0.5)+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  ggtitle("NDII")+
  ylab("Index")+
  theme_light()


a = visreg(vigg, "year", by = "treat", overlay = T,
           partial = F)

vig_plt = ggplot(a$fit, aes(year, visregFit, col = treat, fill = treat))+
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.1) +
  geom_line(size = 1, aplha = 0.5)+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  ggtitle("VIG")+
  ylab("Index")+
  theme_light()

ndvi_plt
evi_plt
ndii_plt
vig_plt








ndvig = gam(value~s(year, k = 25, by = treat), data = ndvi, method = "REML", family = binomial("identity"))


summary(ndvig)

gam.check(ndvig)

x11()
plot(ndvig, pages = 1, shade = T, shade.col = "orange",
     shift = coef(ndvig)[1]) #With values of ndvi


a = visreg(ndvig, "year", by = "treat", overlay = T,
           partial = F)

ggplot(a$fit, aes(year, visregFit, col = treat))+
  #geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.1) +
  geom_line(size = 1.5, aplha = 0.8)+
  scale_color_manual(values = eqm)+
  ggtitle("NDVI")+
  ylab("Index")+
  theme_light()

