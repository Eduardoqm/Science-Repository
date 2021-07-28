#ANOVA of Hyperion Indexs - V2

#Eduardo Q Marques 27-07-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(car)

#Data preparation ------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
df$year = as.factor(df$year)

#Tukey Test function to make data frame --------------------------------------------------
tuk = function(x){
  #PostHoc analysis
  posth = TukeyHSD(x, wich = "year")
  
  #Convert to dataframe and plot
  posth2 = as.data.frame(posth[["year:treat"]])
  posth2$comp = row.names(posth2)
  posth2 = remove_rownames(posth2)
  colnames(posth2)[4] = c("pvalue")
  
  #Filter interesting comparisons (control and burned treatments)
  posth2$treat = substr(posth2$comp, 19, 22)
  posth2$year = substr(posth2$comp, 14, 17)
  posth2$index = a[1,6]
  posth3 = posth2 %>% 
    filter(comp %in% c("2004:control-2004:b1yr", "2004:control-2004:b3yr",
                       "2005:control-2005:b1yr", "2005:control-2005:b3yr",
                       "2006:control-2006:b1yr", "2006:control-2006:b3yr",
                       "2008:control-2008:b1yr", "2008:control-2008:b3yr",
                       "2010:control-2010:b1yr", "2010:control-2010:b3yr",
                       "2011:control-2011:b1yr", "2011:control-2011:b3yr",
                       "2012:control-2012:b1yr", "2012:control-2012:b3yr"))
  posth3 = posth3[,c(6,7,8,4)]
}


#Structural Indices ----------------------------------------------------------------------
a = df %>% filter(index == "evi2") %>% na.omit()
evi = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "ndvi") %>% na.omit()
ndvi = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "vig") %>% na.omit()
vig = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "vari") %>% na.omit()
vari = tuk(aov(value~year:treat, data = a, paried = F))

struc = rbind(evi, ndvi, vari, vig)

x11()
ggplot(struc, aes(x=year, y=pvalue, color = treat, shape = treat))+
  geom_point(size = 10, alpha = 0.7)+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  facet_wrap(~index, scales="free")+
  scale_color_manual(values = c("darkorange", "red"))+
  theme_bw()


#Biochemistry Indices --------------------------------------------------------------------
a = df %>% filter(index == "lwvi2") %>% na.omit()
lwvi2 = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "msi") %>% na.omit()
msi = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "ndii") %>% na.omit()
ndii = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "ndwi") %>% na.omit()
ndwi = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "nirv") %>% na.omit()
nirv = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "psri") %>% na.omit()
psri = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "pssr") %>% na.omit()
pssr = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "sipi") %>% na.omit()
sipi = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "wbi") %>% na.omit()
wbi = tuk(aov(value~year:treat, data = a, paried = F))

bioc = rbind(lwvi2, msi, ndii, ndwi, nirv, psri, pssr, sipi, wbi)

x11()
ggplot(bioc, aes(x=year, y=pvalue, color = treat, shape = treat))+
  geom_point(size = 10, alpha = 0.7)+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  facet_wrap(~index, scales="free")+
  scale_color_manual(values = c("darkorange", "red"))+
  theme_bw()


#Physiologic Indices ---------------------------------------------------------------------
a = df %>% filter(index == "pri") %>% na.omit()
pri = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "rendvi") %>% na.omit()
rendvi = tuk(aov(value~year:treat, data = a, paried = F))

phy = rbind(pri, rendvi)

x11()
ggplot(phy, aes(x=year, y=pvalue, color = treat, shape = treat))+
  geom_point(size = 10, alpha = 0.7)+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  facet_wrap(~index, scales="free")+
  scale_color_manual(values = c("darkorange", "red"))+
  theme_bw()


#Fire Indices ----------------------------------------------------------------------------
a = df %>% filter(index == "nbr") %>% na.omit()
nbr = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "nbr2") %>% na.omit()
nbr2 = tuk(aov(value~year:treat, data = a, paried = F))

fire = rbind(nbr, nbr2)

x11()
ggplot(fire, aes(x=year, y=pvalue, color = treat, shape = treat))+
  geom_point(size = 10, alpha = 0.7)+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  facet_wrap(~index, scales="free")+
  scale_color_manual(values = c("darkorange", "red"))+
  theme_bw()






