#ANOVA results in Hyperion Density plots

#Eduardo Q Marques 28-07-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(rstatix)
library(car)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

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

#Physiologic Indices ---------------------------------------------------------------------
a = df %>% filter(index == "pri") %>% na.omit()
pri = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "rendvi") %>% na.omit()
rendvi = tuk(aov(value~year:treat, data = a, paried = F))

phy = rbind(pri, rendvi)

#Fire Indices ----------------------------------------------------------------------------
a = df %>% filter(index == "nbr") %>% na.omit()
nbr = tuk(aov(value~year:treat, data = a, paried = F))
a = df %>% filter(index == "nbr2") %>% na.omit()
nbr2 = tuk(aov(value~year:treat, data = a, paried = F))

fire = rbind(nbr, nbr2)

#Join every comparisons and join with indexs data ----------------------------------------
comp = rbind(struc, bioc, phy, fire)
comp2 = comp %>% 
  unite(id, treat, year, index, sep = "_")

df2 = df %>% 
  unite(id, treat, year, index, sep = "_")

df3 = full_join(df2, comp2, by = "id")
df3 = df3 %>%
  separate(id, c("treat", "year", "index"), sep = "_")

#Density plots ---------------------------------------------------------------------------
df = df3
df$year = as.character(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Change names format
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "vig"] <- c("VIG")
df$index[df$index == "vari"] <- c("VARI")
df$index[df$index == "nirv"] <- c("NIRv")
df$index[df$index == "lwvi2"] <- c("LWVI2")
df$index[df$index == "msi"] <- c("MSI")
df$index[df$index == "ndwi"] <- c("NDWI")
df$index[df$index == "pssr"] <- c("PSSR")
df$index[df$index == "psri"] <- c("PSRI")
df$index[df$index == "sipi"] <- c("SIPI")
df$index[df$index == "wbi"] <- c("WBI")
df$index[df$index == "pri"] <- c("PRI")
df$index[df$index == "rendvi"] <- c("RENDVI")
df$index[df$index == "nbr"] <- c("NBR")
df$index[df$index == "nbr2"] <- c("NBR2")


df$treat[df$treat == "control"] <- c("Controle")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")

colnames(df)[6] = c("Parcela")

df$pvalueb1[df$Parcela == "B1yr" & df$pvalue <= 0.05] = c("*")
df$pvalueb3[df$Parcela == "B3yr" & df$pvalue <= 0.05] = c("*")

eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)
#eqm = c("orange", "red", "blue")

#Trying ggarrange ------------------------------------------------------------------------
ggdens = function(z){
  x = df %>% filter(index %in% c(z))
  ggplot(x, aes(x = value, y = year, fill=Parcela))+
    geom_density_ridges(alpha = 0.30)+
    geom_text(aes(color=Parcela, label = pvalueb1),
              x = min(x$value, na.rm=T), hjust = -1, size = 7)+
    geom_text(aes(color=Parcela, label = pvalueb3),
              x = min(x$value, na.rm=T), size = 7)+
    ggtitle(z)+
    theme_minimal()+
    scale_fill_manual(values = eqm)+
    scale_color_manual(values = eqm)+
    theme(text = element_text(family = "Times New Roman", size = 14),
          plot.title = element_text(size = 12, hjust = 0.5))
}

evi = ggdens("EVI")
ndvi = ggdens("NDVI")
vari = ggdens("VARI")
vig = ggdens("VIG")
ndii = ggdens("NDII")
nirv = ggdens("NIRv")
lwvi2 = ggdens("LWVI2")
msi = ggdens("MSI")
ndwi = ggdens("NDWI")
pssr = ggdens("PSSR")
psri = ggdens("PSRI")
sipi = ggdens("SIPI")
wbi = ggdens("WBI")
pri = ggdens("PRI")
rendvi = ggdens("RENDVI")
nbr = ggdens("NBR")
nbr2 = ggdens("NBR2")

#Structural ------------------------------------------------------------------------------
struc = ggarrange(evi+rremove("xlab")+rremove("ylab"),
                  ndvi+rremove("xlab")+rremove("ylab"),
                  vari+rremove("xlab")+rremove("ylab"),
                  vig+rremove("xlab")+rremove("ylab"),
                  common.legend = TRUE,
                  legend="right",
                  ncol = 2, nrow = 2)
struc

#Biochemistry ----------------------------------------------------------------------------
bioc = ggarrange(lwvi2+rremove("xlab")+rremove("ylab"),
                 msi+rremove("xlab")+rremove("ylab"),
                 ndii+rremove("xlab")+rremove("ylab"),
                 ndwi+rremove("xlab")+rremove("ylab"),
                 pssr+rremove("xlab")+rremove("ylab"),
                 psri+rremove("xlab")+rremove("ylab"),
                 sipi+rremove("xlab")+rremove("ylab"),
                 wbi+rremove("xlab")+rremove("ylab"),
                 nirv+rremove("xlab")+rremove("ylab"),
                 common.legend = TRUE,
                 legend="right",
                 ncol = 3, nrow = 3)
bioc

#Physiologic -----------------------------------------------------------------------------
phy = ggarrange(pri+rremove("xlab")+rremove("ylab"),
                rendvi+rremove("xlab")+rremove("ylab"),
                common.legend = TRUE,
                legend="right",
                ncol = 1, nrow = 2)
phy

#Fire ------------------------------------------------------------------------------------
fire = ggarrange(nbr+rremove("xlab")+rremove("ylab"),
                nbr2+rremove("xlab")+rremove("ylab"),
                common.legend = TRUE,
                legend="right",
                ncol = 1, nrow = 2)
fire





















