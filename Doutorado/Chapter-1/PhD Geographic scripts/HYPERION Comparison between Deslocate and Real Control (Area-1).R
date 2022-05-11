#Comparison between Deslocate and Real Control (Area-1)

#Eduardo Q Marques 22-10-2020

library(ggridges)
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)
library(ggpubr)
library(GGally)
library(rasterVis)

area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes/Hyperion",layer="Polygon_A_B_C_Hyperion")

area1b <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

setwd("~/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Hyperion")
(r1 = list.files())
r2 = lapply(r1,raster)

#Deslocate control extract
r3 = lapply(r2, crop, area1[3,],snap='near')
r4 = lapply(r3,resample,r3[[114]],method='ngb')
r5 = stack(r4)

#Real control extract
r3b = lapply(r2, crop, area1b[1,],snap='near')
r4b = lapply(r3b,resample,r3b[[114]],method='ngb')
r5b = stack(r4b)

#Select 2004 and 2005
#False control
hyc2004f = r5[[c(seq(1,119,7))]]
hyc2005f = r5[[c(seq(13,119,7))]]
levelplot(hyc2004f)
levelplot(hyc2005f)

#Real control
hyc2004r = r5b[[c(seq(1,119,7))]]
hyc2005r = r5b[[c(seq(13,119,7))]]
levelplot(hyc2004r)
levelplot(hyc2005r)

#Extract and comparison data
#False control
hy04f = as.data.frame(hyc2004f)
hy04f = melt(hy04f)
hy04f$treat = c("Deslocated")

hy05f = as.data.frame(hyc2005f)
hy05f = melt(hy05f)
hy05f$treat = c("Deslocated")

#Real control
hy04r = as.data.frame(hyc2004r)
hy04r = melt(hy04r)
hy04r$treat = c("Real")

hy05r = as.data.frame(hyc2005r)
hy05r = melt(hy05r)
hy05r$treat = c("Real")

#Glue
hy04 = rbind(hy04f, hy04r)
hy04 = filter(hy04, variable != "evi.2004")
hy04 = filter(hy04, variable != "ari.2004")

hy05 = rbind(hy05f, hy05r)
hy05 = filter(hy05, variable != "evi.2011")
hy05 = filter(hy05, variable != "ari.2011")

hyfull = rbind(hy04, hy05)
hyfull = hyfull %>% 
  separate(variable, c("index","year"))


#Plots of Density
eqm = c("yellow", "blue")

ggplot(hy04, aes(x = value, y = variable, fill=treat)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~variable, scales="free") +
  theme_minimal()

ggplot(hy05, aes(x = value, y = variable, fill=treat)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~variable, scales="free") +
  theme_minimal()

ggplot(hyfull, aes(x = value, y = year, fill=treat)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)

#Boxplot
ggplot(hyfull, aes(x = year, y = value, fill=treat)) +
  geom_boxplot(alpha = 0.55) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)


#Correlation this years

hyfull2 = hyfull %>% 
  unite(col=id, c("index", "treat"), sep = "-", remove = F)

eqm2 = c("orange", "blue")

corr = function(x, y){
  r = hyfull2 %>% 
    filter(index == x) %>% 
    filter(treat == "Real")
  
  f = hyfull2 %>% 
    filter(index == x) %>% 
    filter(treat == "Deslocated")
  
  df = cbind(r, f)
  df = df[,c(3,4,9)]
  colnames(df) = c("year", "real", "deslocated")
  
  ggplot(df, aes(x=real, y=deslocated, col = year))+
    geom_point(size=3, alpha = 0.3)+
    geom_smooth(method="lm", se=F)+ 
    stat_cor(show.legend = F)+
    theme_minimal()+
    ggtitle(x)+
    theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
    scale_color_manual(values = eqm2)
}

corr("evi2")
corr("ndvi")
corr("vari")
corr("vig")
corr("lwvi2")
corr("msi")
corr("ndii")
corr("ndwi")
corr("nirv")
corr("psri")
corr("pssr")
corr("sipi")
corr("wbi")
corr("pri")
corr("rendvi")



ndvi04 = hy04 %>% 
  filter(variable == "ndvi.2004")

ndvi04$treat[ndvi04$treat == "Deslocated"] = c("New Control")
ndvi04$treat[ndvi04$treat == "Real"] = c("Control")
colnames(ndvi04) = c("year", "value", "Treatment")

a = ggplot(ndvi04, aes(value, fill = Treatment))+
  geom_density(alpha = 0.7)+
  labs(y = NULL, x = "Values of NDVI")+
  scale_fill_manual(values = c("yellow", "red"))+
  theme_bw()+
  theme(legend.position = c(0.2, 0.5))
  
ndvi11 = hy05 %>% 
  filter(variable == "ndvi.2011")

ndvi11$treat[ndvi11$treat == "Deslocated"] = c("New Control")
ndvi11$treat[ndvi11$treat == "Real"] = c("Control")
colnames(ndvi11) = c("year", "value", "Treatment")

b = ggplot(ndvi11, aes(value, fill = Treatment))+
  geom_density(alpha = 0.7)+
  labs(y = NULL, x = "Values of NDVI")+
  scale_fill_manual(values = c("yellow", "red"))+
  theme_bw()+
  theme(legend.position = c(0.2, 0.5))

ggarrange(a, b, ncol = 1)



ggsave(filename = "NDVI_comparison2004.png", plot = a,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Diferença entre os controles (Hyperion)",
       width = 10, height = 10, units = "cm", dpi = 300)


ggsave(filename = "NDVI_comparison2011.png", plot = b,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Diferença entre os controles (Hyperion)",
       width = 10, height = 10, units = "cm", dpi = 300)





