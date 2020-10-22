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
library(rasterVis)

area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/shapes/Hyperion",layer="Polygon_A_B_C_Hyperion")

area1b <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

setwd("~/My Jobs/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Hyperion")
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
hyc2005f = r5[[c(seq(2,119,7))]]
levelplot(hyc2004f)
levelplot(hyc2005f)

#Real control
hyc2004r = r5b[[c(seq(1,119,7))]]
hyc2005r = r5b[[c(seq(2,119,7))]]
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




