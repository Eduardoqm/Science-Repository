#Make all hyperion data as data frame with XY

#Ludmila Rattis and Eduardo Q Marques 10-03-2020

library(tidyverse)
library(ggridges)
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)
library(rasterVis)

area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes/Hyperion",layer="Polygon_A_B_C_Hyperion")



setwd("~/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Hyperion")
(r1 = list.files())
r2 = lapply(r1,raster)
r3 = lapply(r2, crop, area1,snap='near')
r4 = lapply(r3,resample,r3[[114]],method='ngb')
r5 = stack(r4)

levelplot(r5[[c(seq(1,119,7))]])

# table(unlist(lapply(r3,ncell)))
# r4 = lapply(r3[-c(seq(2,119,7))],extend,r3[[114]],value=NA)
# r5 = lapply(r4,extend,c(0,1),value=NA)
# table(unlist(lapply(r4,ncell)))
# r5 = lapply(r4, crop, area1)
# table(unlist(lapply(r5,ncell)))
# r4 = lapply(r3,extend,ext,value=NA)
# 
# table(unlist(lapply(r5,ncell)))



##area1
rastA = rasterize(area1,r5[[1]],field="Parcela",background=NA)#Before change Area1 CRS

r6 = stack(r5,rastA)

df = as.data.frame(cbind(id = 1:ncell(r6),
                         as.data.frame(r6,xy=TRUE))); head(df)

(dfT = as.tbl(df) %>%
  gather("yr", "value", -c(id,x,y,layer)) %>%
    separate(yr,c("index","year")) %>%
    mutate(treat = case_when( layer == 1 ~ "control",
               layer == 2 ~ "b3yr",
               layer == 3 ~ "b1yr"),
           dist = -13.07458 - y) %>%
    drop_na(treat))


#setwd("~/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
#write.csv(dfT, "Hyperion_indexs_all_xy-B.csv")


#Test plots
df2 = dfT%>%
  group_by(index,treat,year,y) %>%
  summarise(value = mean(value,na.rm=TRUE))

pssr = df2 %>% filter(index == "pssr")

wbi = df2 %>% filter(index == "wbi")

ndii = df2 %>% filter(index == "ndii")

ndwi = df2 %>% filter(index == "ndwi")


tile_plot = function(x){
  ggplot(x, aes(year, y, fill = value))+ 
    geom_tile()+
    facet_wrap(~treat)+
    scale_fill_viridis(discrete=FALSE)
}

tile_plot(pssr)
tile_plot(wbi)
tile_plot(ndii)
tile_plot(ndwi)

