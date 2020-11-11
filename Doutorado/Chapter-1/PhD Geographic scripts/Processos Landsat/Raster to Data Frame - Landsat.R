#Make all Landsat data as data frame with XY

#Eduardo Q Marques 11-11-2020

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

area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/shapes/Hyperion",layer="Polygon_A_B_C_Hyperion")



setwd("~/My Jobs/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Landsat/All Indices")
(r1 = list.files())
r2 = lapply(r1,raster)

area1 = spTransform(area1, crs(r2[[1]]))
r3 = lapply(r2, crop, area1,snap='near')
#r4 = lapply(r3,resample,r3[[114]],method='ngb')
r5 = stack(r3)

#levelplot(r5)


##area1
#Raterize Area-1 to extract info by pixel
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













#Save the converted images
#setwd("~/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")
#write.csv(dfT, "Hyperion_indexs_all_xy.csv")











#Test plots
df2 = dfT%>%
  group_by(index,treat,year,y) %>%
  summarise(value = mean(value,na.rm=TRUE))

pssr = df2 %>% filter(index == "pssr")

wbi = df2 %>% filter(index == "wbi")

ndii = df2 %>% filter(index == "ndii")


tile_plot = function(x){
  ggplot(x, aes(year, y, fill = value))+ 
    geom_tile()+
    facet_wrap(~treat)+
    scale_fill_viridis(discrete=FALSE)
}

tile_plot(pssr)
tile_plot(wbi)
tile_plot(ndii)

