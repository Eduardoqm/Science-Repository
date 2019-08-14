#Spacilization fire
#Eduaro Q Marques  13/08/2019

library(ggplot2); library(reshape2); library(dplyr);
library(tidyr); library(rgdal); library(raster)

#Fire data -----------------------------------------------------------------------------------
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Fogo')
fire <- read.csv("FIRE DATA SUMMARY.csv", header = TRUE, sep = ",")
#View(fire)

#Manipulate data
fire <- fire[,c(2, 3, 9)]

fire <- filter(fire, YR==2007)
fire = fire %>% 
  separate(col = "id", c("id", "ano"), sep = ' ')
fire <- fire[,c(1, 2, 4)]

fire2 = fire %>% 
  group_by(id) %>%
  na.omit() %>%
  summarise(alt..cm. = max(alt..cm.))

colnames(fire2) = c("id2", "alt_fire")

#Shape do input data --------------------------------------------------------------------------
grid <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1",layer="Grid_Area1B")


grid@data = merge(grid@data, fire2, by = "id2", all.x = T)

grid@data[is.na(grid@data)] <- 0

#See shape and data frame
View(grid@data)

#plot(grid)

spplot(grid["alt_fire"])

#Rastize data
#r <- raster(ncol=180, nrow=180)
#extent(r) <- extent(grid)

#rp <- rasterize(grid, r, 'alt_fire')


#Salve new vector
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1")
#writeOGR(grid, ".", "grid_values", driver="ESRI Shapefile")












