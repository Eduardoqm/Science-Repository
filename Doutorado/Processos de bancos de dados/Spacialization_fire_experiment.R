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

fire <- filter(fire, YR==2009)
fire = fire %>% 
  separate(col = "id", c("id", "ano"), sep = ' ')
fire <- fire[,c(1, 2, 4)]

fire2 = fire %>% 
  group_by(id) %>%
  na.omit() %>%
  summarise(alt..cm. = max(alt..cm.))
colnames(fire2) = c("ID", "alt_fire")

#Shape do input data --------------------------------------------------------------------------
grid <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1",layer="Grid_Area1D")
grid@data$n = c(1:651)

master = merge(grid@data, fire2, by = "ID", all.x = T)
master[is.na(master)] <- 0
master = master %>% 
  arrange(n) 
  #arrange(row)
  


grid@data$alt_fire <- master$alt_fire
#See shape and data frame
View(grid@data)

#plot(grid)

spplot(grid, "alt_fire")

#Rastize data
#r <- raster(ncol=180, nrow=180)
#extent(r) <- extent(grid)

#rp <- rasterize(grid, r, 'alt_fire')


#Salve new vector
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1")
#writeOGR(grid, ".", "grid_values", driver="ESRI Shapefile")












