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

fire <- filter(fire, YR==2010)
fire = fire %>% 
  separate(col = "id", c("id", "ano"), sep = ' ')
fire <- fire[,c(1, 2, 4)]

fire2 = fire %>% 
  group_by(id) %>%
  na.omit() %>%
  summarise(alt..cm. = max(alt..cm.))
colnames(fire2) = c("ID", "alt_fire")

#Shape do input data --------------------------------------------------------------------------
grid <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1",layer="Grid_Area1_AA")
grid@data$n = c(1:651)

master = merge(grid@data, fire2, by = "ID", all.x = T)
master[is.na(master)] <- 0
master = master %>% 
  arrange(n) 
  
grid@data$alt_fire <- master$alt_fire
#See shape and data frame
#View(grid@data)
spplot(grid, "alt_fire", main = "Altura da chama (cm) - 2010", col = "transparent")

#Rastize data
#r <- raster(ncol=180, nrow=180)
#extent(r) <- extent(grid)
#fire <- rasterize(grid, r, 'alt_fire')
#plot(fire)

#Salve raster
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados rasterizados/Fogo")
#writeRaster(fire, filename="larg_fire2010.tiff", overwrite=TRUE)

#Salve new vector
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1")
#writeOGR(grid, ".", "grid_values", driver="ESRI Shapefile")

