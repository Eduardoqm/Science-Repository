#Spacilization fire
#Eduaro Q Marques  15/08/2019

library(ggplot2); library(reshape2); library(dplyr);
library(tidyr); library(rgdal); library(raster)

#Fire data -----------------------------------------------------------------------------------
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Area1-plot/Tanguro Parcela')
lit <- read.csv("1_master_liteira_area_1_jun2019.csv", header = TRUE, sep = ",")
#View(lit)

#Manipulate data
lit <- lit[,c(3,4,10)]

lit <- filter(lit, years==2010)

lit2 = lit %>% 
  group_by(pont) %>%
  na.omit() %>%
  summarise(weight = mean(weight))
colnames(lit2) = c("ID", "litt_peso")

#Shape do input data --------------------------------------------------------------------------
grid <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1",layer="Grid_Area1_AA")
grid@data$n = c(1:651)

master = merge(grid@data, lit2, by = "ID", all.x = T)
master[is.na(master)] <- 0
master = master %>% 
  arrange(n) 
  
grid@data$litt_peso <- master$litt_peso
#See shape and data frame
#View(grid@data)
spplot(grid, "litt_peso", main = "Liteira Peso (g) - 2010", col = "transparent")

#Rastize data
#r <- raster(ncol=180, nrow=180)
#extent(r) <- extent(grid)
#litt <- rasterize(grid, r, 'litt_peso')
#plot(fire)

#Salve raster
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados rasterizados/Fogo")
#writeRaster(fire, filename="larg_fire2010.tiff", overwrite=TRUE)

#Salve new vector
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1")
#writeOGR(grid, ".", "grid_values", driver="ESRI Shapefile")

