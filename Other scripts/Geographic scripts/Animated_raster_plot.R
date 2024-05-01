#Animated Raster Plot

#Eduardo Q Marques 23-11-2020

library(raster)
library(viridis)
library(rasterVis)
library(rgdal)

list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Landsat/All Indices", pattern = "ndvi", full.names=TRUE,recursive=TRUE)

area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

index <- stack(list)
index = projectRaster(index, crs = crs(area1))

x11()
plot(index[[51]])
limit = drawPoly()
index2 = crop(index, limit)

#animate(index2, n=1, col = rev(viridis(100)))

setwd("C:\\Users\\Eduardo Q Marques\\Desktop\\gif")

for (x in 1:nlayers(index2)) {
  
  
  name = names(index2[[x]])
  name = paste(name, ".png")
  
  png(file=name, width=600, height=550)
  plot(index2[[x]], main = names(index2[[x]]))
  #levelplot(index2[[x]], margin = F, main = names(index2[[x]]),
   #         col.regions=viridis(100))
  dev.off()
}


