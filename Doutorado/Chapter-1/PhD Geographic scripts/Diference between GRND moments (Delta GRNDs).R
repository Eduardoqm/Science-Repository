#Diference between GRND moments (Delta GRNDs)

#Eduardo Q Marques 08-06-2022


library(raster)
library(rasterVis)
library(rgdal)
library(viridis)

#Load data and apply de means of years datasets
setwd("~/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Landsat")

grnd04 = (raster("grnd-20040623.tif") + raster("grnd-20040725.tif"))/2
grnd11 = (raster("grnd-20110627.tif") + raster("grnd-20110830.tif"))/2
grnd19 = (raster("grnd-20190601.tif") + raster("grnd-20190617.tif") + raster("grnd-20190703.tif"))/3

plot(grnd04)
plot(grnd11)
plot(grnd19)

#Stack GRND moments
grnd = stack(grnd04, grnd11, grnd19)
levelplot(grnd, col.regions = viridis(20))

#Cut to Area-1 limits
area1 = readOGR("~/Research/Doutorado/Banco de Dados Tanguro/Shapes/Polygon_A_B_C.shp")
area1 = spTransform(area1, crs(grnd[[1]]))

plot(grnd[[1]])
plot(area1, add = T)

grnd = crop(grnd, area1)
levelplot(grnd, layout=c(1, 3))

#Calculate deltas
grnd_dff = grnd

grnd_dff[[1]] = grnd[[3]] - grnd[[1]] #Delta 2004 - 2019 Start and recover of fire experiment
grnd_dff[[2]] = grnd[[2]] - grnd[[1]] #Delta 2004 - 2011 Fire experiment period
grnd_dff[[3]] = grnd[[3]] - grnd[[2]] #Delta 2011 - 2019 Recover time

levelplot(grnd_dff, layout=c(1, 3))



grnd_dff@data@names = c("2004 - 2019", "2004 - 2011", "2011 - 2019")

levelplot(grnd_dff, layout=c(1, 3), par.settings = RdBuTheme, labels = F, scales = list(col = "white"), colorkey=list(labels=list(cex=1.3)))



grnd@data@names = c("2004", "2011", "2019")

levelplot(grnd, layout=c(1, 3), par.settings = viridisTheme, labels = F, scales = list(col = "white"), colorkey=list(labels=list(cex=1.3)))


#Export plots
setwd("~/Research/Doutorado/Capitulo1/Figuras/Esquemas visuais")

png("GRND 2004-2011-2019.png", width=700, height=700) 
levelplot(grnd, layout=c(1, 3), par.settings = viridisTheme, labels = F, scales = list(col = "white"), colorkey=list(labels=list(cex=1.3)))
dev.off()


png("Delta GRND.png", width=700, height=700) 
levelplot(grnd_dff, layout=c(1, 3), par.settings = RdBuTheme, labels = F, scales = list(col = "white"), colorkey=list(labels=list(cex=1.3)))
dev.off()
