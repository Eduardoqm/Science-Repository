#Calculate mean of fire frenquence from ERA5 pixel 0.25 (Trying my functions)

#Eduardo Q Marques 05-10-2022

library(raster)
library(ncdf4)

#Essential functions to VSCode --------------------------------------------------
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load data ----------------------------------------------------------------------
#ERA5 Raster
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/Blocked_ERA5SL/")
era5 = brick("ERA_SL_WG_block_7pixels_2012_2020.nc")
era5 = era5[[1]]
plot(era5)

#Mapbiomas Raster
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/Mapbiomas_GEDI/")
img = raster("xingu_2020_reclass.tif")
plot(img)

b <- brick("FireFrequence_1985_2020_Mapbiomas6_Xingu_box_30.tif", package="raster")
plot(b)

library(rgdal)

gc()
memory.limit(9999999999)

pct <- rgdal::SGDF2PCT(as(b, "SpatialGridDataFrame"))
fire2 <- setValues(raster(b), pct$idx-1)
colortable(fire2) <- pct$ct

gc() 

plot(fire2)

fire3 = resample(fire2, img, method="ngb") #Resample fire to pixel match with MapBiomas
plot(fire3)

fire4 = mask(fire3, img) #Extract just the class forest
plot(fire4)

#setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/Mapbiomas_GEDI/")
#writeRaster(fire4, "Fire_frequence_only_class_forest.tif", datatype = "GeoTiff ")




xing = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup ETH Server/Data/Shapes/Xingu_MT.shp", layer = "Xingu_MT")
plot(xing)

era5b = crop(era5, xing)
plot(era5b)


#Resample by not traditional way -------------------------------------------------
era_grid <- rasterToPolygons(era5b, fun=function(x){x>=0})
plot(era_grid)
era_grid


#Mean
fire_mean = raster::extract(fire4, era_grid[1,], fun = mean, df = T, na.rm = T)
fire_mean$coord = coordinates(era_grid[1,])


for (z in 2:length(era_grid)) {
   print(z)
   w = raster::extract(fire4, era_grid[z,], fun = mean, df = T, na.rm = T)
   w$coord = coordinates(era_grid[z,])
   fire_mean = rbind(fire_mean, w)
}


setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")
write.csv(fire_mean, "Fire_frequence_mean_pixel.csv", row.names = F)



#Sum
fire_sum = raster::extract(fire4, era_grid[1,], fun = sum, df = T, na.rm = T)
fire_sum$coord = coordinates(era_grid[1,])


for (z in 2:length(era_grid)) {
  print(z)
  w = raster::extract(fire4, era_grid[z,], fun = sum, df = T, na.rm = T)
  w$coord = coordinates(era_grid[z,])
  fire_sum = rbind(fire_sum, w)
}


setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")
write.csv(fire_mean, "Fire_frequence_sum_pixel.csv", row.names = F)




