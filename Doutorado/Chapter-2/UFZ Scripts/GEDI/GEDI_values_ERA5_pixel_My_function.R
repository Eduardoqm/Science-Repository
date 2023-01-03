#Calculate Vegetation hight from ERA5 pixel 0.25 (Trying my functions)

#Eduardo Q Marques 05-10-2022

library(raster)

#Essential functions to VSCode --------------------------------------------------
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load data ----------------------------------------------------------------------
#ERA5 Raster
setwd("/home/queirozm/eqm_eth_ufz/Data/ERA5_SL/")
era5 = brick("ERA5_Single_Levels-Wind_Gust-2012_2020.nc")
era5 = era5[[1]]
plotx(era5)

#GEDI Raster
setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
gedi = raster("GEDI_xingu_forest_2019.tif")
plotx(gedi)

library(rgdal)
xing = readOGR(dsn = "/home/queirozm/eqm_eth_ufz/Data/Shapes/Xingu_MT.shp", layer = "Xingu_MT")
plotx(xing)

era5b = crop(era5, xing)
plotx(era5b)


#Resample by not traditional way -------------------------------------------------
era_grid <- rasterToPolygons(era5b, fun=function(x){x>=0})
plotx(era_grid)
era_grid


gedi_max = raster::extract(gedi, era_grid[1,], fun = mean, df = T, na.rm = T)
gedi_max$coord = coordinates(era_grid[1,])


for (z in 2:length(era_grid)) {
   print(z)
   w = raster::extract(gedi, era_grid[z,], fun = mean, df = T, na.rm = T)
   w$coord = coordinates(era_grid[z,])
   gedi_max = rbind(gedi_max, w)
}


setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
write.csv(gedi_max, "GEDI_mean_pixel.csv", row.names = F)




