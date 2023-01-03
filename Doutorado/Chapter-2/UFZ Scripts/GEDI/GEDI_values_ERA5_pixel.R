#Calculate Vegetation hight from ERA5 pixel 0.25

#Eduardo Q Marques 04-10-2022

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

#Resample by traditional way -----------------------------------------------------
gedi2 = resample(gedi, era5, method="bilinear")
plotx(gedi2)

setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
writeRaster(gedi2, "GEDI_025_xingu_forest_2019.tif")


#era_grid <- as(era5, 'SpatialGridDataFrame')
#era_grid <- as(era5, 'SpatialPixels')
#era_grid <- rasterToPolygons(era5, fun=function(x){x>=0})
#plotx(era_grid)
#gedi_df = raster::extract(gedi, era_grid, xy = T)



library(stars)
gedi2 = st_warp(gedi_p, era_grid, method = "average", use_gdal = TRUE)
plotx(gedi2)




