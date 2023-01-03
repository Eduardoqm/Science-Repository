# Resample Precipitation of ERA5 Land to the same resolution of ERA SL (0.25)

# Eduardo Q Marques  08-08-2022

library(raster)

#Essential Functions ----------------------------------------------------------------------
#Function to plot data in EVE server
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load data ----------------------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/ERA5_SL")
wg = brick("ERA5_Single_Levels-Wind_Gust-2012_2020.nc") #The base to be resmapled

setwd("/home/queirozm/eqm_eth_ufz/Data/ERA5_land")
ppt = brick("Recurrent_Precipitation_ERA5-Land_2012_2020.nc")
tp = brick("Total_Precipitation_ERA5-Land_2012_2020.nc")
#Names em ppt stack
ppt@data@names = tp@data@names
plotx(ppt)

#Resampling
pptb <- resample(ppt, wg, method='bilinear')
plotx(pptb)

#raster::writeRaster(pptb, "/home/queirozm/eqm_eth_ufz/Data/ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_2012_2020.nc")