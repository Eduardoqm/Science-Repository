###################################################################################
# Process with Precipitation data                                                 #
#                                                                                 #
# Eduardo Q Marques 15-05-2022                                                    #
###################################################################################

# This script will:
# 1-Convering Total Precipitationto to recurrent,
# 2-Resample data to 0.25 degrees

library(raster)

#Essential Functions ----------------------------------------------------------------------
#Function to plot data in EVE server
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

# 1-Convering Total Precipitationto to recurrent ======================================================
setwd("/home/queirozm/eqm_eth_ufz/Data/ERA5_land")
ppt = brick("Total_Precipitation_ERA5-Land_2001_2011.nc") #Wind Speed from ERA5 Land

#Transform accumulated in recurrent precipitation
ppt2 = ppt
ppt3 = ppt

for (z in 2:length(ppt@data@names)) {
  print(length(ppt@data@names) - z)
  w = z - 1
  ppt3[[z]] = ppt2[[z]] - ppt[[w]]
}

plotx(ppt)
plotx(ppt3)

# 2-Resample data to 0.25 degrees ========================================================================
setwd("/home/queirozm/eqm_eth_ufz/Data/ERA5_SL")
wg = brick("ERA5_Single_Levels-Wind_Gust-2012_2020.nc") #The base to be resmapled

#Resampling
ppt4 <- resample(ppt3, wg[[1]], method='bilinear')
plotx(ppt4)

#Save rasters ------------------------------------------------------------------
raster::writeRaster(ppt4, "/home/queirozm/eqm_eth_ufz/Data/ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_2001_2011.nc")

