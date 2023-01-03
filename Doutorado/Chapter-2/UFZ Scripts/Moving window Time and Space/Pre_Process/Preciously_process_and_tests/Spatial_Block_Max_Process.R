
# Process of spatial block maxima in ERA5 data

# Eduardo Q Marques 04-08-2022

library(raster)

#Essential Functions ----------------------------------------------------------------------
#Function to plot data in EVE server
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Function to run focal on stacks
maximus = function(w, z){
    w2 = w
    for (k in 1:nlayers(w)) {
    print(nlayers(w) - k)
    w2[[k]] <- raster::focal(w[[k]], w=matrix(1,nrow=z,ncol=z), fun = max)
    }
    return(w2)
}

#Load data -----------------------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data")
wg = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-2012_2020.nc")

ppt = brick("ERA5_land/Recurrent_Precipitation_ERA5-Land_2012_2020.nc")
tp = brick("ERA5_land/Total_Precipitation_ERA5-Land_2012_2020.nc")
#Names em ppt stack
ppt@data@names = tp@data@names

#Verifying data ------------------------------------------------------------------------------
#plotx(wg)
#plotx(ppt)

#Blocking by surronding pixels ---------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data")

wg3p = maximus(wg, 3)
raster::writeRaster(wg3p, "ERA_SL_WG_block_3pixels.nc")

wg5p = maximus(wg, 5)
raster::writeRaster(wg5p, "ERA_SL_WG_block_5pixels.nc")

wg7p = maximus(wg, 7)
raster::writeRaster(wg7p, "ERA_SL_WG_block_7pixels.nc")