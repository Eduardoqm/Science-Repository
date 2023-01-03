#Eduardo Q Marques  03-08-2022

library(raster)

setwd("/home/queirozm/eqm_eth_ufz/Data/ERA5_SL/Blocked_ERA5SL")
wg = brick("ERA_SL_WG_block_7pixels_1979_1989.nc")

setwd("/home/queirozm/eqm_eth_ufz/Data/ERA5_land")
ppt = brick("Resampled_Recurrent_Prec_ERA5-Land_1990_2000.nc")
tp = brick("Total_Precipitation_ERA5-Land_1990_2000.nc")

#Names em ppt stack
ppt@data@names = tp@data@names


plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

plotx(wg)
plotx(ppt[[96422:96432]])

wg2 = wg[[2]]
plotx(wg2)

wg7 <- raster::focal(wg2, w=matrix(1,nrow=7,ncol=7), fun = max)
plotx(wg7)

wg2 = wg[[1:16]]
plotx(wg2)
wg2p7 = wg2

for (k in 1:nlayers(wg2)) {
    print(nlayers(wg2b) - k)
   wg2p7[[k]] <- raster::focal(wg2[[k]], w=matrix(1,nrow=7,ncol=7), fun = max)
}


plotx(wg2p7)


setwd("/home/queirozm/eqm_eth_ufz/Data")
wg3 = brick("ERA_SL_WG_block_3pixels.nc")
wg5 = brick("ERA_SL_WG_block_5pixels.nc")
wg7 = brick("ERA_SL_WG_block_7pixels.nc")

plotx(wg3)
plotx(wg5)
plotx(wg7)
