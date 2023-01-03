#Calculate Vegetation cover from MapBiomas

#Eduardo Q Marques 30-09-2022

library(raster)

#Essential functions to VSCode --------------------------------------------------
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load daata --------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
img = brick("xingu_2012_2020_reclass.tif")

plotx(img)

#Removing deforestation effects -------------------------------------------------
img12 = img[[1]]
img20 = img[[2]]

img12[img20 == 3] = 3

plotx(img12)
plotx(img20)