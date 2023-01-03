#Calculate Vegetation hight from MapBiomas forest

#Eduardo Q Marques 30-09-2022

library(raster)

#Essential functions to VSCode --------------------------------------------------
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load data ----------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
img = raster("xingu_2020_reclass.tif")
gedi = raster("GEDI_xingu_2019.tif")

plotx(img)
plotx(gedi)

#Removing deforestation effects -------------------------------------------------
#Classes of GEDI
#0-60 Forest canopy height, meters
#101 Water
#102 Snow/ice
#103 No data

#img20 = img[[2]] #MapBiomas reclass to 2020 (1-Forest, 2-Non Forest, 3-Antropization)
plotx(img)

gedi2 = resample(gedi, img, method="ngb") #Resample GEDI to pixel match with MapBiomas
plotx(gedi2)

gedi3 = mask(gedi2, img) #Extract just the class forest
plotx(gedi3)

gedi4 = gedi3
gedi4[gedi4 > 60] <- NA #Remove another class to remain just tree hight
plotx(gedi4)

setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
writeRaster(gedi4, "GEDI_xingu_forest_2019.tif")
