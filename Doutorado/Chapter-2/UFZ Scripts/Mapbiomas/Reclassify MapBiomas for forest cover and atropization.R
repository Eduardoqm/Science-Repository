#Reclassify MapBiomas for forest cover and atropization

#Eduardo Q Marques 27-09-2022


library(raster)
library(rgdal)

#Essential functions to VSCode --------------------------------------------------
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Load daata --------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
img = raster("xingu_2020.tif")

plotx(img)

#Reclassifying -----------------------------------------------------------------
#Classes of MapBiomas
forest_list = as.numeric(c(1,3,4,5,49)) #Forest
natural_list = c(10,11,12,32,29,50,13,26,33,31,27,23,25) #Natural formations plus water and dune
antrop_list = c(14,15,18,19,39,20,40,62,41,36,46,47,48,9,21,11,24,30) #Antropization

#img2 = img
#img12 = img[[1]]
#img20 = img[[2]]

#Doing the reclassifying
for (z in forest_list) {
  print(z)
  img[img == z] <- 1
  #img20[img20 == z] <- 1
}

for (z in natural_list) {
  print(z)
  img[img == z] <- NA
  #img20[img20 == z] <- 2
}

for (z in antrop_list) {
  print(z)
  img[img == z] <- NA
  #img20[img20 == z] <- 3
}

#plotx(img12)
#plotx(img20)

#img_rcl = stack(img12, img20)

#plotx(img_rcl)
plotx(img)

setwd("/home/queirozm/eqm_eth_ufz/Data/Mapbiomas_GEDI/")
writeRaster(img, "xingu_2020_reclass.tif")
