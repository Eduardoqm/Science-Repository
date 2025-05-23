#Mosaic LST from Landsat-9 (Elias Paper)

#Eduardo Q Marques 11-04-2025


library(terra)

#Load data ---------------------------------------------------------------------
listLST = function(x){
  lst =  list.files(path="G:/Meu Drive/GEE_LST_Landsat9",
                     pattern = x,
                     full.names=TRUE,recursive=TRUE)
  return(lst)
}

lst1 = listLST("LST_Landsat9_1_2023")
lst2 = listLST("LST_Landsat9_2_2023")

#Make the mosaics --------------------------------------------------------------
img = rast(lst1[1])

for (n in 2:length(lst1)) {
  print(n)
  img2 = rast(lst1[n])
  img = mosaic(img, img2, fun="mean")
}

plot(img)

setwd("C:/Users/Eduardo/Documents/Analises_Elias/Rasters")
writeRaster(img, "LST_Land9_jan2023.tif")