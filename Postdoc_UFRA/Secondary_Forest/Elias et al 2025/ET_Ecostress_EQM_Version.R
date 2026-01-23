#Evapotranspiration by ECOSTRESS (Elias Paper)

#Adapted from Divino V Silverio by Eduardo Q Marques 21-04-2025 updated 08-12-2025

library(terra)
library(sf)

#Load Data ---------------------------------------------------------------------
setwd("C:/Users/Public/Documents/Analises_Elias/Rasters/ET_separado/2024")
dir()

guama=read_sf("C:/Users/Public/Documents/Analises_Elias/Shapes/BR_Amazon_DrySeason_filtered.shp")
#plot(guama)

ex=rast("ECO_L3T_JET.002_ETdaily_doy2024019181712_aid0009_19S.tif")
ex2 <- project(ex, crs(guama))

ex2b <- rast(guama, resolution=res(ex2))
ex2c <- rasterize(guama, ex2b)
res(ex2c)
#plot(ex2c)
#plot(ex2, add = T)

#Get file names by month -------------------------------------------------------
metalist = list.files(path = "C:/Users/Public/Documents/Analises_Elias/Rasters/ET_separado/2024/metadata", full.names = T)

#Read the rainy
meta1 = c(
  Dec <- substr(readLines(metalist[12]), 127, 205),
  Jan <- substr(readLines(metalist[1]), 127, 205),
  Fev <- substr(readLines(metalist[2]), 127, 205),
  Mar <- substr(readLines(metalist[3]), 127, 205),
  Apr <- substr(readLines(metalist[4]), 127, 205),
  May <- substr(readLines(metalist[5]), 127, 205))
head(meta1); length(meta1)

#Read the dry
meta2 = c(
  Jun <- substr(readLines(metalist[6]), 127, 205),
  Jul <- substr(readLines(metalist[7]), 127, 205),
  Agu <- substr(readLines(metalist[8]), 127, 205),
  Sep <- substr(readLines(metalist[9]), 127, 205),
  Out <- substr(readLines(metalist[10]), 127, 205),
  Nov <- substr(readLines(metalist[11]), 127, 205))
head(meta2); length(meta2)

#January to April --------------------------------------------------------------
start.time <- Sys.time()
Jan_Apr <- list()

for(i in 1:length(meta1)) {
#for(i in c(1:2,281)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta1[i])
    r <- project(r, crs(guama))
    r2 <- terra::resample(r, ex2c)
    Jan_Apr[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta1[i], ":", conditionMessage(e), "\n")
    Jan_Apr[[i]] <- NULL
  })
}

Jan_Apr2 <- Jan_Apr[!sapply(Jan_Apr, is.null)]

stacked <- terra::rast(Jan_Apr2)
plot(stacked)
et2 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic
plot(et2)

writeRaster(et2, "ECOSTRESS_ET_Rainy_2024.tif")
#writeRaster(et2, "ECOSTRESS_EVAP_Jan_April_2024.tif")


#May to July -------------------------------------------------------------------
gc()
May_Jul <- list()

for(i in 1:length(meta2)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta2[i])
    r <- project(r, crs(guama))
    r2 <- terra::resample(r, ex2c)
    May_Jul[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta2[i], ":", conditionMessage(e), "\n")
    May_Jul[[i]] <- NULL
  })
}

May_Jul2 <- May_Jul[!sapply(May_Jul, is.null)]

stacked <- terra::rast(May_Jul2)
et3 <- terra::app(stacked, fun = max, na.rm = TRUE)
plot(et3)

#Proccess 218: 16.70
#et3b = ifel(et3 > 20, NA, et4) #Maximums I foud was 10.20, 42.50, 746.84
#plot(et3b)
writeRaster(et3, "ECOSTRESS_ET_Dry_2024.tif")
#writeRaster(et3, "ECOSTRESS_EVAP_May_July_2024.tif")


et_m = mean(et2, et3)
plot(et_m)
writeRaster(et_m, "ECOSTRESS_ET_Annual_2024.tif")


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
