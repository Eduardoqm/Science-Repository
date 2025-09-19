#Evapotranspiration by ECOSTRESS (Elias Paper)

#Adapted from Divino V Silverio by Eduardo Q Marques 21-04-2025 updated 17-09-2025

library(terra)
library(sf)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/ECOSTRESS_day")

guama=read_sf("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Shapes/BR_Amazon_DrySeason_filtered.shp")
#guama2=st_transform(guama,crs = 4326)
#guama2=st_transform(guama, crs = 32723)
#guama2=st_transform(guama, crs = "EPSG:32723")

ex=rast("ECO_L3T_JET.002_ETdaily_doy2022002132231_aid0009_23S.tif")
ex2 <- project(ex, crs(guama))

ex2b <- rast(guama, resolution=res(ex2))
ex2c <- rasterize(guama, ex2b)
res(ex2c)
plot(ex2c)
plot(ex2, add = T)

#Get file names by month -------------------------------------------------------
metalist = list.files(path = "G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/ECOSTRESS_day/metadata", full.names = T)

#Read the file
meta1 = c(
Jan <- substr(readLines(metalist[1]), 127, 205),
Fev <- substr(readLines(metalist[2]), 127, 205),
Mar <- substr(readLines(metalist[3]), 127, 205),
Apr <- substr(readLines(metalist[4]), 127, 205))
head(meta1); length(meta1)

meta2 = c(
  May <- substr(readLines(metalist[5]), 127, 205),
  Jun <- substr(readLines(metalist[6]), 127, 205),
  Jul <- substr(readLines(metalist[7]), 127, 205))
head(meta2); length(meta2)

meta3 = c(
  Agu <- substr(readLines(metalist[8]), 127, 205),
  Sep <- substr(readLines(metalist[9]), 127, 205))
head(meta3); length(meta3)

meta4 = c(
  Out <- substr(readLines(metalist[10]), 127, 205),
  Nov <- substr(readLines(metalist[11]), 127, 205),
  Dec <- substr(readLines(metalist[12]), 127, 205))
head(meta4); length(meta4)

#January to April --------------------------------------------------------------
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

writeRaster(et2, "ECOSTRESS_EVAP_Jan_April_2022.tif")


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
et3 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic
plot(et3)

writeRaster(et3, "ECOSTRESS_EVAP_May_July_2022.tif")

#August to September -----------------------------------------------------------
gc()
Aug_Sep <- list()

for(i in 1:length(meta3)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta3[i])
    r <- project(r, crs(guama))
    r2 <- terra::resample(r, ex2c)
    Aug_Sep[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta3[i], ":", conditionMessage(e), "\n")
    Aug_Sep[[i]] <- NULL
  })
}


Aug_Sep2 <- Aug_Sep[!sapply(Aug_Sep, is.null)]

stacked <- terra::rast(Aug_Sep2)
et4 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic
plot(et4)

writeRaster(et4, "ECOSTRESS_EVAP_Aug_Sep_2022.tif")

#October to December -----------------------------------------------------------
gc()
Oct_Dec <- list()

plan(multisession, workers = 27)

for(i in 1:length(meta4)) {
  cat("Proccess", i, "\n")
  
  tryCatch({
    r <- terra::rast(meta4[i])
    r <- project(r, crs(guama))
    r2 <- terra::resample(r, ex2c)
    Oct_Dec[[i]] <- r2
  }, error = function(e) {
    cat("No exist", meta4[i], ":", conditionMessage(e), "\n")
    Oct_Dec[[i]] <- NULL
  })
}

Oct_Dec2 <- Oct_Dec[!sapply(Oct_Dec, is.null)]

stacked <- terra::rast(Oct_Dec2)
et5 <- terra::app(stacked, fun = max, na.rm = TRUE) #Take less time to mosaic
plot(et5)

writeRaster(et5, "ECOSTRESS_EVAP_Oct_Dec_2022.tif")




# ------------------------------------------------------------------------------
#Part2 Unifying the seasons

library(terra)
library(sf)

#Load Data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/ECOSTRESS_day")
dir()

wet1 = rast("ECOSTRESS_EVAP_Jan_April_2022.tif")
wet2 = rast("ECOSTRESS_EVAP_Oct_Dec_2022.tif")

dry1 = rast("ECOSTRESS_EVAP_May_July_2022.tif")
dry2 = rast("ECOSTRESS_EVAP_Aug_Sep_2022.tif")

#Joing wet and dry seasons -----------------------------------------------------
#Wet
gc()
stacked <- c(wet1, wet2)
wet <- terra::app(stacked, fun = mean, na.rm = TRUE)
plot(wet)

writeRaster(wet, "ECOSTRESS_EVAP_WetSeason_2022.tif")

#Dry
gc()
stacked <- c(dry1, dry2)
dry <- terra::app(stacked, fun = mean, na.rm = TRUE)
plot(dry)

writeRaster(dry, "ECOSTRESS_EVAP_DrySeason_2022.tif")

#Annual
gc()
#wet = rast("ECOSTRESS_EVAP_WetSeason_2022.tif")
#dry = rast("ECOSTRESS_EVAP_DrySeason_2022.tif")
stacked <- c(wet, dry)
annual <- terra::app(stacked, fun = mean, na.rm = TRUE)
plot(annual)

writeRaster(annual, "ECOSTRESS_EVAP_annual_2022.tif")



