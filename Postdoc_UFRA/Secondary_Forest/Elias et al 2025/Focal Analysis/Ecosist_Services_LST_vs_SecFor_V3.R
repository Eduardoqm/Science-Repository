#LST by Secondary Forest Age (Focal)

#Eduardo Q Marques 22-08-2025 Divino Update 03-09-2025

library(terra)
library(tidyverse)
library(sf)
library(future) #Multicore work

parallel::detectCores()

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m")
dir()

#LST
lst_year = rast("LST_Landsat_Annual_2022_70m.tif")
lst_dry = rast("LST_Landsat_Dry_2022_70m.tif")
lst_wet = rast("LST_Landsat_Wet_2022_70m.tif")

plot(lst_year)
plot(lst_dry)
plot(lst_wet)

#Primary Forest
fr_pri=rast("Forest_70m.tif" )
plot(fr_pri)

#Secondary Forest
sf=rast("MB_Forest_age_70m.tif")
plot(sf)

#Percentage of Secondary Forest
sf_perc = rast("Perc_SecForest_70m.tif")
plot(sf_perc)

#Tiles for processing
grix = vect("C:/Users/Public/Documents/Analises_Elias/Shapes/Grix_4P.shp")

#Focal Function -------------------------------------------------------------
setwd("C:/Users/Public/Documents/Analises_Elias/Dados/LST")
start.time <- Sys.time()

#LST Annual---------------------------------------------------------------------
#Use focal to calculate difference in Secondary and Primary (Delta)
plot(grix)
fr_prix = mask(crop(fr_pri, grix[1]), grix[1])
lst_yearx = mask(crop(lst_year, grix[1]), grix[1])
sfx = mask(crop(sf, grix[1]), grix[1])
sf_percx = mask(crop(sf_perc, grix[1]), grix[1])
lst_prix = ifel(is.na(fr_prix),NA,lst_yearx)

lst_f <- focal(lst_prix, w=21, median, na.rm=TRUE, na.policy="only")
plot(lst_f, add = T, legend = FALSE)

lst_delta_pri=lst_yearx-lst_f

resf=as.data.frame(c(sfx,lst_delta_pri, sf_percx))
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf = resf %>% na.omit()

for (z in 2:length(grix)) {
  fr_prix = mask(crop(fr_pri, grix[z]), grix[z])
  lst_yearx = mask(crop(lst_year, grix[z]), grix[z])
  sfx = mask(crop(sf, grix[z]), grix[z])
  sf_percx = mask(crop(sf_perc, grix[z]), grix[z])
  lst_prix = ifel(is.na(fr_prix),NA,lst_yearx)
  
  lst_f <- focal(lst_prix, w=21, median, na.rm=TRUE, na.policy="only")
  plot(lst_f, add = T, legend = FALSE)
  
  lst_delta_pri=lst_yearx-lst_f
  
  resf2=as.data.frame(c(sfx,lst_delta_pri, sf_percx))
  colnames(resf2) = c("sf_age", "delta_lst", "sf_perc")
  resf2 = resf2 %>% na.omit()
  resf = rbind(resf, resf2)  
}

resf$cond = "Annual"
resf$year = 2022
write.csv(resf, "LST_SecFor_Age_Annual_full_2022.csv", row.names = F)


#LST Dry Season-----------------------------------------------------------------
plot(grix)
fr_prix = mask(crop(fr_pri, grix[1]), grix[1])
lst_dryx = mask(crop(lst_dry, grix[1]), grix[1])
sfx = mask(crop(sf, grix[1]), grix[1])
sf_percx = mask(crop(sf_perc, grix[1]), grix[1])
lst_prix = ifel(is.na(fr_prix),NA,lst_dryx)

lst_f <- focal(lst_prix, w=21, median, na.rm=TRUE, na.policy="only")
plot(lst_f, add = T, legend = FALSE)

lst_delta_pri=lst_dryx-lst_f

resf=as.data.frame(c(sfx,lst_delta_pri, sf_percx))
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf = resf %>% na.omit()

for (z in 2:length(grix)) {
  fr_prix = mask(crop(fr_pri, grix[z]), grix[z])
  lst_dryx = mask(crop(lst_dry, grix[z]), grix[z])
  sfx = mask(crop(sf, grix[z]), grix[z])
  sf_percx = mask(crop(sf_perc, grix[z]), grix[z])
  lst_prix = ifel(is.na(fr_prix),NA,lst_dryx)
  
  lst_f <- focal(lst_prix, w=21, median, na.rm=TRUE, na.policy="only")
  plot(lst_f, add = T, legend = FALSE)
  
  lst_delta_pri=lst_dryx-lst_f
  
  resf2=as.data.frame(c(sfx,lst_delta_pri, sf_percx))
  colnames(resf2) = c("sf_age", "delta_lst", "sf_perc")
  resf2 = resf2 %>% na.omit()
  resf = rbind(resf, resf2)  
}

resf$cond = "Dry Season"
resf$year = 2022
write.csv(resf, "LST_SecFor_Age_Dry_full_2022.csv", row.names = F)


#LST Rainy Season --------------------------------------------------------------
plot(grix)
fr_prix = mask(crop(fr_pri, grix[1]), grix[1])
lst_wetx = mask(crop(lst_wet, grix[1]), grix[1])
sfx = mask(crop(sf, grix[1]), grix[1])
sf_percx = mask(crop(sf_perc, grix[1]), grix[1])
lst_prix = ifel(is.na(fr_prix),NA,lst_wetx)

lst_f <- focal(lst_prix, w=21, median, na.rm=TRUE, na.policy="only")
plot(lst_f, add = T, legend = FALSE)

lst_delta_pri=lst_wetx-lst_f

resf=as.data.frame(c(sfx,lst_delta_pri, sf_percx))
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf = resf %>% na.omit()

for (z in 2:length(grix)) {
  fr_prix = mask(crop(fr_pri, grix[z]), grix[z])
  lst_wetx = mask(crop(lst_wet, grix[z]), grix[z])
  sfx = mask(crop(sf, grix[z]), grix[z])
  sf_percx = mask(crop(sf_perc, grix[z]), grix[z])
  lst_prix = ifel(is.na(fr_prix),NA,lst_wetx)
  
  lst_f <- focal(lst_prix, w=21, median, na.rm=TRUE, na.policy="only")
  plot(lst_f, add = T, legend = FALSE)
  
  lst_delta_pri=lst_dryx-lst_f
  
  resf2=as.data.frame(c(sfx,lst_delta_pri, sf_percx))
  colnames(resf2) = c("sf_age", "delta_lst", "sf_perc")
  resf2 = resf2 %>% na.omit()
  resf = rbind(resf, resf2)  
}

resf$cond = "Rainy Season"
resf$year = 2022
write.csv(resf2, "LST_SecFor_Age_Rainy_full_2022.csv", row.names = F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

