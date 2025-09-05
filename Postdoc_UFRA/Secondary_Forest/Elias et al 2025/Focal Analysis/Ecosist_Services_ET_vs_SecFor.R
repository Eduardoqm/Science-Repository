#Evapotranspiration by Secondary Forest Age (Focal)

#Eduardo Q Marques 05-09-2025

library(terra)
library(tidyverse)
library(sf)
library(future) #Multicore work

parallel::detectCores()

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")
dir()

#ECOSTRESS
et_year = rast("ECOSTRESS_EVAP_Annual_2022_70m.tif")
et_dry = rast("ECOSTRESS_EVAP_DrySeason_2022_70m.tif")
et_wet = rast("ECOSTRESS_EVAP_WetSeason_2022_70m.tif")
plot(et_year)
plot(et_dry)
plot(et_wet)

#Primary Forest
fr_pri=rast("Forest_70m.tif" )
plot(fr_pri)

#Secondary Forest
sf=rast("MB_Forest_age_70m.tif")
plot(sf)

#Percentage of Secondary Forest
sf_perc = rast("Perc_SecForest_70m.tif")
plot(sf_perc)

#Proccess before Focal ---------------------------------------------------------
sf_perc = ifel(sf_perc == 0,NA, sf_perc)
plot(sf_perc)

#Focal Function -------------------------------------------------------------
setwd("G:\\Meu Drive\\Postdoc_UFRA\\Papers\\Serrapilheira (Elias et al)\\Analises_Elias\\Dados\\")
start.time <- Sys.time()

#ET Annual
gc()
#Calculating et for Primary Forest
et_pri=ifel(is.na(fr_pri),NA,et_year)
#plot(et_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(et_f)

et_delta_pri=et_year-et_f
#plot(et_delta_pri)

resf=as.data.frame(c(sf,et_delta_pri, sf_perc))
colnames(resf) = c("sf_age", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Annual"

write.csv(resf2, "ET_SecFor_Age_Annual_full.csv", row.names = F)


#ET Dry Season
gc()
et_pri=ifel(is.na(fr_pri),NA,et_dry)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
et_delta_pri=et_dry-et_f
#plot(et_delta_pri)

resf=as.data.frame(c(sf,et_delta_pri, sf_perc))
colnames(resf) = c("sf_age", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Dry Season"

write.csv(resf2, "ET_SecFor_Age_Dry_full.csv", row.names = F)


#ET Rainy Season
gc()
et_pri=ifel(is.na(fr_pri),NA,et_wet)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
et_delta_pri=et_wet-et_f

#writeRaster(et_delta_pri, "et_delta_pri.tif")
#et_delta_pri = rast("et_delta_pri.tif")
#plot(et_delta_pri)

resf=as.data.frame(c(sf,et_delta_pri, sf_perc))
colnames(resf) = c("sf_age", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Rainy Season"

write.csv(resf2, "ET_SecFor_Age_Rainy_full.csv", row.names = F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

