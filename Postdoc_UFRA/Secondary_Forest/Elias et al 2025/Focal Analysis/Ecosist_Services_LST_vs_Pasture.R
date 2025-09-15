#LST by Pasture (Focal)

#Eduardo Q Marques 09-09-2025

library(terra)
library(tidyverse)
library(sf)
library(future) #Multicore work

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")
dir()

#LST
lst_year = rast("LST_Landsat_Annual_2022_2023_70m.tif")
lst_dry = rast("LST_Landsat_Dry_2022_2023_70m.tif")
lst_wet = rast("LST_Landsat_Wet_2022_2023_70m.tif")

plot(lst_year)
plot(lst_dry)
plot(lst_wet)

#Pasture
grass=rast("Pasture_70m.tif")
plot(grass)

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

gc()
#LST Annual
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(grass),NA,lst_year)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
rm(lst_pri)

gc()
lst_delta_pri=lst_year-lst_f
rm(et_year, et_f)
plot(lst_delta_pri)

gc()
resf=as.data.frame(c(sf,lst_delta_pri, sf_perc))
rm(grass2, et_delta_pri)
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Annual"

write.csv(resf2, "LST_Pasture_Annual_full.csv", row.names = F)


#LST Dry Season
gc()
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(grass),NA,lst_dry)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
rm(lst_pri)

gc()
lst_delta_pri=lst_dry-lst_f
rm(et_year, et_f)
plot(lst_delta_pri)

gc()
resf=as.data.frame(c(sf,lst_delta_pri, sf_perc))
rm(grass2, et_delta_pri)
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Dry Season"

write.csv(resf2, "LST_Pasture_Dry_full.csv", row.names = F)


#LST Rainy Season
gc()
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(grass),NA,lst_wet)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
rm(lst_pri)

gc()
lst_delta_pri=lst_wet-lst_f
rm(et_year, et_f)
plot(lst_delta_pri)

gc()
resf=as.data.frame(c(sf,lst_delta_pri, sf_perc))
rm(grass2, et_delta_pri)
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Rainy Season"

write.csv(resf2, "LST_Pasture_Rainy_full.csv", row.names = F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
