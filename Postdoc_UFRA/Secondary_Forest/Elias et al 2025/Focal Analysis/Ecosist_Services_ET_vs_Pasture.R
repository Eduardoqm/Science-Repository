#ET by Pasture (Focal)

#Eduardo Q Marques 09-09-2025

library(terra)
library(tidyverse)
library(sf)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")
dir()

#et
et_year = rast("ECOSTRESS_EVAP_Annual_2022_70m.tif")
et_dry = rast("ECOSTRESS_EVAP_DrySeason_2022_70m.tif")
et_wet = rast("ECOSTRESS_EVAP_WetSeason_2022_70m.tif")

plot(et_year)
plot(et_dry)
plot(et_wet)

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
#ET Annual
#Calculating et for Primary Forest
et_pri=ifel(is.na(grass),NA,et_year)

#Use focal to calculate difference in Secondary and Primary (Delta)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
rm(et_pri)

gc()
et_delta_pri=et_year-et_f
rm(et_year, et_f)
plot(et_delta_pri)

gc()
resf=as.data.frame(c(sf,et_delta_pri, sf_perc))
rm(grass2, et_delta_pri)
colnames(resf) = c("sf_age", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Annual Season"

write.csv(resf2, "ET_Pasture_Annual_full.csv", row.names = F)


#ET Dry Season
gc()
#Calculating et for Primary Forest
et_pri=ifel(is.na(grass),NA,et_dry)

#Use focal to calculate difference in Secondary and Primary (Delta)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
rm(et_pri)

gc()
et_delta_pri=et_dry-et_f
rm(et_year, et_f)
plot(et_delta_pri)

gc()
resf=as.data.frame(c(sf,et_delta_pri, sf_perc))
rm(grass2, et_delta_pri)
colnames(resf) = c("sf_age", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Dry Season"

write.csv(resf2, "ET_Pasture_Dry_full.csv", row.names = F)


#ET Rainy Season
gc()
#Calculating et for Primary Forest
et_pri=ifel(is.na(grass),NA,et_wet)

#Use focal to calculate difference in Secondary and Primary (Delta)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
rm(et_pri)

gc()
et_delta_pri=et_wet-et_f
rm(et_year, et_f)
plot(et_delta_pri)

gc()
resf=as.data.frame(c(sf,et_delta_pri, sf_perc))
rm(grass2, et_delta_pri)
colnames(resf) = c("sf_age", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Rainy Season"

write.csv(resf2, "ET_Pasture_Rainy_full.csv", row.names = F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
