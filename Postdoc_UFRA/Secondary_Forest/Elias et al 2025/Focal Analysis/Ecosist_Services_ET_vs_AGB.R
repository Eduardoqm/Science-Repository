#ET by ESA Biomass (Focal)

#Eduardo Q Marques 08-09-2025

library(terra)
library(tidyverse)
library(sf)
library(future) #Multicore work

parallel::detectCores()

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

#Primary Forest
fr_pri = rast("Forest_70m.tif" )
plot(fr_pri)

#ESA Biomass
esa = rast("ESA_Biomass_70m.tif" )
plot(esa)

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
#Calculating et for Primary Forest
et_pri=ifel(is.na(fr_pri),NA,et_year)
#plot(et_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(et_f)
rm(et_pri)

et_delta_pri=et_year-et_f
rm(et_year, et_f)
plot(et_delta_pri)

esa2 = ifel(is.na(et_delta_pri),NA, esa)
plot(esa2)

resf=as.data.frame(c(esa2,et_delta_pri, sf_perc))
rm(esa2, et_delta_pri)
colnames(resf) = c("agb", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
rm(resf)
resf2$cond = "Annual"

write.csv(resf2, "ET_AGB_Annual_full.csv", row.names = F)
rm(resf2)

#ET Dry Season
#Calculating et for Primary Forest
et_pri=ifel(is.na(fr_pri),NA,et_dry)
#plot(et_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(et_f)
rm(et_pri)

et_delta_pri=et_dry-et_f
rm(et_dry, et_f)
plot(et_delta_pri)

esa2 = ifel(is.na(et_delta_pri),NA, esa)
plot(esa2)

gc()
resf=as.data.frame(c(esa2,et_delta_pri, sf_perc))
rm(esa2, et_delta_pri)
colnames(resf) = c("agb", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
rm(resf)
resf2$cond = "Dry"

write.csv(resf2, "ET_AGB_Dry_full.csv", row.names = F)
rm(resf2)

#ET Rainy Season
gc()
#Calculating et for Primary Forest
et_pri=ifel(is.na(fr_pri),NA,et_wet)
#plot(et_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
et_f <- focal(et_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(et_f)
rm(et_pri)

et_delta_pri=et_wet-et_f
rm(et_wet, et_f)
plot(et_delta_pri)

esa2 = ifel(is.na(et_delta_pri),NA, esa)
plot(esa2)

gc()
resf=as.data.frame(c(esa2,et_delta_pri, sf_perc))
rm(esa2, et_delta_pri)
colnames(resf) = c("agb", "delta_et", "sf_perc")
resf2 = resf %>% na.omit()
rm(resf)
resf2$cond = "Rainy"

write.csv(resf2, "ET_AGB_Rainy_full.csv", row.names = F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken