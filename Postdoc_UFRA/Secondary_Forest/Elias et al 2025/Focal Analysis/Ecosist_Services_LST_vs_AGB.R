#LST by ESA Biomass (Focal)

#Eduardo Q Marques 08-09-2025

library(terra)
library(tidyverse)
library(sf)
library(future) #Multicore work

parallel::detectCores()

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

#LST Annual
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(fr_pri),NA,lst_year)
#plot(lst_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(lst_f)
rm(lst_pri)

lst_delta_pri=lst_year-lst_f
rm(lst_year, lst_f)
plot(lst_delta_pri)

esa2 = ifel(is.na(lst_delta_pri),NA, esa)
plot(esa2)

resf=as.data.frame(c(esa2,lst_delta_pri, sf_perc))
rm(esa2, lst_delta_pri)
colnames(resf) = c("agb", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
rm(resf)
resf2$cond = "Annual"

write.csv(resf2, "LST_AGB_Annual_full.csv", row.names = F)
rm(resf2)

#LST Dry Season
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(fr_pri),NA,lst_dry)
#plot(lst_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(lst_f)
rm(lst_pri)

lst_delta_pri=lst_dry-lst_f
rm(lst_dry, lst_f)
plot(lst_delta_pri)

esa2 = ifel(is.na(lst_delta_pri),NA, esa)
plot(esa2)

resf=as.data.frame(c(esa2,lst_delta_pri, sf_perc))
rm(esa2, lst_delta_pri)
colnames(resf) = c("agb", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
rm(resf)
resf2$cond = "Annual"

write.csv(resf2, "LST_AGB_Dry_full.csv", row.names = F)
rm(resf2)

#LST Rainy Season
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(fr_pri),NA,lst_wet)
#plot(lst_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(lst_f)
rm(lst_pri)

lst_delta_pri=lst_wet-lst_f
rm(lst_wet, lst_f)
plot(lst_delta_pri)

esa2 = ifel(is.na(lst_delta_pri),NA, esa)
plot(esa2)

resf=as.data.frame(c(esa2,lst_delta_pri, sf_perc))
rm(esa2, lst_delta_pri)
colnames(resf) = c("agb", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
rm(resf)
resf2$cond = "Annual"

write.csv(resf2, "LST_AGB_Rainy_full.csv", row.names = F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken








