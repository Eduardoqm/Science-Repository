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

#Focal Function -------------------------------------------------------------
setwd("C:/Users/Public/Documents/Analises_Elias/Dados/LST")
start.time <- Sys.time()


#LST Annual
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(fr_pri),NA,lst_year)
#plot(lst_pri)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(lst_f)

lst_delta_pri=lst_year-lst_f
plot(lst_delta_pri)

resf=as.data.frame(c(sf,lst_delta_pri, sf_perc))
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Annual"

write.csv(resf2, "LST_SecFor_Age_Annual_full_2022.csv", row.names = F)


#LST Dry Season
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(fr_pri),NA,lst_dry)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")

lst_delta_pri=lst_dry-lst_f
plot(lst_delta_pri)

resf=as.data.frame(c(sf,lst_delta_pri, sf_perc))
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Dry Season"

write.csv(resf2, "LST_SecFor_Age_Dry_full_2022.csv", row.names = F)


#LST Rainy Season
#Calculating LST for Primary Forest
lst_pri=ifel(is.na(fr_pri),NA,lst_wet)

#Use focal to calculate difference in Secondary and Primary (Delta)
lst_f <- focal(lst_pri, w=21, median, na.rm=TRUE, na.policy="only")
#plot(lst_f)

lst_delta_pri=lst_wet-lst_f
plot(lst_delta_pri)

resf=as.data.frame(c(sf,lst_delta_pri, sf_perc))
colnames(resf) = c("sf_age", "delta_lst", "sf_perc")
resf2 = resf %>% na.omit()
resf2$cond = "Rainy Season"

write.csv(resf2, "LST_SecFor_Age_Rainy_full_2022.csv", row.names = F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

