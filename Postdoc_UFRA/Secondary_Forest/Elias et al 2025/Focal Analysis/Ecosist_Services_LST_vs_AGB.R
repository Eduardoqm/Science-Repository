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

