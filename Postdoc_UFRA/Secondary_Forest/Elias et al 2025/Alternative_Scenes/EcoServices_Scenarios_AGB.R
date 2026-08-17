#Eco Services Scenarios Biomass
#Eduardo Q Marques 17-08-2026

library(terra)
library(tidyverse)

setwd("/home/leaf/Documentos/Serrapilheira _Elias et al/Scenery")
dir()

#Load data ---------------------------------------------------------------------
esa = rast("ESA_Biomass_70m.tif"  )
scf = rast("MB_Forest_age_70m.tif")
past = rast("Pasture_70m.tif")
prf = rast("Forest_70m.tif")
dlt_esa_p = rast("Delta_AGB_Forest_age_Pasture.tif")
df_delta = read.csv("Pasture_AGB_age_full_C.csv")

plot(esa)
plot(dlt_esa_p)


#Scenery 1 -> Coverting SF in Pasture ------------------------------------------
dlt_esa_p2 = ifel(is.na(scf), NA, dlt_esa_p) #Filtering SF
plot(dlt_esa_p2)

dlt_esa_p3 = dlt_esa_p2*-1 #Inverting signal to make sense
plot(dlt_esa_p3)

#Scenery 1
scn_p = esa+dlt_esa_p3
scn_p2 = ifel(is.na(scn_p), esa, scn_p)

mean(values(scn_p2),na.rm=T) #204.06
mean(values(esa),na.rm=T) #206.02

#Result
204.06 - 206.02 #-1.95
#Converting SF in Pasture decrease the AGB in 1.95 M/ton.


#Scenery 2 and 3-> Converting Pasture in sf and persisting to 38 years ---------
df_delta2 = df_delta %>% 
  filter(cond == "Dry Season") %>% 
  group_by(age) %>% 
  summarise(delta_lst = mean(delta_lst))

#Input min, mean and maximun
max_lmar = min(df_delta2$delta_lst) #Delta in 38 years (oldest SF)
mean_lmar = mean(df_delta2$delta_lst) #Mean delta
min_lmar = max(df_delta2$delta_lst) #Delta for youngster SF

past_lst = ifel(is.na(past), NA, esa) #Filtering Pasture pixels

#Delta for youngster SF
scn_min_sf = past_lst+min_lmar
scn_min_sf2 = ifel(is.na(scn_min_sf), esa, scn_min_sf)

mean(values(scn_min_sf2),na.rm=T) #33.6648

#Result
33.66 - 33.76 #-0.1
#Converting Pasture to young SF, the air temperature decrease 0.1°C.

#Mean delta
scn_mean_sf = past_lst+mean_lmar
scn_mean_sf2 = ifel(is.na(scn_mean_sf), esa, scn_mean_sf)

mean(values(scn_mean_sf2),na.rm=T) #33.11481

#Result
33.11 - 33.76 #-0.65
#Converting Pasture to SF, in mean the air temperature decrease 0.65°C.


#Mean delta
scn_max_sf = past_lst+max_lmar
scn_max_sf2 = ifel(is.na(scn_max_sf), esa, scn_max_sf)

mean(values(scn_max_sf2),na.rm=T) #32.8423

#Result
32.84 - 33.76 #-0.92
#Converting Pasture to 38 year old SF, the air temperature decrease 0.9°C.


#Saving rasters of scenarios ---------------------------------------------------
setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario/Scenery_Raster")
writeRaster(scn_p2, "Scenary_LST_SF_to_Pasture.tif")
writeRaster(scn_min_sf2, "Scenary_LST_Pasture_to_young_SF.tif")
writeRaster(scn_mean_sf2, "Scenary_LST_Pasture_to_mean_SF.tif")
writeRaster(scn_max_sf2, "Scenary_LST_Pasture_to_old_SF.tif")


