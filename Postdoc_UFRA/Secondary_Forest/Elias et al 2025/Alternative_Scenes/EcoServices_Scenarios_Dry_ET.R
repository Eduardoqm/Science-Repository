#Eco Services Scenarios (Dry) - ET
#Eduardo Q Marques 05-02-2026

library(terra)
library(tidyverse)

setwd("G:/Meu Drive/Dados_Elias_paper/et_ET_scenario")
dir()

#Load data ---------------------------------------------------------------------
et_cur = rast("ET_Dry_current.tif")
scf = rast("MB_Forest_age_70m.tif")
past = rast("Pasture_70m.tif")
prf = rast("Forest_70m.tif")
dlt_et_p = rast("Pasture_Delta_ET_Dry.tif")
df_delta = read.csv("G:/Meu Drive/Dados_Elias_paper/Delta_Data_Frame/ET_Pasture_age_FULL.csv")

plot(et_cur)
plot(dlt_et_p)

#Scenery 1 -> Coverting SF in Pasture ------------------------------------------
dlt_et_p2 = ifel(is.na(scf), NA, dlt_et_p) #Filtering SF
plot(dlt_et_p2)

dlt_et_p3 = dlt_et_p2*-1 #Inverting signal to make sense
plot(dlt_et_p3)

#Scenery 1
scn_p = et_cur+dlt_et_p3
scn_p2 = ifel(is.na(scn_p), et_cur, scn_p)

mean(values(scn_p2),na.rm=T) #3.207124
mean(values(et_cur),na.rm=T) #3.211829

#Result
3.20 - 3.21 #-0.01
#Converting SF in Pasture decrease evapotranspiration in 0.01 W/m²/day.


#Scenery 2 and 3-> Converting Pasture in sf and persisting to 38 years ---------
df_delta2 = df_delta %>% 
  filter(cond == "Dry Season") %>% 
  group_by(age) %>% 
  summarise(delta_et = mean(delta_et))

#Input min, mean and maximun
max_lmar = max(df_delta2$delta_et) #Delta in 38 years (oldest SF)
mean_lmar = mean(df_delta2$delta_et) #Mean delta
min_lmar = min(df_delta2$delta_et) #Delta for youngster SF

past_et = ifel(is.na(past), NA, et_cur) #Filtering Pasture pixels

#Delta for youngster SF
scn_min_sf = past_et+min_lmar
scn_min_sf2 = ifel(is.na(scn_min_sf), et_cur, scn_min_sf)

mean(values(scn_min_sf2),na.rm=T) #3.179223

#Result
3.17 - 3.21 #-0.04
#Converting Pasture to young SF, the evapotranspiration decrease 0.04 W/m²/day.

#Mean delta
scn_mean_sf = past_et+mean_lmar
scn_mean_sf2 = ifel(is.na(scn_mean_sf), et_cur, scn_mean_sf)

mean(values(scn_mean_sf2),na.rm=T) #3.271518

#Result
3.27 - 3.21 #0.06
#Converting Pasture to SF, in mean the evapotranspiration increase 0.06 W/m²/day.


#Mean delta
scn_max_sf = past_et+max_lmar
scn_max_sf2 = ifel(is.na(scn_max_sf), et_cur, scn_max_sf)

mean(values(scn_max_sf2),na.rm=T) #3.323752

#Result
3.32 - 3.21 #0.11
#Converting Pasture to 38 year old SF, the evapotranspiration increase 0.11 W/m²/day.








































#Scenery 2 -> Converting Pasture in a 38 year old Forest -----------------------
#mean(0.37, 0.39, 0.45) #delta in 38 years old from the three years (2022 to 2024)
sf_lmar = 0.40 #Limiar value

past_et = ifel(is.na(past), NA, et_cur) #Filtering Pasture pixels

#Scenery 2
scn_sf = past_et+sf_lmar
scn_sf2 = ifel(is.na(scn_sf), et_cur, scn_sf)

mean(values(scn_sf2),na.rm=T) #3.321186

#Result
3.32 - 3.21 #0.11
#Converting Pasture to 38 year old SF, the evapotranspiration increase 0.11 W/m²/day.



