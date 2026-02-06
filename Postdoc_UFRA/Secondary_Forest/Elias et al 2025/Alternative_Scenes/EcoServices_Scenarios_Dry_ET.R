#Eco Services Scenarios (Dry) - ET
#Eduardo Q Marques 05-02-2026

library(terra)

setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario")
dir()

#Load data ---------------------------------------------------------------------
et_cur = rast("ET_Dry_current.tif")
scf = rast("MB_Forest_age_70m.tif")
past = rast("Pasture_70m.tif")
prf = rast("Forest_70m.tif")
dlt_et_p = rast("Pasture_Delta_ET_Dry.tif")

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



