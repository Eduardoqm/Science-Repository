#Eco Services Scenarios (Dry) - LST
#Eduardo Q Marques 04-02-2026

library(terra)

setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario")
dir()

#Load data ---------------------------------------------------------------------
lst_cur = rast("LST_Dry_current.tif")
scf = rast("MB_Forest_age_70m.tif")
past = rast("Pasture_70m.tif")
prf = rast("Forest_70m.tif")
dlt_lst_p = rast("Pasture_Delta_LST_Dry.tif")

plot(lst_cur)
plot(dlt_lst_p)

#Scenery 1 -> Coverting SF in Pasture ------------------------------------------
dlt_lst_p2 = ifel(is.na(scf), NA, dlt_lst_p) #Filtering SF
plot(dlt_lst_p2)

dlt_lst_p3 = dlt_lst_p2*-1 #Inverting signal to make sense
plot(dlt_lst_p3)

#Scenery 1
scn_p = lst_cur+dlt_lst_p3
scn_p2 = ifel(is.na(scn_p), lst_cur, scn_p)

mean(values(scn_p2),na.rm=T) #33.81549
mean(values(lst_cur),na.rm=T) #33.76452

#Result
33.81 - 33.76 #0.05
#Converting SF in Pasture increase the air temperature in 0.05°C.

#Scenery 2 -> Converting Pasture in a 38 year old Forest -----------------------
mean(3.27, 3.51, 3.30) #delta in 38 years old from the three years (2022 to 2024)
sf_lmar = -3.27 #Limiar value

past_lst = ifel(is.na(past), NA, lst_cur) #Filtering Pasture pixels

#Scenery 2
scn_sf = past_lst+sf_lmar
scn_sf2 = ifel(is.na(scn_sf), lst_cur, scn_sf)

mean(values(scn_sf2),na.rm=T) #32.86762

#Result
33.76 - 32.86 #0.9
#Converting Pasture to 38 year old SF, the air temperature decrease 0.9°C.













x11()
plot(lst_cur)
x11()
plot(scn_sf2)















plot(scn_p)

x11()
plot(lst_cur)
x11()
plot(scn_p2)
#writeRaster(scn_p2, "LST_Annual_pasture_scenario.tif")

df_cur = as.data.frame(lst_cur)
df_scn = as.data.frame(scn_p2)

df_cur$cond = "Current"
df_scn$cond = "Converted to Pasture"
df = rbind(df_cur, df_scn)
#write.csv(df, "LST_Annual_pasture_scenario.csv")


library(ggplot2)

x11()
ggplot(df, aes(x=cond))+
  geom_boxplot()








