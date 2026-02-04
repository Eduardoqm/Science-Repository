#Eco Services Scenarios (Dry)
#Eduardo Q Marques 04-02-2026

library(terra)

setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario")
dir()

#Means from classes ------------------------------------------------------------
lst_cur = rast("LST_Dry_current.tif")
scf = rast("MB_Forest_age_70m.tif")
past = rast("Pasture_70m.tif")
prf = rast("Forest_70m.tif")

#scf_t = ifel(is.na(scf), NA, lst_cur)
#past_t = ifel(is.na(past), NA, lst_cur)
#prf_t = ifel(is.na(prf), NA, lst_cur)

#mean(values(scf_t),na.rm=T)
#mean(values(past_t),na.rm=T)
#mean(values(prf_t),na.rm=T)

#By Secondary Forest age
#scf_t10 = ifel(scf>10, NA, scf_t)
#scf_t20 = ifel(scf == (20:29), scf_t, NA)
#scf_t30 = ifel(scf<30, NA, scf_t)

#mean(values(scf_t10),na.rm=T)
#mean(values(scf_t20),na.rm=T)
#mean(values(scf_t30),na.rm=T)

#-------------------------------------------------------------------------------
#lst_cur = rast("LST_Dry_current.tif")
#dlt_lst_f = rast("Forest_Delta_LST_annual.tif")
dlt_lst_p = rast("Pasture_Delta_LST_Dry.tif")
#scf = rast("MB_Forest_age_70m.tif")
#sc_perc = rast("Perc_SecForest_70m.tif" ) #Testing percentage >70

plot(lst_cur)
#plot(dlt_lst_f)
plot(dlt_lst_p)

#lst_cur_f = ifel(is.na(dlt_lst_f), NA, lst_cur)
#scn_f = lst_cur+dlt_lst_f
#plot(scn_f)

dlt_lst_p2 = ifel(is.na(scf), NA, dlt_lst_p) #Filtering SF
plot(dlt_lst_p2)

#dlt_lst_p3 = ifel(dlt_lst_p2 < 0 , NA, dlt_lst_p2) #Positve deltas
#plot(dlt_lst_p3)
#dlt_lst_p3 = ifel(scf > 19, dlt_lst_p2, NA) #Filtering age
dlt_lst_p3 = dlt_lst_p2*-1
plot(dlt_lst_p3)

scn_p = lst_cur+dlt_lst_p3
#scn_p2 = ifel(sc_perc > 69, scn_p, NA) #Filtering percentages
scn_p3 = ifel(is.na(scn_p), lst_cur, scn_p)

mean(values(scn_p3),na.rm=T) #33.81549
mean(values(lst_cur),na.rm=T) #33.76452

#Converting SF in Pasture increase the air temperature in 0.05°C.

#Converting Pasture to 20 year old Forest --------------------------------------
mean(3.27, 3.51, 3.30) #delta in 38 years old from the trhee years (2022 to 2024)
sf_lmar = -3.27 #Limiar value

past_lst = ifel(is.na(past), NA, lst_cur)

scn_sf = past_lst+sf_lmar
scn_sf2 = ifel(is.na(scn_sf), lst_cur, scn_sf)


mean(values(scn_sf2),na.rm=T) #33.13367

33.76 - 

#Converting Pasture to 38 year old SF, the air temperature decrease 0.63°C.





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








