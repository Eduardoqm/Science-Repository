#Paper VPD ERA5 Amazon
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 20-08-2026

library(terra)

#Load data ---------------------------------------------------------------------
#Secondary Forest
#Leptop
#scf23 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")
#scf24 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")

#Workstation
scf23 = rast("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/MB_Forest_age_2023.tif")
scf24 = rast("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/MB_Forest_age_2024.tif")

plot(scf23)
plot(scf24)

#VPD >= 075 kPa hours by month
#Leptop
#list_rst = list.files("G:/My Drive/GEE_VPD_Horas_2024", full.names = T); list_rst

#Workstation
list_rst = list.files("/home/leaf/Documentos/Paper_VPD_Marques_et_al/Rasters_H2/VPD_month", full.names = T); list_rst

h_vpd = rast(list_rst)
plot(h_vpd)

#Get only SF pixels ------------------------------------------------------------
#h_vpd2 = resample(h_vpd, scf24, method = "average")
scf24b = resample(scf24, h_vpd[[1]], method = "average")
plot(scf24b)

#Percentage of SF
scf_count = ifel(scf24 < 1, 0, 1)
plot(scf_count)

scf_count = resample(scf_count, h_vpd[[1]], method = "sum")
plot(scf_count)

scf_perc = (scf_count/80000)*100
plot(scf_perc)

scf_50 = ifel(scf_perc < 50, NA, scf_perc)
plot(scf_50)

age = ifel(scf_perc <50, NA, scf24b)
plot(age)

df = as.data.frame(age)




