#Paper VPD ERA5 Amazon
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 20-08-2026

library(terra)

#Load data ---------------------------------------------------------------------
#Secondary Forest
scf23 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")
scf24 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")

plot(scf23)
plot(scf24)

#VPD >= 075 kPa hours by month
list_rst = list.files("G:/My Drive/GEE_VPD_Horas_2024", full.names = T); list_rst
h_vpd = rast(list_rst)
plot(h_vpd)

#Get only SF pixels ------------------------------------------------------------
h_vpd2 = resample(h_vpd, scf24, method = "average")
