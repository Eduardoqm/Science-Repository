#Paper VPD ERA5 Amazon
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 20-08-2026

library(terra)

#Load data ---------------------------------------------------------------------
#Secondary Forest
scf23 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")
scf23 = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2023.tif")

plot(scf23)
plot(scf24)

#VPD >= 075 kPa hours by month -------------------------------------------------
list_rst = list.files()
month_vpd = rast(list_rst)
plot(month_vpd)
