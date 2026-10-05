#Driest Period - Nathalia Rasters
#Eduardo Q Marques 30-09-2026

library(terra)

setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia")
dir()

dp_start = rast("DP_onset_cy.tif")
dp_end = rast("DP_end_cy.tif")
wet_peak <- rast("wet_peak_cy.tif")

plot(dp_start)
plot(dp_end)
plot(wet_peak)

#Calendar convertion to hidrologic year ----------------------------------------
dp_start_hy <- ((dp_start - wet_peak + 73) %% 73) + 1
dp_end_hy   <- ((dp_end - wet_peak + 73) %% 73) + 1

plot(dp_start_hy)
plot(dp_end_hy)


writeRaster(dp_start_hy, "DP_onset_hy.tif", overwrite = TRUE)
writeRaster(dp_end_hy, "DP_end_hy.tif", overwrite = TRUE )












