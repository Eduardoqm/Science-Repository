#Driest Period - Nathalia Rasters
#Eduardo Q Marques 30-09-2026

library(terra)

setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia")
dir()

dp_start = rast("DP_onset_cy.tif")
dp_end = rast("DP_end_cy.tif")
wet_peak <- rast("wet_peak_cy.tif")

plot(dp_start, main = "DP Start")
plot(dp_end, main = "DP End")
plot(wet_peak, main = "Wet Peak")

#Calendar convertion to hidrologic year ----------------------------------------
dp_start_hy <- ((dp_start - wet_peak + 73) %% 73) + 1
dp_end_hy   <- ((dp_end - wet_peak + 73) %% 73) + 1

plot(dp_start_hy)
plot(dp_end_hy)

#Number of Dry months ----------------------------------------------------------
dp_length_pentads <- ifel(dp_start >= dp_end,
                          dp_start - dp_end + 1,
                          (73 - dp_end + 1) + dp_start)

plot(dp_length_pentads)

dp_length_days <- dp_length_pentads * 5 #Pentad ~5 days
plot(dp_length_days)

dp_length_months <- dp_length_days / (365 / 12) #One month = 365 / 12
plot(dp_length_months)

dp_months <- round(dp_length_months)
plot(dp_months)


#Exporting rasters -------------------------------------------------------------
writeRaster(dp_start_hy, "DP_onset_hy.tif", overwrite = TRUE)
writeRaster(dp_end_hy, "DP_end_hy.tif", overwrite = TRUE )












