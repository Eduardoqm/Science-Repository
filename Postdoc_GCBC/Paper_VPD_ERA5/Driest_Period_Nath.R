#Driest Period - Nathalia Rasters
#Eduardo Q Marques 18-10-2026

library(terra)

setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia")
dir()

dp_start = rast("DP_onset_cy.tif")
dp_end = rast("DP_end_cy.tif")
wet_peak = rast("wet_peak_cy.tif")

plot(dp_start, main = "DP Start")
plot(dp_end, main = "DP End")
plot(wet_peak, main = "Wet Peak")

am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Calendar convertion to hidrologic year ----------------------------------------
dp_start_hy = ((dp_start - wet_peak + 73) %% 73) + 1
dp_end_hy   = ((dp_end - wet_peak + 73) %% 73) + 1

plot(dp_start_hy)
plot(dp_end_hy)

#Number of Dry months ----------------------------------------------------------
dp_length_pentads = dp_end_hy - dp_start_hy + 1
plot(dp_length_pentads)

dp_length_pentads = mask(crop(dp_length_pentads, am), am)
plot(dp_length_pentads)

dp_length_days = dp_length_pentads * 5 # Pentad ~5 days
plot(dp_length_days)

dp_length_months = dp_length_days / (365 / 12) # One month = 365 / 12
plot(dp_length_months)

dp_months = round(dp_length_months)
plot(dp_months)

#Exporting rasters -------------------------------------------------------------
writeRaster(dp_start_hy, "DP_onset_hy.tif", overwrite = TRUE)
writeRaster(dp_end_hy, "DP_end_hy.tif", overwrite = TRUE )
writeRaster(dp_months, "DP_length_months.tif", overwrite = TRUE )

#Clustering Dry duration -------------------------------------------------------
rcl_matrix = matrix(c(
  0, 3,  1,   # Grupo 1: Seca Curta (0 a 3 meses)
  3, 6,  2,   # Grupo 2: Seca Moderada (3 a 6 meses)
  6, 8,  3,   # Grupo 3: Seca Longa (6 a 9 meses)
  8, 12, 4),    # Grupo 4: Seca Muito Longa (9 a 12 meses)
  ncol = 3, byrow = TRUE)

dp_zones_4g = classify(dp_months, rcl_matrix, include.lowest = TRUE)
dp_zones_4g = as.factor(dp_zones_4g)

levels(dp_zones_4g) = data.frame(id = 1:4, 
  zone = c("Short (0-3m)", "Moderate (3-6m)", "Long (6-8m)", "Extra Long (8-12m)"))

plot(dp_zones_4g, main = "Dry Season Pattern (Nathalia data)",
     col = c("blue", "lightgreen", "orange", "red"))














































