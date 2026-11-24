#Proccess and Analysis extracting (FIRE) - H2
#Eduardo Q Marques 24-11-2026

library(terra)
library(sf)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#Driest Period length
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
names(vpd_zone) = "zone"
plot(vpd_zone)

#Mapbiomas
scf = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2024B.tif")
plot(scf)

fire_freq = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/Fire/MB_Fire_frequency_1985_2025.tiff")
plot(fire_freq)

#Amazonia limits
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Extrating values --------------------------------------------------------------
total <- ncell(scf)

#Processing by blocks
bloco <- 1e6  #1 million

#Way to output
caminho_saida <- "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/Dados_pixel_analises_FIRE_H2.csv"

start.time <- Sys.time()
first_block <- TRUE

for (ini in seq(1, total, by = bloco)) {

  fim <- min(ini + bloco - 1, total)
  idx_bloco <- ini:fim

  cat(sprintf("Processing pixels %.0f of %.0f de %.0f (%.1f%%)\n",
              ini, fim, total, 100 * fim / total))

  val_bloco <- scf[idx_bloco][, 1]

  validos_bloco <- !is.na(val_bloco) #To not proccess NAs

  if (any(validos_bloco)) {
    idx_validos <- idx_bloco[validos_bloco]
    val_validos <- val_bloco[validos_bloco]
    xy_bloco <- xyFromCell(scf, idx_validos)

    df_zone <- terra::extract(vpd_zone, xy_bloco) #Extract values from zone
    df_freq <- terra::extract(fire_freq, xy_bloco) #Extract values from zone
    
    df_bloco = cbind(df_zone, df_freq)

#Include xy
    df_bloco$age <- as.numeric(val_validos)
    df_bloco$x   <- xy_bloco[, 1]
    df_bloco$y   <- xy_bloco[, 2]

#Saving CSV (create first time and append after)
    write.table(df_bloco, caminho_saida,
                sep = ",", dec = ".",
                row.names = FALSE,
                col.names = first_block,
                append = !first_block)

    first_block <- FALSE
  }

#Clean memory
  rm(val_bloco, validos_bloco, xy_bloco, df_bloco)
  if (exists("idx_validos")) rm(idx_validos, val_validos)
  gc()
}

print(Sys.time() - start.time) #Proccessing in 2.18hr (Laptop)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    