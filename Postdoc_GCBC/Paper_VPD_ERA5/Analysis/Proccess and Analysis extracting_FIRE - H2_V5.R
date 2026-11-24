#Proccess and Analysis extracting (FIRE) - H2
#Eduardo Q Marques 24-11-2026

library(terra)
library(sf)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#Driest Period length
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
plot(vpd_zone)

#Mapbiomas
fire = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Fire_frequency_1985_2025.tiff")
plot(fire)

scf = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2024B.tif")
plot(scf)

#Amazonia limits
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Extrating values --------------------------------------------------------------
total <- ncell(scf)

#Processing by blocks
bloco <- 1e6  #1 million

#Way to output
caminho_saida <- "G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Paper1_VPD_ERA5/H2/Dados_pixel_analises_FIRE_H2.csv"

# Remove previous file if it exists to avoid duplicating data on re-run
if (file.exists(caminho_saida)) file.remove(caminho_saida)

start.time <- Sys.time()
first_block <- TRUE

for (ini in seq(1, total, by = bloco)) {
  
  fim <- min(ini + bloco - 1, total)
  idx_bloco <- ini:fim
  
  cat(sprintf("Processing pixels %.0f to %.0f of %.0f (%.2f%%)\n",
              ini, fim, total, 100 * fim / total))
  
  # Extract values from base raster (scf)
  val_bloco <- scf[idx_bloco][, 1]
  validos_bloco <- !is.na(val_bloco)
  
  if (any(validos_bloco)) {
    idx_validos <- idx_bloco[validos_bloco]
    val_validos <- val_bloco[validos_bloco]
    xy_bloco    <- xyFromCell(scf, idx_validos)
    
    # Extract VPD_zone and Fire (ID = FALSE returns value columns directly in the exact order of xy_bloco)
    ext_vpd  <- terra::extract(vpd_zone, xy_bloco, ID = FALSE)
    ext_fire <- terra::extract(fire, xy_bloco, ID = FALSE)
    
    # Check if both extractions returned valid data (at least 1 column and matching row count)
    if (is.null(ext_vpd) || is.null(ext_fire) ||
        ncol(ext_vpd) == 0 || ncol(ext_fire) == 0 ||
        nrow(ext_vpd) != nrow(xy_bloco) || nrow(ext_fire) != nrow(xy_bloco)) {
      next
    }
    
    # Build final data.frame directly (no merge needed)
    df_bloco <- data.frame(
      age      = as.numeric(val_validos),
      x        = xy_bloco[, 1],
      y        = xy_bloco[, 2],
      vpd_zone = ext_vpd[, 1],
      fire     = ext_fire[, 1]
    )
    
    # (Optional) Uncomment the line below to drop pixels where both vpd_zone and fire are NA:
    # df_bloco <- df_bloco[!is.na(df_bloco$vpd_zone) | !is.na(df_bloco$fire), ]
    
    if (nrow(df_bloco) > 0) {
      # fwrite is much faster than write.table and handles headers automatically
      data.table::fwrite(
        df_bloco,
        file = caminho_saida,
        sep = ",",
        dec = ".",
        row.names = FALSE,
        col.names = first_block,
        append = !first_block
      )
      
      first_block <- FALSE
    }
  }
  
  # Periodic garbage collection (every 10 blocks) to avoid slowing down the loop
  if ((ini - 1) %% (bloco * 10) == 0) {
    gc(verbose = FALSE)
  }
}

print(Sys.time() - start.time) #Proccessing in 2.18hr (Laptop)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    