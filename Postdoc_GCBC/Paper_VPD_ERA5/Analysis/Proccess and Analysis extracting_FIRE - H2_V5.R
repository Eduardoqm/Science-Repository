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

start.time <- Sys.time()
first_block <- TRUE

for (ini in seq(1, total, by = bloco)) {
  
  fim <- min(ini + bloco - 1, total)
  idx_bloco <- ini:fim
  
  cat(sprintf("Processing pixels %.0f of %.0f de %.0f (%.1f%%)\n",
              ini, fim, total, 100 * fim / total))
  
  val_bloco <- scf[idx_bloco][, 1]
  
  validos_bloco <- !is.na(val_bloco)
  
  if (any(validos_bloco)) {
    idx_validos <- idx_bloco[validos_bloco]
    val_validos <- val_bloco[validos_bloco]
    xy_bloco    <- xyFromCell(scf, idx_validos)
    
    # Base data.frame with coordinates and a unique ID for alignment
    df_points <- data.frame(
      id  = 1:length(idx_validos),
      age = as.numeric(val_validos),
      x   = xy_bloco[, 1],
      y   = xy_bloco[, 2]
    )
    
    # Extract VPD_zone (remove df argument)
    df_vpd <- terra::extract(vpd_zone, xy_bloco)
    # Extract Fire (remove df argument)
    df_fire <- terra::extract(fire, xy_bloco)
    
    # If either extraction returned zero rows, skip this block
    if (nrow(df_vpd) == 0 || nrow(df_fire) == 0) {
      next
    }
    
    # Rename extracted columns before merging
    names(df_vpd)[2]  <- "vpd_zone"
    names(df_fire)[2] <- "fire"
    
    # Merge by ID to ensure correct pixel alignment
    df_bloco <- merge(df_points, df_vpd, by.x = "id", by.y = "ID", all.x = TRUE)
    df_bloco <- merge(df_bloco, df_fire, by.x = "id", by.y = "ID", all.x = TRUE)
    
    # Drop the ID column
    df_bloco$id <- NULL
    
    # Save CSV
    write.table(df_bloco, caminho_saida,
                sep = ",", dec = ".",
                row.names = FALSE,
                col.names = first_block,
                append = !first_block)
    
    first_block <- FALSE
  }
  
  # Memory cleanup
  if (exists("xy_bloco"))    rm(xy_bloco)
  if (exists("df_vpd"))      rm(df_vpd)
  if (exists("df_fire"))     rm(df_fire)
  if (exists("df_bloco"))    rm(df_bloco)
  if (exists("df_points"))   rm(df_points)
  if (exists("idx_validos")) rm(idx_validos)
  if (exists("val_validos")) rm(val_validos)
  if (exists("val_bloco"))   rm(val_bloco)
  if (exists("validos_bloco")) rm(validos_bloco)
  
  gc()
}

print(Sys.time() - start.time) #Proccessing in 2.18hr (Laptop)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    