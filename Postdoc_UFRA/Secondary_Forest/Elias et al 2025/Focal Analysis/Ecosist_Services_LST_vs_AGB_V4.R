#LST by ESA Biomass (Focal)
#Tile-safe scripting — earth-compatible version without chunk size.
# Eduardo Q Marques 02-12-2025

library(terra)
library(dplyr)
library(data.table)

#Save temporary files in disk (To make RAM free) -------------------------------
terraOptions(
  tempdir  = "C:/TEMP_RASTERS",
  memfrac  = 0.6,
  progress = 1
)
dir.create("C:/TEMP_RASTERS", showWarnings = FALSE)

#Load rasters ------------------------------------------------------------------
lst_year = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/LST_Landsat_Annual_2022_70m.tif")
lst_dry  = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/LST_Landsat_Dry_2022_70m.tif")
lst_wet  = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/LST_Landsat_Wet_2022_70m.tif")

fr_pri   = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/Forest_70m.tif")
esa      = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/ESA_Biomass_70m.tif")
sf_perc  = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/Perc_SecForest_70m.tif")

#Focal-to-disk -----------------------------------------------------------------
focal_to_disk <- function(input_rast, w = 21, out_tif, overwrite = TRUE) {
  
  message("→ Running focal() safely by tiles: ", out_tif)
  
  out <- focal(
    input_rast,
    w = w,
    fun = median,
    na.rm = TRUE,
    na.policy = "only",
    filename = out_tif,
    overwrite = overwrite,
    wopt = list(
      datatype  = "FLT4S",
      gdal      = c("TILED=YES", "COMPRESS=DEFLATE")
    )
  )
  return(rast(out_tif))
}

# -------------------- Safe tile reader (unchanged) --------------------
# --------------------------------------------------------------
# SAFE TILE READER (final, no recursion, no crash)
# --------------------------------------------------------------
read_tile <- function(r, row, nrows) {
  
  # limite superior do raster
  max_rows <- nrow(r)
  
  # corrigir nrows se ultrapassar o fim
  if (row + nrows - 1 > max_rows) {
    nrows <- max_rows - row + 1
  }
  
  # tentativa 1 — leitura direta normal
  v <- try(readValues(r, row = row, nrows = nrows), silent = TRUE)
  if (!inherits(v, "try-error")) return(v)
  
  message(" → Tile read failed, retrying in half-size chunks...")
  
  # tentativa 2 — ler em partes menores
  chunk <- floor(nrows / 2)
  if (chunk < 20) {
    warning("Tile too small to safely split. Returning NA block.")
    return(matrix(NA_real_, nrow = nrows, ncol = nlyr(r)))
  }
  
  # ler duas metades separadas
  v1 <- try(readValues(r, row = row,        nrows = chunk), silent = TRUE)
  v2 <- try(readValues(r, row = row+chunk, nrows = nrows-chunk), silent = TRUE)
  
  if (inherits(v1,"try-error") || inherits(v2,"try-error")) {
    warning(" → Chunked read failed, inserting NA tile.")
    return(matrix(NA_real_, nrow = nrows, ncol = nlyr(r)))
  }
  
  # juntar e devolver
  return(rbind(v1, v2))
}

# -------------------- MAIN PROCESS FUNCTION --------------------
process_condition <- function(lst_r, cond_name, out_csv_path) {
  
  message("\n=================================================")
  message("     STARTING CONDITION → ", cond_name)
  message("=================================================\n")
  
  t0 <- Sys.time()
  
  # --- 1) mask primary forest -------------------------
  lst_pri_file <- paste0("lst_pri_", cond_name, ".tif")
  message("→ Creating primary forest mask: ", lst_pri_file)
  
  lst_pri <- ifel(is.na(fr_pri), NA, lst_r)
  lst_pri <- writeRaster(
    lst_pri,
    filename = lst_pri_file,
    overwrite = TRUE,
    wopt = list(
      datatype  = "FLT4S",
      gdal      = c("TILED=YES", "COMPRESS=DEFLATE")
    )
  )
  rm(lst_pri); gc()
  
  # --- 2) focal median --------------------------------
  lst_f_file <- paste0("lst_f_w21_", cond_name, ".tif")
  lst_f <- focal_to_disk(rast(lst_pri_file), w = 21, out_tif = lst_f_file)
  rm(lst_f); gc()
  
  # --- 3) delta = lst - focal --------------------------
  lst_delta_file <- paste0("lst_delta_pri_", cond_name, ".tif")
  message("→ Computing delta: ", lst_delta_file)
  
  delta_r <- lst_r - rast(lst_f_file)
  
  writeRaster(
    delta_r,
    filename = lst_delta_file,
    overwrite = TRUE,
    wopt = list(
      datatype  = "FLT4S",
      gdal      = c("TILED=YES", "COMPRESS=DEFLATE")
    )
  )
  rm(delta_r); gc()
  
  # ---------------- CSV OUTPUT INIT ---------------------
  fwrite(
    data.frame(agb = numeric(0), delta_lst = numeric(0), sf_perc = numeric(0), cond = character(0)),
    file = out_csv_path,
    sep = ",",
    col.names = TRUE
  )
  
  # ---------------- tile-by-tile extraction -------------------
  r_delta <- rast(lst_delta_file)
  r_esa   <- esa
  r_sf    <- sf_perc
  
  blocksize <- 2000
  n_total   <- nrow(r_delta)
  n_blocks  <- ceiling(n_total / blocksize)
  
  message("→ Iterating in ", n_blocks, " tile blocks")
  
  for (i in seq_len(n_blocks)) {
    
    rowstart <- (i - 1) * blocksize + 1
    nrows    <- ifelse(i < n_blocks, blocksize, n_total - (i - 1) * blocksize)
    
    message("  • Tile ", i, "/", n_blocks)
    
    vals_delta <- read_tile(r_delta, rowstart, nrows)
    vals_esa   <- read_tile(r_esa,   rowstart, nrows)
    vals_sf    <- read_tile(r_sf,    rowstart, nrows)
    
    df <- data.frame(
      agb       = vals_esa[,1],
      delta_lst = vals_delta[,1],
      sf_perc   = vals_sf[,1]
    )
    
    df <- df[complete.cases(df), ]
    
    if (nrow(df) > 0) {
      df$cond <- cond_name
      fwrite(df, file = out_csv_path, sep = ",", append = TRUE, col.names = FALSE)
    }
    
    rm(vals_delta, vals_esa, vals_sf, df)
    gc()
  }
  
  rm(r_delta, r_esa, r_sf)
  gc()
  
  t1 <- Sys.time()
  message("\n→ Condition ", cond_name, " completed in ",
          round(difftime(t1, t0, units = "mins"), 2), " min.\n")
}

# -------------------- RUN ALL CONDITIONS -----------------------------
start <- Sys.time()

process_condition(lst_year, "Annual", "LST_AGB_Annual_full_2022.csv")
gc()

process_condition(lst_dry,  "Dry_Season", "LST_AGB_Dry_full_2022.csv")
gc()

process_condition(lst_wet,  "Rainy_Season", "LST_AGB_Rainy_full_2022.csv")
gc()

end <- Sys.time()
message("\nTOTAL TIME: ", round(difftime(end, start, units = "mins"), 2), " min.\n")

