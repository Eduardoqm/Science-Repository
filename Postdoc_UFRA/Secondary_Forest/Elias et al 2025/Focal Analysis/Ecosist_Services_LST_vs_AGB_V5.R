# -----------------------------------------------------------
# CARREGAR PACOTES
# -----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)
library(future)
library(future.apply)

# -----------------------------------------------------------
# CONFIGURAÇÕES GERAIS
# -----------------------------------------------------------
setwd("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m")

plan(multisession, workers = max(1, parallel::detectCores() - 2))

if (!dir.exists("C:/TEMP_RASTERS")) {
  dir.create("C:/TEMP_RASTERS")
}

terraOptions(
  memfrac = 0.5,
  tempdir = "C:/TEMP_RASTERS",
  threads = max(1, parallel::detectCores() - 2)
)

# -----------------------------------------------------------
# CARREGAR RASTERS
# -----------------------------------------------------------
lst_year <- rast("LST_Landsat_Annual_2022_70m.tif")
lst_dry  <- rast("LST_Landsat_Dry_2022_70m.tif")
lst_wet  <- rast("LST_Landsat_Wet_2022_70m.tif")

fr_pri   <- rast("Forest_70m.tif")
esa      <- rast("ESA_Biomass_70m.tif")
sf_perc  <- rast("Perc_SecForest_70m.tif")

# Pré-processamento
#sf_perc <- ifel(sf_perc == 0, NA, sf_perc)
setwd("C:/Users/Public/Documents/Analises_Elias/Dados/LST")

# -----------------------------------------------------------
# FUNÇÃO PARA PROCESSAR UM LST EM BLOCOS
# -----------------------------------------------------------
process_lst_block <- function(lst_rast, cond_name, output_file, nlines_block = 100) {
  
  message("=== Iniciando processamento: ", cond_name, " ===")
  
  # Inicializar CSV vazio
  write.csv(data.frame(agb=NA, delta_lst=NA, sf_perc=NA, cond=NA)[0,],
            output_file, row.names=FALSE)
  
  nrows <- nrow(lst_rast)
  
  # Raster de referência para focal
  lst_pri <- ifel(is.na(fr_pri), NA, lst_rast)
  lst_f <- focal(lst_pri, w=21, fun=median, na.rm=TRUE, na.policy="only")
  lst_delta <- lst_rast - lst_f
  esa2 <- ifel(is.na(lst_delta), NA, esa)
 
  # Processar em blocos de linhas
  for(start_row in seq(1, nrows, by=nlines_block)) {
    end_row <- min(start_row + nlines_block - 1, nrows)
    
    # Ler apenas os valores do bloco
    r_block <- values(lst_rast, row=start_row, nrows=end_row-start_row+1)
    f_block <- values(lst_f, row=start_row, nrows=end_row-start_row+1)
    esa_block <- values(esa2, row=start_row, nrows=end_row-start_row+1)
    sf_block <- values(sf_perc, row=start_row, nrows=end_row-start_row+1)
    
    # Converter para data.frame
    df_block <- data.frame(
      agb = esa_block,
      delta_lst = r_block - f_block,
      sf_perc = sf_block,
      cond = cond_name
    )
    
    df_block <- na.omit(df_block)
    
    # Salvar no CSV (append)
    write.table(df_block, output_file, sep=",", row.names=FALSE,
                col.names=FALSE, append=TRUE)
    
    message("Bloco de linhas ", start_row, " a ", end_row, " concluído")
  }
  
  message("=== Condição ", cond_name, " concluída ===")
}

# -----------------------------------------------------------
# RODAR TODAS AS CONDIÇÕES
# -----------------------------------------------------------
process_lst_block(lst_year, "Annual_2022",      "LST_AGB_Annual_full_2022.csv")
process_lst_block(lst_dry,  "Dry Season_2022",  "LST_AGB_Dry_full_2022.csv")
process_lst_block(lst_wet,  "Rainy Season_2022","LST_AGB_Rainy_full_2022.csv")

