#LST by ESA Biomass (Focal)

#Jony Brito 09-12-2025

library(terra)
library(sf)
library(tidyverse)

# CONFIGURAÇÕES GERAIS ---------------------------------------------------------
setwd("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m")


terraOptions(
  memfrac = 0.5,  # Terra só pode usar 50% da RAM total do PC
  tempdir = "C:/TEMP_RASTERS", # Pasta de arquivos temporários
)


# CARREGAR RASTERS -------------------------------------------------------------
lst_year <- rast("LST_Landsat_Annual_2022_70m.tif")  # Raster anual
lst_dry  <- rast("LST_Landsat_Dry_2022_70m.tif")     # Raster da estação seca
lst_wet  <- rast("LST_Landsat_Wet_2022_70m.tif")     # Raster da estação chuvosa

fr_pri   <- rast("Forest_70m.tif")                   # Raster de floresta primária
esa      <- rast("ESA_Biomass_70m.tif")              # Biomassa (AGB)
sf_perc  <- rast("Perc_SecForest_70m.tif")           # Percentual de floresta secundária

# Pré-processamento: transforma zeros em NA (0 = área sem floresta secundária)
sf_perc <- ifel(sf_perc == 0, NA, sf_perc)

# FUNÇÃO PARA PROCESSAR UM LST EM BLOCOS ---------------------------------------
process_lst_block <- function(lst_rast, cond_name, output_file, nlines_block = 100) {
  
  message("=== Iniciando processamento: ", cond_name, " ===")
  # Mensagem para acompanhar o andamento
  
  # Criar um CSV vazio apenas com cabeçalho
  write.csv(
    data.frame(agb=NA, delta_lst=NA, sf_perc=NA, cond=NA)[0,], # dataframe vazio
    output_file, 
    row.names=FALSE
  )
  
  nrows <- nrow(lst_rast)  # Número total de linhas do raster
  
  # Criar raster "primário" para cálculo do delta LST
  lst_pri <- ifel(is.na(fr_pri), NA, lst_rast)
  # Onde fr_pri é NA → LST também vira NA
  
  lst_f <- focal(lst_pri, w=21, fun=median, na.rm=TRUE, na.policy="only")
  # Aplica filtro focal (janela 21x21) computando a mediana
  # Isso gera o LST filtrado
  
  lst_delta <- lst_rast - lst_f  
  # Delta LST = pixel - mediana da vizinhança
  
  esa2 <- ifel(is.na(lst_delta), NA, esa)
  # Remove valores AGB onde o delta_lst é NA
  
  # Loop para processar o raster em blocos (reduz consumo de RAM)
  for(start_row in seq(1, nrows, by=nlines_block)) {
    
    end_row <- min(start_row + nlines_block - 1, nrows)
    # Linha final do bloco atual
    
    # Puxa valores somente do bloco (evita carregar o raster inteiro)
    r_block  <- values(lst_rast, row=start_row, nrows=end_row-start_row+1)
    f_block  <- values(lst_f, row=start_row, nrows=end_row-start_row+1)
    esa_block <- values(esa2, row=start_row, nrows=end_row-start_row+1)
    sf_block  <- values(sf_perc, row=start_row, nrows=end_row-start_row+1)
    
    # Junta tudo em um data.frame
    df_block <- data.frame(
      agb = esa_block,
      delta_lst = r_block - f_block,
      sf_perc = sf_block,
      cond = cond_name
    )
    
    df_block <- na.omit(df_block)
    # Remove linhas com NA
    
    # Salva adicionando ao CSV
    write.table(
      df_block, output_file,
      sep=",", row.names=FALSE,
      col.names=FALSE, append=TRUE
    )
    
    message("Bloco de linhas ", start_row, " a ", end_row, " concluído")
  }
  
  message("=== Condição ", cond_name, " concluída ===")
}


# RODAR TODAS AS CONDIÇÕES -----------------------------------------------------

process_lst_block(lst_year, "Annual", "LST_AGB_Annual_full.csv")
# Processa o raster anual

process_lst_block(lst_dry,  "Dry Season", "LST_AGB_Dry_full.csv")
# Processa o raster da estação seca

process_lst_block(lst_wet,  "Rainy Season", "LST_AGB_Rainy_full.csv")
# Processa o raster da estação chuvosa

