# Script otimizado para processar LST e AGB com terra (chunked, disco-first)
# Eduardo Q Marques - versão otimizada 2025
# Ajuste paths conforme seu sistema

library(terra)
library(dplyr)
library(data.table)   # fast fwrite
# opcional (recomendo): arrow para parquet (mais eficiente)
# library(arrow)

# -------------------- Configurações terra / disco -----------------------
terraOptions(tempdir = "C:/TEMP_RASTERS",      # crie esta pasta
             memfrac = 0.6,                    # usa até 60% da RAM disponível
             progress = 1)

dir.create("C:/TEMP_RASTERS", showWarnings = FALSE)
# ajuste diretórios conforme sua estrutura
setwd("C:/Users/Public/Documents/Analises_Elias/Dados/LST")

# -------------------- Carregar rasters (apenas referencias, sem plot) ----------------
# mudar paths conforme seu layout
lst_year = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/LST_Landsat_Annual_2022_70m.tif")
lst_dry  = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/LST_Landsat_Dry_2022_70m.tif")
lst_wet  = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/LST_Landsat_Wet_2022_70m.tif")

fr_pri   = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/Forest_70m.tif")
esa      = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/ESA_Biomass_70m.tif")
sf_perc  = rast("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m/Perc_SecForest_70m.tif")

# Verificações rápidas
print(lst_year)
print(esa)
# NÃO fazer plot de rasters gigantes — comentado

# -------------------- Função utilitária: calc focal gravando no disco ----------------
# Usa median com janela w e grava resultado em arquivo TIFF (gera menos RAM)
focal_to_disk <- function(input_rast, w = 21, out_tif, overwrite = TRUE) {
  message("Calculando focal (median) e gravando: ", out_tif)
  # 'focal' do terra aceita filename para gravar no disco
  out <- focal(input_rast,
               w = w,
               fun = median,
               filename = out_tif,
               overwrite = overwrite,
               na.rm = TRUE,
               na.policy = "only")
  return(rast(out_tif))
}

# -------------------- Pipeline para uma condição (Annual / Dry / Rainy) ----------------
process_condition <- function(lst_r, cond_name, out_csv_path) {
  message("==> Iniciando condição: ", cond_name)
  t0 <- Sys.time()
  
  # 1) criar raster onde só há forest primary (mantém valores de lst apenas em primary forest)
  # Isso evita gerar objetos gigantes na memória; cria arquivo temporário no disco
  lst_pri_file <- paste0("lst_pri_", cond_name, ".tif")
  message("Gerando lst_pri (aplicando mask primary forest) ...")
  lst_pri <- ifel(is.na(fr_pri), NA, lst_r)
  writeRaster(lst_pri, filename = lst_pri_file, overwrite = TRUE)
  rm(lst_pri); gc()
  
  # 2) calcular focal median com janela w=21 e gravar no disco
  lst_f_file <- paste0("lst_f_w21_", cond_name, ".tif")
  lst_f <- focal_to_disk(rast(lst_pri_file), w = 21, out_tif = lst_f_file, overwrite = TRUE)
  rm(lst_pri); gc()
  
  # 3) delta = lst_r - lst_f  (gravar direto no disco)
  lst_delta_file <- paste0("lst_delta_pri_", cond_name, ".tif")
  message("Calculando delta (lst - focal) e gravando: ", lst_delta_file)
  delta_r <- lst_r - lst_f
  writeRaster(delta_r, filename = lst_delta_file, overwrite = TRUE)
  rm(lst_f); gc()
  
  # 4) construir um raster "esa2" que seja NA quando delta for NA (implementado por bloco abaixo)
  # Em vez de criar outro raster na memória, vamos ler os três rasters por blocos e escrever CSV chunked.
  
  # Estimativa e checagem de espaço: (opcional)
  n_cells <- ncell(delta_r)
  message("Número total de células: ", format(n_cells, big.mark = ","))
  # ATENÇÃO: CSV final pode ser muito grande (~38+ GB com seus tamanhos). Garanta espaço em disco.
  
  # 5) chunked read & write -> ler blocos de linhas e gravar apenas valores válidos
  # Usamos terra::readStart/readStop e readValues para blocos por nrows
  # OBS: usa data.table::fwrite para velocidade
  con_out <- out_csv_path
  # cabeçalho
  fwrite(data.frame(agb = numeric(0), delta_lst = numeric(0), sf_perc = numeric(0), cond = character(0)),
         file = con_out, sep = ",", col.names = TRUE)
  
  # Ler por blocos de linhas (ajustável: nrows_block)
  # Recomendo nrows_block ~ 500-5000 dependendo do seu I/O e RAM. Aqui uso 2000 como default.
  nrows_block <- 2000
  nrows_total <- nrow(delta_r)
  nblocks <- ceiling(nrows_total / nrows_block)
  message("Lendo por blocos: ", nblocks, " blocos (nrows_block = ", nrows_block, ")")
  
  # abrir leitura por cada raster
  rd_delta <- rast(lst_delta_file)
  rd_esa   <- esa
  rd_sf    <- sf_perc
  
  for (i in seq_len(nblocks)) {
    row_start <- (i - 1) * nrows_block + 1
    nrows <- ifelse(i < nblocks, nrows_block, nrows_total - (i - 1) * nrows_block)
    message("Bloco ", i, "/", nblocks, " — linhas ", row_start, " (nrows=", nrows, ")")
    
    # readValues em terra: readValues(rast, row=..., nrows=...)
    vals_delta <- readValues(rd_delta, row = row_start, nrows = nrows)
    vals_esa   <- readValues(rd_esa,   row = row_start, nrows = nrows)
    vals_sf    <- readValues(rd_sf,    row = row_start, nrows = nrows)
    
    # combinar em data.frame
    df <- data.frame(
      agb = vals_esa[,1],
      delta_lst = vals_delta[,1],
      sf_perc = vals_sf[,1]
    )
    
    # remover NA (somente linhas com valores completos)
    df <- df[complete.cases(df), , drop = FALSE]
    if (nrow(df) > 0) {
      df$cond <- cond_name
      # escrever append
      fwrite(df, file = con_out, sep = ",", col.names = FALSE, append = TRUE)
    }
    
    # liberar memória temporária
    rm(vals_delta, vals_esa, vals_sf, df)
    gc()
  }
  
  # limpeza
  rm(rd_delta, rd_esa, rd_sf)
  gc()
  
  t1 <- Sys.time()
  message("Condição ", cond_name, " finalizada. Tempo: ", round(difftime(t1, t0, units = "mins"), 2), " minutos")
}

# -------------------- Executar para cada condição ----------------
start.time <- Sys.time()

process_condition(lst_year, "Annual", "LST_AGB_Annual_full_2022.csv")
gc()
process_condition(lst_dry, "Dry Season", "LST_AGB_Dry_full_2022.csv")
gc()
process_condition(lst_wet, "Rainy Season", "LST_AGB_Rainy_full_2022.csv")
gc()

end.time <- Sys.time()
message("Tempo total: ", round(difftime(end.time, start.time, units = "mins"), 2), " minutos")
