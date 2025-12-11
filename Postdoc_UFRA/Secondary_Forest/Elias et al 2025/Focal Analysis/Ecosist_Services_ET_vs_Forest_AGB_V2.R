#ET Forest by ESA Biomass (Focal)
#E.Q.Marques and J.Brito 11-12-2025

library(terra)
library(sf)
library(tidyverse)

#General Configurations --------------------------------------------------------
setwd("C:/Users/Cliente/Downloads/dados")  

#Creates a temporary Terra files folder (prevents RAM overflow)
if (!dir.exists("C:/Users/Cliente/Downloads/dados/tmp_terra")) {
  dir.create("C:/Users/Cliente/Downloads/dados/tmp_terra")}

terraOptions(
  #memfrac = 0.5,  #Delimitate terra do use only 50% of RAM
  tempdir = "C:/Users/Cliente/Downloads/dados/tmp_terra",
  threads = max(1, parallel::detectCores() - 2)) # Number of threads

#Load data ---------------------------------------------------------------------
et_year <- rast("ECOSTRESS_ET_Annual_2022_70m.tif")
et_dry  <- rast("ECOSTRESS_ET_Dry_2022_70m.tif")
et_wet  <- rast("ECOSTRESS_ET_Wet_2022_70m.tif")

fr_pri <- rast("Forest_70m.tif")
esa <- rast("ESA_Biomass_70m.tif")
sf_perc <- rast("Perc_SecForest_70m.tif")
sf_perc <- ifel(sf_perc == 0, NA, sf_perc)

#Function to save Delta Raster -------------------------------------------------
save_delta_raster <- function(et_rast, output_file) {
  
  message("Generating Delta Raster: ", output_file)
  
  et_pri <- ifel(is.na(fr_pri), NA, et_rast)
  et_f   <- focal(et_pri, w=21, fun=median, na.rm=TRUE, na.policy="only")
  delta_et <- et_rast - et_f
  
  writeRaster(delta_et, output_file, overwrite=TRUE)
  
  message("Save Raster: ", output_file)
}

#Function save dataframe by blocks ---------------------------------------------
process_et_block <- function(et_rast, Delta_rast, cond_name, output_file, nlines_block = 100) {
  
  message("=== Starting Process: ", cond_name, " ===")
  
  #Create a empity CSV
  write.csv(
    data.frame(agb=NA, delta_et=NA, sf_perc=NA, cond=NA)[0,],
    output_file,
    row.names=FALSE
  )
  
  nrows <- nrow(et_rast) #Lines count
  esa2 <- ifel(is.na(et_rast), NA, esa)#AGB mask
  
  #Loop in blocks
  for(start_row in seq(1, nrows, by=nlines_block)) {
    
    end_row <- min(start_row + nlines_block - 1, nrows)
    
    r_block <- as.vector(values(et_rast, row=start_row, nrows=end_row-start_row+1))
    delta_block <- as.vector(values(Delta_rast, row=start_row, nrows=end_row-start_row+1))
    agb_block <- as.vector(values(esa2, row=start_row, nrows=end_row-start_row+1))
    sf_block <- as.vector(values(sf_perc, row=start_row, nrows=end_row-start_row+1))
    
    df_block <- data.frame(
      agb = agb_block,
      delta_et = delta_block,
      sf_perc = sf_block,
      cond = cond_name
    )
    
    df_block <- na.omit(df_block)
    
    write.table(
      df_block, output_file,
      sep=",", row.names=FALSE,
      col.names=FALSE, append=TRUE
    )
    
    message("Lines Block", start_row, " a ", end_row, " conclude")
  }
  
  message("=== Condition ", cond_name, " conclude ===")
}

#Executing  functions ----------------------------------------------------------
start.time <- Sys.time()

#Focal
save_delta_raster(et_year, "Delta_et_Forest_AGB_Annual_2022.tif")
save_delta_raster(et_dry,  "Delta_et_Forest_AGB_Dry_2022.tif")
save_delta_raster(et_wet,  "Delta_et_Forest_AGB_Rainy_2022.tif")

Delta_year  <- rast("Delta_et_Forest_AGB_Annual_2022.tif")  
Delta_dry  <- rast("Delta_et_Forest_AGB_Dry_2022.tif")  
Delta_wet  <- rast("Delta_et_Forest_AGB_Rainy_2022.tif")  

#Dataframe
process_et_block(et_year, Delta_year, "Annual", "et_AGB_Annual_full.csv")
process_et_block(et_dry, Delta_dry,  "Dry Season", "et_AGB_Dry_full.csv")
process_et_block(et_wet,Delta_wet,  "Rainy Season", "et_AGB_Rainy_full.csv")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Summarizing data --------------------------------------------------------------
year_df <- read_csv("et_AGB_Annual_full.csv")
dry_df <- read_csv("et_AGB_Dry_full.csv")
rainy_df <- read_csv("et_AGB_Rainy_full.csv")

final_df <- rbind(year_df, dry_df, rainy_df)

final_df2 <- final_df %>%
  filter(sf_perc >= 70) %>%
  mutate(agb = round(agb, 0)) %>%
  group_by(agb, cond) %>%
  summarise(
    delta_et = mean(delta_et, na.rm = TRUE),
    sf_perc   = mean(sf_perc, na.rm = TRUE),
    n = n()) %>%
  filter(n > 200, agb < 250)

final_df2$year <- 2022

write.csv(final_df2, "et_Forest_AGB_2022.csv", row.names = FALSE)

