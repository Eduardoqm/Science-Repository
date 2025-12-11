#LST Forest by Forest Age (Focal)
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
lst_year <- rast("LST_Landsat_Annual_2022_70m.tif")
lst_dry  <- rast("LST_Landsat_Dry_2022_70m.tif")
lst_wet  <- rast("LST_Landsat_Wet_2022_70m.tif")

fr_pri <- rast("Forest_70m.tif")
age <- rast("MB_Forest_age_70m.tif")
sf_perc <- rast("Perc_SecForest_70m.tif")
sf_perc <- ifel(sf_perc == 0, NA, sf_perc)

#Function to save Delta Raster -------------------------------------------------
save_delta_raster <- function(lst_rast, output_file) {
  
  message("Generating Delta Raster: ", output_file)
  
  lst_pri <- ifel(is.na(fr_pri), NA, lst_rast)
  lst_f   <- focal(lst_pri, w=21, fun=median, na.rm=TRUE, na.policy="only")
  delta_lst <- lst_rast - lst_f
  
  writeRaster(delta_lst, output_file, overwrite=TRUE)
  
  message("Save Raster: ", output_file)
}

#Function save dataframe by blocks ---------------------------------------------
process_lst_block <- function(lst_rast, Delta_rast, cond_name, output_file, nlines_block = 100) {
  
  message("=== Starting Process: ", cond_name, " ===")
  
  #Create a empity CSV
  write.csv(
    data.frame(age=NA, delta_lst=NA, sf_perc=NA, cond=NA)[0,],
    output_file,
    row.names=FALSE
  )
  
  nrows <- nrow(lst_rast) #Lines count
  age2 <- ifel(is.na(lst_rast), NA, age)#age mask
  
  #Loop in blocks
  for(start_row in seq(1, nrows, by=nlines_block)) {
    
    end_row <- min(start_row + nlines_block - 1, nrows)
    
    r_block <- as.vector(values(lst_rast, row=start_row, nrows=end_row-start_row+1))
    delta_block <- as.vector(values(Delta_rast, row=start_row, nrows=end_row-start_row+1))
    age_block <- as.vector(values(age2, row=start_row, nrows=end_row-start_row+1))
    sf_block <- as.vector(values(sf_perc, row=start_row, nrows=end_row-start_row+1))
    
    df_block <- data.frame(
      age = age_block,
      delta_lst = delta_block,
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
save_delta_raster(lst_year, "Delta_LST_Forest_age_Annual_2022.tif")
save_delta_raster(lst_dry,  "Delta_LST_Forest_age_Dry_2022.tif")
save_delta_raster(lst_wet,  "Delta_LST_Forest_age_Rainy_2022.tif")

Delta_year  <- rast("Delta_LST_Forest_age_Annual_2022.tif")  
Delta_dry  <- rast("Delta_LST_Forest_age_Dry_2022.tif")  
Delta_wet  <- rast("Delta_LST_Forest_age_Rainy_2022.tif")  

#Dataframe
process_lst_block(lst_year, Delta_year, "Annual", "LST_age_Annual_full.csv")
process_lst_block(lst_dry, Delta_dry,  "Dry Season", "LST_age_Dry_full.csv")
process_lst_block(lst_wet,Delta_wet,  "Rainy Season", "LST_age_Rainy_full.csv")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Summarizing data --------------------------------------------------------------
year_df <- read_csv("LST_age_Annual_full.csv")
dry_df <- read_csv("LST_age_Dry_full.csv")
rainy_df <- read_csv("LST_age_Rainy_full.csv")

final_df <- rbind(year_df, dry_df, rainy_df)

final_df2 <- final_df %>%
  filter(sf_perc >= 70) %>%
  mutate(age = round(age, 0)) %>%
  group_by(age, cond) %>%
  summarise(
    delta_lst = mean(delta_lst, na.rm = TRUE),
    sf_perc   = mean(sf_perc, na.rm = TRUE),
    n = n()) %>%
  filter(n > 200, age < 250)

final_df2$year <- 2022

write.csv(final_df2, "LST_Forest_age_2022.csv", row.names = FALSE)

