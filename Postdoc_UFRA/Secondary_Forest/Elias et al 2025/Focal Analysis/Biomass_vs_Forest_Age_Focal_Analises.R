#ESA Biomass by Forest Age (Focal)
#E.Q.Marques 10-08-2026

library(terra)
library(sf)
library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("/home/leaf/Documentos/Serrapilheira _Elias et al/Scenery")  

esa <- rast("ESA_Biomass_70m.tif")

fr_pri <- rast("Forest_70m.tif")
age <- rast("MB_Forest_age_70m.tif")
sf_perc <- rast("Perc_SecForest_70m.tif")
sf_perc <- ifel(sf_perc == 0, NA, sf_perc)

#General Configurations --------------------------------------------------------
setwd("/home/leaf/Documentos/Serrapilheira _Elias et al/Biomass_Focal")  

#Creates a temporary Terra files folder (prevents RAM overflow)
if (!dir.exists("/home/leaf/Documentos/Serrapilheira _Elias et al/Biomass_Focal/tmp_terra")) {
  dir.create("/home/leaf/Documentos/Serrapilheira _Elias et al/Biomass_Focal/tmp_terra")}

terraOptions(
  #memfrac = 0.5,  #Delimitate terra do use only 50% of RAM
  tempdir = "/home/leaf/Documentos/Serrapilheira _Elias et al/Biomass_Focal/tmp_terra",
  threads = max(1, parallel::detectCores() - 2)) # Number of threads


#Function to save Delta Raster -------------------------------------------------
save_delta_raster <- function(esa_rast, output_file) {
  
  message("Generating Delta Raster: ", output_file)
  
  esa_pri <- ifel(is.na(fr_pri), NA, esa_rast)
  esa_f   <- focal(esa_pri, w=21, fun=median, na.rm=TRUE, na.policy="only")
  delta_esa <- esa_rast - esa_f
  
  writeRaster(delta_esa, output_file, overwrite=TRUE)
  
  message("Save Raster: ", output_file)
}

#Function save dataframe by blocks ---------------------------------------------
process_lst_block <- function(esa_rast, Delta_rast, cond_name, output_file, nlines_block = 100) {
  
  message("=== Starting Process: ", cond_name, " ===")
  
  #Create a empity CSV
  write.csv(
    data.frame(age=NA, delta_esa=NA, sf_perc=NA, cond=NA)[0,],
    output_file,
    row.names=FALSE
  )
  
  nrows <- nrow(esa_rast) #Lines count
  age2 <- ifel(is.na(esa_rast), NA, age)#age mask
  
  #Loop in blocks
  for(start_row in seq(1, nrows, by=nlines_block)) {
    
    end_row <- min(start_row + nlines_block - 1, nrows)
    
    r_block <- as.vector(values(esa_rast, row=start_row, nrows=end_row-start_row+1))
    delta_block <- as.vector(values(Delta_rast, row=start_row, nrows=end_row-start_row+1))
    age_block <- as.vector(values(age2, row=start_row, nrows=end_row-start_row+1))
    sf_block <- as.vector(values(sf_perc, row=start_row, nrows=end_row-start_row+1))
    
    df_block <- data.frame(
      age = age_block,
      delta_esa = delta_block,
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
save_delta_raster(esa, "Delta_AGB_Forest_age.tif")
delta_esa  <- rast("Delta_AGB_Forest_age.tif")

#Dataframe
process_lst_block(esa, delta_esa, "Annual", "AGB_age_full.csv")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Summarizing data --------------------------------------------------------------
df = read_csv("AGB_age_full.csv")

final_df = df %>%
  filter(sf_perc >= 70) %>%
  mutate(age = round(age, 0))
  
write.csv(final_df, "AGB_age_full_B.csv", row.names = FALSE)



ggplot(final_df, aes(x = age, y = delta_esa))+
  geom_point()+
  geom_smooth()




