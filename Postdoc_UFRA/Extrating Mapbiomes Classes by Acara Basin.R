#Extrating Mapbiomes Classes by Acara Basin

#Eduardo Q Marques 24-02-2026

library(terra)
library(sf)
library(tidyverse)

#Data
mb = rast("G:/My Drive/Research/Geodata/Rasters/MapBiomes_Brazil/Collection_9/mb2023.tif")
#bc = read_sf("G:/My Drive/Research/Propostas/Ativas/FINEP_2025/Bacias_Acara/Bacias_Level_12_Acara_Guama/Bacias_Level_12_Acara_Guama.shp")
bc = read_sf("G:/My Drive/Research/Propostas/Ativas/FINEP_2025/Bacias_Acara/Bacias_Level_12_Acara_Guama/bacias_finep_bruno.shp")

#plot(mb)
#plot(bc, add = T)

bc = bc %>% select(nome)

mb2 = mask(crop(mb, bc), bc)
plot(mb2)

#Dataframe to input classes
df = freq(mb2)

options(scipen = 999)
df$aha = (df$count*900)/10000 #Area in hectares
df$aha_total = sum(df$aha) #Total area in hectares
df$perc = (df$count/sum(df$count))*100 #Percentage of wich class
df$Bacia = "All"

#By Basin
#for (i in 1:731) {
for (i in 1:46) {
  #print(bc$HYBAS_ID[i])
  print(bc$nome[i])
  mbi = mask(crop(mb, bc[i,2]), bc[i,2])
  plot(mbi)
  
  df2 = freq(mbi)
  df2$aha = (df2$count*900)/10000
  df2$aha_total = sum(df2$aha)
  df2$perc = (df2$count/sum(df2$count))*100
  #df2$Bacia = bc$HYBAS_ID[i]
  df2$Bacia = bc$nome[i]
  
  df = rbind(df, df2)
}

#Input classes names
df = df[,-1]
colnames(df)[1] = c("class")
df = df %>% dplyr::filter(class != 0)

#Make wide to join the vector attributes
df3 <- df %>%
  select(Bacia, class, aha, perc, aha_total) %>%
  pivot_wider(id_cols = c(Bacia, aha_total),
              names_from = class,
              values_from = c(aha, perc),
              names_glue = "{class}_{.value}") %>%
  rename_with(~ str_replace_all(., " ", "_"))

#colnames(df3)[1] = "HYBAS_ID"
colnames(df3)[1] = "nome"
df3 = df3[-1,]

#df3$HYBAS_ID = as.double(df3$HYBAS_ID)
#bc = full_join(bc, df3, by = "HYBAS_ID") #Join to vector

bc = full_join(bc, df3, by = "nome") #Join to vector

#Saving vector
setwd("G:/My Drive/Research/Propostas/Ativas/FINEP_2025/Bacias_Acara/Bacias_Level_12_Acara_Guama")

#st_write(bc, "Bacias_Level_12_Acara_Guama_MapBiomas.shp")
st_write(bc, "bacias_finep_bruno_MapBiomas.shp")

