#Fractional Images V2

#Eduardo Q Marques 20-05-2020

library(raster)
library(rasterVis)
library(rgdal)
library(viridis)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggridges)

#Overview ====================================================================================
#Load data
#Spectral Mixture
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Fraction_Landsat/Mistura Espectral')
dir()

files1 <- list.files(pattern="_frac$") 
files2 <- list.files(pattern = "_frac.tif$")
files = c(files1, files2)

frac = brick(files[[1]]) #To get the crs

#Vectors
area1 = readOGR(dsn = 'C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Shapes/Landsat', layer = 'Polygon_A_B_C')

tang = readOGR(dsn = 'C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Shapes/Tanguro farm', layer = 'Tanguro_limites')

#Make Bare Substrate, PV and NPV stacks ======================================================
#Crop Tanguro limits
tang = spTransform(tang, crs(frac))
frac_tang = crop(frac, tang)

#Bare Substrate
bs = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[1]]
  bs[[x]] = crop(y, tang)
}
names(bs) = files

#Photosynthetic Vegetation
pv = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[2]]
  pv[[x]] = crop(y, tang)
}
names(pv) = files

#Non-Photosynthetic Vegetation
npv = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[3]]
  npv[[x]] = crop(y, tang)
}
names(npv) = files


#Tanguro maps ================================================================================
#Plot data
bs = mask(bs, tang)
#levelplot(bs, margin = FALSE, col.regions = viridis(100), main = "Bare Substrate")

pv = mask(pv, tang)
#levelplot(pv, margin = FALSE, col.regions = viridis(100), main = "Photosynthetic Vegetation")

npv = mask(npv, tang)
#levelplot(npv, margin = FALSE, col.regions = viridis(100), main = "Non-Photosynthetic Vegetation")

#Area-1 maps =================================================================================
#Crop for the stude area
area1 = spTransform(area1, crs(frac))
#bs_area1 = crop(bs, area1)
#pv_area1 = crop(pv, area1)
#npv_area1 = crop(npv, area1)

#Plot
#levelplot(bs_area1, margin = FALSE, col.regions = viridis(100), main = "Bare Substrate")

#levelplot(pv_area1, margin = FALSE, col.regions = viridis(100), main = "Photosynthetic Vegetation")

#levelplot(npv_area1, margin = FALSE, col.regions = viridis(100), main = "Non-Photosynthetic Vegetation")

#npv2 = stack(npv_area1[[10]], npv_area1[[3]], npv_area1[[9]])
#names(npv2) = c("2000","2014","2018")
#levelplot(npv2, margin = FALSE, col.regions = viridis(100), main = "Non-Photosynthetic Vegetation")

#Crop by plot
bs_control = crop(bs, area1[1,]); bs_b3yr = crop(bs, area1[2,]); bs_b1yr = crop(bs, area1[3,])

pv_control = crop(pv, area1[1,]); pv_b3yr = crop(pv, area1[2,]); pv_b1yr = crop(pv, area1[3,])

npv_control = crop(npv, area1[1,]); npv_b3yr = crop(npv, area1[2,]); npv_b1yr = crop(npv, area1[3,])

#Extract data and analyse by plot ============================================================
#Bare Substrate
a = bs_control %>%
  as.data.frame()%>%
  melt()
colnames(a) = c("id","Bare Substrate_Control")

b = bs_b3yr %>%
  as.data.frame()%>%
  melt()
colnames(b) = c("id","Bare Substrate_B3yr")

c = bs_b1yr %>%
  as.data.frame()%>%
  melt()
colnames(c) = c("id","Bare Substrate_B1yr")

bs_df = cbind(a,b)
bs_df = bs_df[,c(-3)]
bs_df = full_join(bs_df,c, by = "id")

#Photosynthetic Vegetation
a = pv_control %>%
  as.data.frame()%>%
  melt()
colnames(a) = c("id","Photosynthetic Vegetation_Control")

b = pv_b3yr %>%
  as.data.frame()%>%
  melt()
colnames(b) = c("id","Photosynthetic Vegetation_B3yr")

c = pv_b1yr %>%
  as.data.frame()%>%
  melt()
colnames(c) = c("id","Photosynthetic Vegetation_B1yr")

pv_df = cbind(a,b)
pv_df = pv_df[,c(-3)]
pv_df = full_join(pv_df,c, by = "id")

#Non-Photosynthetic Vegetation
a = npv_control %>%
  as.data.frame()%>%
  melt()
colnames(a) = c("id","Non-Photosynthetic Vegetation_Control")

b = npv_b3yr %>%
  as.data.frame()%>%
  melt()
colnames(b) = c("id","Non-Photosynthetic Vegetation_B3yr")

c = npv_b1yr %>%
  as.data.frame()%>%
  melt()
colnames(c) = c("id","Non-Photosynthetic Vegetation_B1yr")

npv_df = cbind(a,b)
npv_df = npv_df[,c(-3)]
npv_df = full_join(npv_df,c, by = "id")

#Join data
area1_df = cbind(bs_df, pv_df, npv_df)
area1_df = area1_df[,c(-5, -9)]

#Make a usual dataframe
df = melt(area1_df); colnames(df) = c("id","class","value")
df = df %>% separate(class, c("class", "plot"), sep = "_")#Separate class and plot
df$id = substr(df$id, 11, 14)#Staying only year

#write.csv(df, file = "sma.csv", sep = ",")

