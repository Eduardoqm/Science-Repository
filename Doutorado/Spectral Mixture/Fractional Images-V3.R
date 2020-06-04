#Fractional Images V3

#Eduardo Q Marques 03-06-2020

library(raster)
library(rasterVis)
library(rgdal)
library(rgeos)
library(sf)
library(ggplot2)
library(ggridges)
library(ggmap)
library(ggsn)
library(viridis)
library(tidyverse)
library(reshape2)

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
area1 = spTransform(area1, crs(frac))
frac_tang = crop(frac, area1)

#Bare Substrate
bs = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[1]]
  bs[[x]] = crop(y, area1)
}
names(bs) = files

#Photosynthetic Vegetation
pv = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[2]]
  pv[[x]] = crop(y, area1)
}
names(pv) = files

#Non-Photosynthetic Vegetation
npv = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[3]]
  npv[[x]] = crop(y, area1)
}
names(npv) = files


#Area1 maps ================================================================================
#Plot data
levelplot(bs, margin = FALSE, col.regions = viridis(100), main = "Bare Substrate")

levelplot(pv, margin = FALSE, col.regions = viridis(100), main = "Photosynthetic Vegetation")

levelplot(npv, margin = FALSE, col.regions = viridis(100), main = "Non-Photosynthetic Vegetation")


#Area-1 GGPLOT Maps (2018-2019) ============================================================
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Shapes/Landsat')

limit = read_sf('Polygon_A_B_C.shp')
limit = st_transform(limit, crs(npv)) #Use st_transform becouse using read_sf

#Convert image in Data Frame
storm_bs = stack(bs[[9]], bs[[15]])
storm_npv = stack(npv[[9]], npv[[15]])
storm_pv = stack(pv[[9]], pv[[15]])

#BS
storm = as.data.frame(storm_bs, xy =TRUE)
colnames(storm) = c('x','y','a2018','a2019')
storm$diff = storm$a2019 - storm$a2018 #Calc difference
bs_df = melt(data = storm,  measure.vars = c("a2018", "a2019"))
colnames(bs_df) = c('x','y','diff','year','value')

#NPV
storm = as.data.frame(storm_npv, xy =TRUE)
colnames(storm) = c('x','y','a2018','a2019')
storm$diff = storm$a2019 - storm$a2018 #Calc difference
npv_df = melt(data = storm,  measure.vars = c("a2018", "a2019"))
colnames(npv_df) = c('x','y','diff', 'year','value')

#PV
storm = as.data.frame(storm_pv, xy =TRUE)
colnames(storm) = c('x','y','a2018','a2019')
storm$diff = storm$a2019 - storm$a2018 #Calc difference
pv_df = melt(data = storm,  measure.vars = c("a2018", "a2019"))
colnames(pv_df) = c('x','y','diff','year','value')


#Before and after storm
#eqm = c('darkred','red','yellow','green','darkgreen')
#eqm = c('darkred','red','orange','yellow','green','darkgreen')
eqm = c('darkgreen','yellow','red','darkred')
ggplot(bs_df) +
  geom_raster(data = bs_df, aes(x, y, fill = value))+
  scale_fill_gradientn("value", colours = eqm)+
  geom_sf(data = limit, fill = NA, col = "black", size = 1)+
  facet_grid(rows = vars(year))+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  ggtitle("Before and after blowdown (Bare Substrate after 1 year)")+
  north(limit, symbol = 12)


eqm = c('yellow','red','darkred')
ggplot(npv_df) +
  geom_raster(data = npv_df, aes(x, y, fill = value))+
  scale_fill_gradientn("value", colours = eqm)+
  geom_sf(data = limit, fill = NA, col = "black", size = 1)+
  facet_grid(rows = vars(year))+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  ggtitle("Before and after blowdown (NPV after 1 year)")+
  north(limit, symbol = 12)

eqm = c('white','yellow','green','darkgreen')
ggplot(pv_df) +
  geom_raster(data = pv_df, aes(x, y, fill = value))+
  scale_fill_gradientn("value", colours = eqm)+
  geom_sf(data = limit, fill = NA, col = "black", size = 1)+
  facet_grid(rows = vars(year))+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  ggtitle("Before and after blowdown (PV after 1 year)")+
  north(limit, symbol = 12)



#Diference across before and after
bs_df$kind = c("bs")
npv_df$kind = c("npv")
pv_df$kind = c("pv")
dif = rbind(bs_df, npv_df, pv_df)


eqm = c('darkred','red','orange','white','blue','darkblue')
ggplot(dif) +
  geom_raster(data = dif, aes(x, y, fill = diff))+
  scale_fill_gradientn("diff", colours = eqm)+
  geom_sf(data = limit, fill = NA, col = "black", size = 1)+
  facet_grid(~kind)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  ggtitle("Difference across 2018-2019")+
  north(limit, symbol = 12)




