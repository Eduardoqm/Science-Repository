
library(raster)
library(rasterVis)
library(viridis)

setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Micasense Project/Teste de captura/Voo Darro-captura livre/000')

dir()

mica1 = raster("IMG_0004_1.tif") #Blue
mica2 = raster("IMG_0004_2.tif") #Green
mica3 = raster("IMG_0004_3.tif") #Red
mica4 = raster("IMG_0004_4.tif") #NIR
mica5 = raster("IMG_0004_5.tif") #Red Edge
mica6 = raster("IMG_0004_6.tif") #Thermal

mult_mica = stack(mica1, mica2, mica3, mica4, mica5)
levelplot(mult_mica, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = 'Bandas espectrais - blue, green, red, NIR e red edge')

levelplot(mica6, margin = FALSE, col.regions=heat.colors(100), main = 'Banda Termal')

ndvi = (mica4 - mica3)/(mica4 + mica3)
levelplot(ndvi, margin = FALSE, par.settings=rasterTheme(viridis_pal(option = "D")(255)), main = "NDVI Darro")

ndwi = (mica2 - mica4)/(mica2 + mica4)
levelplot(ndwi, par.settings = RdBuTheme, margin = FALSE, main = 'NDWI Darro')

#Getting location info
mica4 = raster("IMG_0004_4.tif")
mica7 = raster("IMG_0002_4.tif")

#Put CRS



crs(mica4) = CRS('+proj=longlat') 
crs(mica7) = CRS('+proj=longlat')

micasaic = mosaic(mica4, mica7, fun = mean)
plot(micasaic)






#DSM ====================================================================
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Micasense Project/Teste de captura/Voo Darro-captura overlap/000/Mica test/4_index/project_data/dsm_gsd/tiles')

dir()

dsm1 = raster("Mica test_dsm_1_1.tif")
dsm2 = raster("Mica test_dsm_1_2.tif")
dsm3 = raster("Mica test_dsm_1_3.tif")
dsm4 = raster("Mica test_dsm_1_4.tif")
dsm5 = raster("Mica test_dsm_2_1.tif")
dsm6 = raster("Mica test_dsm_2_2.tif")
dsm7 = raster("Mica test_dsm_2_3.tif")
dsm8 = raster("Mica test_dsm_2_4.tif")
dsm9 = raster("Mica test_dsm_3_1.tif")
dsm10 = raster("Mica test_dsm_3_2.tif")
dsm11 = raster("Mica test_dsm_3_3.tif")
dsm12 = raster("Mica test_dsm_3_4.tif")

dsm = mosaic(dsm1, dsm2, dsm3, dsm4, dsm5, dsm6, dsm7, dsm8, dsm9, dsm10, dsm11, dsm12, fun = mean)
plot(dsm)

levelplot(dsm12, margin = FALSE)


#Report Test ========================================================
library(raster)
library(rasterVis)
library(rgdal)
library(rgeos)
## Error in library(rgeos): there is no package called 'rgeos'
library(RColorBrewer)
# turn off factors
options(stringsAsFactors = FALSE)

root = 'C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\Doutorado\\Micasense Project\\Teste de captura\\Overlap_IR'


files = list.files(paste0(root,'\\'),'*.tif')

j = 1

for(j in 1:length(files)){
  
  naip_multispectral_st <- stack(paste0(root,'\\',files[j]))
  
  # convert data into rasterbrick for faster processing
  naip_multispectral_br <- brick(naip_multispectral_st)
  
  
  naip <- naip_multispectral_br
  name =gsub('.tif','',files[j])
  # import the naip pre-fire data
  pdf(paste0(root,'\\relatorio',name,'.pdf'),width=12,height=8,paper='special') 
  #levelplot(naip_multispectral_br,main=paste0(name,' raw'))
  
  plot(naip,main=paste0(name,' NIR'))
  hist(naip,main=paste0(name,' NIR'))
  
  cat(j,':',length(files),'\n')
  dev.off()
}
