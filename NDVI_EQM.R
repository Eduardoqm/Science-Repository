library(raster)
library(rasterVis)
library(ggplot2)
library(rgdal)

#Carregar NDVIs
#2012
b1y12=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro sites/ndvi_B1yr_2012.tif")
b3y12=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro sites/ndvi_B3yr_2012.tif")
crt12=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro sites/ndvi_Control_2012.tif")

#2014
b1y14=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro sites/ndvi_B1yr_2014.tif")
b3y14=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro sites/ndvi_B3yr_2014.tif")
crt14=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro sites/ndvi_Control_2014.tif")

#Mosaico dos plots
ndvi12 <- mosaic(b1y12, b3y12, crt12, fun=mean)
ndvi14 <- mosaic(b1y14, b3y14, crt14, fun=mean)

ndvi <- stack(ndvi12, ndvi14)

levelplot(ndvi, names.attr=expression(2012, 2014))









#Limites area1
limit=readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

par(mfrow=c(2,1))
plot(ndvi12,  main="2012")
plot(limit, add=TRUE)
plot(ndvi14,  main="2014")
plot(limit, add=TRUE)


#


























#Mosaico dos plots
area1 <- mosaic(b1y12, b3y12, crt12, fun=mean)

#Limites area1
limit=readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

#Mudar projeção para os dados ficarem compativeis
proj=projection(area1)
limit_area1=spTransform(limit,proj) #transforma o sistema de projeção do shp no mesmo do raster


plot(area1)
plot_3d(area1)













#Aqui tem que tranformar o SRTM em pontos e depois pegar seu dataframe para plotar
area1b  <-  rasterToPoints(area1)
area1c <-  data.frame(area1b)
colnames(area1c) = c("lon", "lat", "value")

ggplot(area1c, aes(lon,lat)) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradientn("NDVI", colours = terrain.colors(10)) +
  labs(title = "NDVI (2012)", x = "", y = "")
















library(rayshader)

ggndvi <- ggplot(area1c, aes(lon,lat)) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradientn("NDVI", colours = terrain.colors(10)) +
  labs(title = "NDVI (2012)", x = "", y = "")


#Mostrando com vai ficar
plot_gg(ggndvi, preview = TRUE)

#Plot 3d do grafico
plot_gg(ggndvi, multicore = TRUE, raytrace = TRUE, 
        scale = 100, windowsize = c(900, 600), zoom = 0.6, phi = 30, theta = 30)
