#Graficos ggplot 3D     
#Eduardo Q Marques      

#OBS: A funcao plot_gg nÃ£o estÃ¡ funcionando. Baixei o script da funcao e sempre rodo ela antes de executar.
# Os modelos 3D estÃ£o em diferentes "windowsize" (Tamanho de janela)
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(ggplot2)
library(reshape2)
library(raster)
library(rgdal)
library(viridis)
library(rasterVis)

#Open raster
a = stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/EVI2", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
mapa = stack(a[[52]])
area1 = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Area1big")
area1 = spTransform(area1, crs(mapa))
mapa = crop(mapa, area1)



#Aqui tem que tranformar o SRTM em pontos e depois pegar seu dataframe para plotar
srtm3  <-  rasterToPoints(mapa)
srtm4 <-  data.frame(srtm3)
colnames(srtm4) = c("lon", "lat", "index")

#Fazendo o mapa (ggplot)
gg_nc = ggplot(srtm4, aes(lon,lat)) +
  geom_raster(aes(fill = index)) +
  scale_fill_gradientn("EVI2", colours = terrain.colors(10, rev = TRUE)) +
  theme_minimal()+
  labs(title = "Experimento de fogo (2019)", x = "", y = "")


#Plot 3d do grafico
plot_gg(gg_nc, multicore = TRUE, raytrace = TRUE, scale = 50, windowsize = c(900, 600), zoom = 0.6, width = 5.5, height=3.3, phi = 30, theta = 30)


render_movie(gg_nc)
render_depth(focallength=100,focus=0.75)
