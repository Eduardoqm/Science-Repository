#Graficos ggplot 3D     
#Eduardo Q Marques      

#OBS: A funcao plot_gg não está funcionando. Baixei o script da funcao e sempre rodo ela antes de executar.
# Os modelos 3D estão em diferentes "windowsize" (Tamanho de janela)
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(ggplot2)
library(reshape2)
library(raster)
library(rgdal)
library(viridis)

#Aqui tem que tranformar o SRTM em pontos e depois pegar seu dataframe para plotar
srtm3  <-  rasterToPoints(alt_fire)
srtm4 <-  data.frame(srtm3)
colnames(srtm4) = c("lon", "lat", "alt")

#Fazendo o mapa (ggplot)
gg_nc = ggplot(srtm4, aes(lon,lat)) +
  geom_raster(aes(fill = alt)) +
  scale_fill_gradientn("Chama (cm)", colours = viridis(10)) +
  theme_minimal()+
  labs(title = "Experimento de fogo (2007)", x = "", y = "")

#Plot 3d do grafico
plot_gg(gg_nc, multicore = TRUE, raytrace = TRUE, scale = 50, windowsize = c(900, 600), zoom = 0.6, width = 5.5, height=3.3, phi = 30, theta = 30)


render_movie(gg_nc)
render_depth(focallength=100,focus=0.75)
