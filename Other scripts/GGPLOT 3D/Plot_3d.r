#########################
#Graficos ggplot 3D     #
#                       #
#Eduardo Q Marques      #
#                       #
#01/07/2019             #
#########################

#OBS: A funcao plot_gg não está funcionando. Baixei o script da funcao e sempre rodo ela antes de executar.
# Os modelos 3D estão em diferentes "windowsize" (Tamanho de janela)

#Pacotes---------------------------------------------------------------------------------------------------

#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(ggplot2)
library(reshape2)
library(raster)
library(rgdal)

#Exemplo grafico de pontos----------------------------------------------------------------------------------
mtcars_gg = ggplot(mtcars) + 
  geom_point(aes(x=mpg,color=cyl,y=disp),size=2) +
  scale_color_continuous(limits=c(0,8)) +
  ggtitle("mtcars: Displacement vs mpg vs # of cylinders") +
  theme(title = element_text(size=8),
        text = element_text(size=12)) 

plot_gg(mtcars_gg, height=3, width=3.5, multicore=TRUE, pointcontract = 0.7, soliddepth=-200)


#Exemplo mapa de altitude------------------------------------------------------------------------------------
ggvolcano = volcano %>% 
  melt() %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  #geom_contour(aes(x = Var1, y = Var2, z = value)) +
  scale_x_continuous("X", expand = c(0, 0)) +
  scale_y_continuous("Y", expand = c(0, 0)) +
  scale_fill_gradientn("Z", colours = terrain.colors(10)) +
  coord_fixed()

par(mfrow = c(1, 2))
plot_gg(ggvolcano, width = 7, height = 4, raytrace = FALSE, preview = TRUE)

plot_gg(ggvolcano, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)


#Transformando um arquivo SRTM em um grafico 3d---------------------------------------------------------------
srtm=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/EQMapas/shapes/SRTM_GO/SD-22-X-D.tif")

#Aqui tem que tranformar o SRTM em pontos e depois pegar seu dataframe para plotar
srtm2  <-  rasterToPoints(srtm)
srtm3 <-  data.frame(srtm2)
colnames(srtm3) = c("lon", "lat", "alt")

#Transformando em um grafico (ggplot)
ggsrtm <- ggplot(srtm3, aes(lon,lat)) +
  geom_raster(aes(fill = alt)) +
  scale_fill_gradientn("alt", colours = terrain.colors(10))

#Mostrando com vai ficar
plot_gg(ggsrtm, width = 7, height = 4, raytrace = FALSE, preview = TRUE)

#Plot 3d do grafico
plot_gg(ggsrtm, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 50, windowsize = c(800, 400), zoom = 0.5, phi = 30, theta = 30)


#Mapa 3D parque do Bacaba---------------------------------------------------------------------------------------
#Carregar srtm que compreende o parque do bacaba
srtm=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/EQMapas/shapes/SRTM_BCB/SRTMGL1_003.elevation.tif")

#Carregar shape de limites do parque do bacaba
bcb=readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/EQMapas/shapes",layer="limite_reserva_total")

#Mudar projeção para os dados ficarem compativeis
proj=projection(srtm)
bcb_sr=spTransform(bcb,proj) #transforma o sistema de projeção do shp no mesmo do raster

#Extrair dados para o bacaba
srtm2=mask(srtm,bcb_sr)

#Aqui tem que tranformar o SRTM em pontos e depois pegar seu dataframe para plotar
srtm3  <-  rasterToPoints(srtm2)
srtm4 <-  data.frame(srtm3)
colnames(srtm4) = c("lon", "lat", "alt")

#Fazendo o mapa (ggplot)
ggsrtm <- ggplot(srtm4, aes(lon,lat)) +
  geom_raster(aes(fill = alt)) +
  scale_fill_gradientn("Altitude (m)", colours = terrain.colors(10)) +
  labs(title = "Parque do Bacaba", x = "", y = "") +
  scale_x_continuous(breaks = c(-52.341, -52.35250, -52.364))

#Mostrando com vai ficar
plot_gg(ggsrtm, preview = TRUE)

#Plot 3d do grafico
plot_gg(ggsrtm, multicore = TRUE, raytrace = TRUE, 
        scale = 50, windowsize = c(900, 600), zoom = 0.6, phi = 30, theta = 30)
