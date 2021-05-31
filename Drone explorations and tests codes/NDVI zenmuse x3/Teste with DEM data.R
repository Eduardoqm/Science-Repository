
library(raster)
library(rasterVis)
library(rgdal)
library(viridis)
library(plotly)
library(plot3D)

#Load SRTM file--------------------------------------------------------------------------------
srtm = raster(choose.files())

srtm3  <-  rasterToPoints(srtm)
srtm4 <-  data.frame(srtm3)
colnames(srtm4) = c("lon", "lat", "alt")

plot_ly(x= srtm4$lon, y = srtm4$lat, z= srtm4$alt, type="surface")














# 3D SRTM ggplot MAP FROM RAYSHADER
#EduardoQM

#Packages---------------------------------------------------------------------------------------------------
library(rayshader)
library(ggplot2)
library(raster)
library(rgdal)

#Load SRTM file--------------------------------------------------------------------------------
srtm = raster(choose.files())

#Load vector of park limit
#bcb=readOGR("limite_reserva_total.shp")

#Here we will change the projection for the data to be compatible
#proj=projection(srtm)
#bcb_sr=spTransform(bcb,proj)

#Here we will mask with the vector
#srtm2=mask(srtm,bcb_sr)

#Now let's turn the SRTM into points and then get your dataframe to plot
srtm3  <-  rasterToPoints(srtm)
srtm4 <-  data.frame(srtm3)
colnames(srtm4) = c("lon", "lat", "alt")
#srtm5 = srtm4; colnames(srtm5) = c("X","Y","Z")
#srtm6 = rasterFromXYZ(srtm5)
#writeRaster(srtm6, "srtm_bcb.tif")

#Mapping in ggplot
ggsrtm <- ggplot(srtm4, aes(lon,lat)) +
  geom_raster(aes(fill = alt)) +
  scale_fill_gradientn("Altitude (m)", colours = viridis(100)) +
  labs(title = "DEM - Matrice 100", x = "", y = "")
  #scale_x_continuous(breaks = c(-52.341, -52.35250, -52.364))

#Now let's get the coolest part of the script ... Plot in 3D! First let's write a line to see how it will look...
plot_gg(ggsrtm, preview = TRUE)

#Now we can configure the size of the window we want and finally plot our map!
plot_gg(ggsrtm, multicore = TRUE, raytrace = TRUE, 
        scale = 50, windowsize = c(900, 600), zoom = 0.6, phi = 30, theta = 30)
