library(raster)
library(rasterVis)
library(rgdal)
library(viridis)
library(ggplot2)
library(plotly)

img = brick(choose.files())

x11()
plot(img, col = viridis(100))
corte = drawPoly()

img2 = crop(img, corte)
plot(img2, col = viridis(100))


df = as.data.frame(img2, xy = T)
colnames(df) = c("lon", "lat", "alt")

df$lon = as.integer(df$lon)
df$lat = as.integer(df$lat)
df$alt = as.integer(df$alt)



plot_ly(x= df$lon, y = df$lat, z = df$alt, type="surface")


a = ggplot(df, aes(lon,lat)) +
  geom_raster(aes(fill = alt))

library(rayshader)
plot_gg(a, multicore = TRUE, raytrace = TRUE, 
        scale = 50, windowsize = c(900, 600), zoom = 0.6, phi = 30, theta = 30)
