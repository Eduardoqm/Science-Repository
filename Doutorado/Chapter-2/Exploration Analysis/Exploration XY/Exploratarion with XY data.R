#Exploratarion with XY data
#Blowdown Data (AREA-1)
#Eduardo Q Marques 10-06-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(viridis)
library(fmsb)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')

df = read.csv("blowdown_full_update_2021_XY.csv", sep = ",")


#Exploration ggplot ===========================================================================
#x11()
eqm = c("red","orange", "blue")
eqm2 = c("red", "blue")

ggplot(df, aes(x=x, y=y))+
  geom_point(aes(col = parcela_bl), size = 3, alpha = 0.5)+
  scale_color_manual(values = eqm)+
  ggtitle("Fall Trees Location")+
  theme_light()+
  coord_fixed()

ggplot(df, aes(x=x, y=y))+
  geom_point(aes(col = succ), size = 3, alpha = 0.5)+
  scale_color_manual(values = eqm2)+
  ggtitle("Fall Trees Succecional Condition")+
  theme_light()+
  coord_fixed()

ggplot(df, aes(x=x, y=y))+
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
  stat_density_2d(col = "black")+
  scale_fill_viridis_c()+
  ggtitle("Fall Trees Density")+
  theme_light()+
  coord_fixed()


#3D Map ===============================================================================
a = ggplot(df, aes(x=x, y=y))+
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
  stat_density_2d(col = "black")+
  scale_fill_viridis_c()+
  ggtitle("Fall Trees Density")+
  coord_fixed()



#library(rayshader)
#plot_gg(a, multicore = TRUE, raytrace = TRUE, width = 7, height = 7,
 #       scale = 300, windowsize = c(900, 600), zoom = 0.6, phi = 30, theta = 30)







library(leaflet)
library(raster)
library(sp)
library(rgdal)

df2 = df[,c(32, 31, 29)] %>% na.omit()
xy = df2[,c(1, 2)]

loc_trees = SpatialPointsDataFrame(coords = xy, data = df2,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

leaflet() %>% 
  #  addMiniMap(position = "bottomleft") %>%
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons(data = area1, color = "white", weight = 1, fillOpacity = 0) %>% 
  addCircles(data = loc_trees, color = "yellow", opacity = 0.9,
             highlightOptions = highlightOptions(color = "red", weight = 2,
                                                 bringToFront = TRUE),
             popup = loc_trees@data$species,
             label = loc_trees@data$species)
  
  

#Do interpolate raster from points
#bb_hire = as(extent(loc_trees), "SpatialPolygons")
bb_hire = as(extent(area1), "SpatialPolygons")
r = raster(bb_hire, ncol = 15, nrow = 10)
rc = rasterize(loc_trees@coords, r, fun = "count")
plot(rc, col = viridis(100))

rc2 <- replace(rc,is.na(rc),0)
plot(rc2, col = viridis(100))

#Resample the coarser raster to match finer grid
r.big <- raster(ncol=60, nrow=40, bb_hire)
rc3 <- resample(x=rc2, y=r.big, method="bilinear")

proj4string(rc3) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(rc3, col = viridis(100))
plot(loc_trees, add = T)

eqm <- colorNumeric(viridis(10), values(rc3),
                    na.color = "transparent")

leaflet() %>% 
  addMiniMap(position = "bottomleft") %>%
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons(data = area1, color = "white", weight = 1, fillOpacity = 0) %>% 
  addRasterImage(rc3, colors = eqm, opacity = 0.7) %>% 
  addLegend(pal = eqm, values = values(rc3),
            title = "N of damaged trees")




