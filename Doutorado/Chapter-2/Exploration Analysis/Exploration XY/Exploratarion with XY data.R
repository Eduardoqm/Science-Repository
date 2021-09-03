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
  ggtitle("Fall Trees Successional Condition")+
  theme_light()+
  coord_fixed()

ggplot(df, aes(x=x, y=y))+
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
  stat_density_2d(col = "black")+
  scale_fill_viridis_c()+
  ggtitle("Fall Trees Density")+
  theme_light()+
  coord_fixed()

int = ggplot(df, aes(x=x, y=y))+
  geom_point(aes(col = parcela_bl), size = 3, alpha = 0.5)+
  scale_color_manual(values = eqm)+
  ggtitle("Fall Trees Location")+
  theme_light()+
  coord_fixed()

ggplotly(int)

#Succecional condition =========================================================================
succ = df[,c(7,14,30)]
succ$unit = as.numeric(1)
total = (sum(succ$unit)-1)

succ2 = succ %>%
  na.omit() %>% 
  group_by(succ) %>% 
  summarise(unit = sum(unit)) %>% 
  mutate(percent = (unit/total)*100)

ggplot(succ2, aes(x="", y=percent, fill=succ))+
  geom_bar(width = 1, stat = "identity", alpha = 0.8)+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(substr(succ2$percent, 1,2), "%")),
            vjust = -2, size = 5, col = "white")+
  xlab("")+ ylab("")+ labs(fill='Successional
  Condition')+
  ggtitle("Percentage of fallen trees")+
  scale_fill_manual(values = eqm2)+
  theme_minimal()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


succ3 = succ %>%
  na.omit() %>% 
  group_by(parcela_bl, succ) %>% 
  summarise(unit = sum(unit)) %>% 
  mutate(percent = (unit/total)*100)

ggplot(succ3, aes(x = parcela_bl, y=percent, fill=succ))+
  geom_bar(position="stack", stat = "identity", alpha = 0.8)+
  ggtitle("Percentage of fallen trees by treatment")+
  xlab("")+ ylab("")+ labs(fill='Successional
  Condition')+
  scale_fill_manual(values = eqm2)+
  theme_bw()


succ4 = succ %>%
  na.omit() %>% 
  group_by(parcela_bl, succ) %>% 
  summarise(unit = sum(unit))
succ4$percent = as.numeric(0)

ttl_crt = succ4[5,3]+succ4[6,3]
succ4[5,4] = (succ4[5,3]/ttl_crt)*100; succ4[6,4] = (succ4[6,3]/ttl_crt)*100

ttl_b3 = succ4[3,3]+succ4[4,3]
succ4[3,4] = (succ4[3,3]/ttl_b3)*100; succ4[4,4] = (succ4[4,3]/ttl_b3)*100

ttl_b1 = succ4[1,3]+succ4[2,3]
succ4[1,4] = (succ4[1,3]/ttl_b1)*100; succ4[2,4] = (succ4[2,3]/ttl_b1)*100

ggplot(succ4, aes(x = parcela_bl, y=percent, fill=succ))+
  geom_bar(position="stack", stat = "identity", alpha = 0.8)+
  ggtitle("Percentage of fallen trees by treatment - V2")+
  xlab("")+ ylab("")+ labs(fill='Successional
  Condition')+
  scale_fill_manual(values = eqm2)+
  theme_bw()

#Interactive Map ===============================================================================
library(leaflet)
library(raster)
library(sp)
library(rgdal)

df2 = df[,c(32, 31, 7, 29, 1, 10, 14, 17, 25, 26, 30)] #%>% na.omit()
df2 = df2 %>% filter(x != is.na(x))

xy = df2[,c(1, 2)]

loc_trees = SpatialPointsDataFrame(coords = xy, data = df2,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#writeOGR(loc_trees, dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes/Blowdown", layer = "Blowdown_trees_XY", driver="ESRI Shapefile")

area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

leaflet() %>% 
  #  addMiniMap(position = "bottomleft") %>%
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons(data = area1, color = "white", weight = 1, fillOpacity = 0) %>% 
  addCircles(data = loc_trees, color = "yellow", opacity = 0.9,
             popup = paste("Specie:", loc_trees@data$species, "<br>",
                           "Kind of damage:", loc_trees@data$tipo_de_dano, "<br>",
                           "Succesional stage:", loc_trees@data$succ, "<br>",
                           "Density:", loc_trees@data$densidade))
  
  

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




