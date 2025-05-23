#Convert XY trees to coordinates
#Blowdown Data (AREA-1)
#Eduardo Q Marques 01-06-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(viridis)
library(fmsb)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')

df = read.csv("blowdown_full_update_2021_B.csv", sep = ",")

#df2 = df[,c(1,7,8,9,18,19,20,21,22,23,27,28)]
#df2 = df2[,c(-4,-6,-7)]

# ===============================================================================================
#OBS:
#1-metragem e a distancia do canto da parcela leste para oeste, linha 1 (0m) para linha 31 (1500m)
#2-lodist e a distancia em metros da linha no sentido leste ou oeste
#3-nsdist com o transecto e em metros referente ao norte ou sul do transecto
#4-nsdits sem o transsecto e em metros referente a borda da parcela

#Linhas de 1 a 31, a cada 50 metros (X)
#Transectos de A a U, a cada 50 metros (Y)
#Execoes A, AA, AB, B:
#A = 0m
#AA = 10m
#AB = 20m
#B = 50m

#X = 0:1500
#Y = 0:1000

#Posso separar os metodos nas etapas de transformacao para as classes:

#5-10 tem nsdist(Y), transecto(Y) e metragem(X)

#10-20 tem transecto(Y) e metragem(X)

#40 tem linha(X), nsdist(Y) e lodist(X)

#Transformation in meters from Point A0 =======================================================
#Part 1 - Make Y coordenates
#Transect to meter
df$trasc_m[df$transecto == "A"] <- 0
df$trasc_m[df$transecto == "AA"] <- 10
df$trasc_m[df$transecto == "AB"] <- 20
df$trasc_m[df$transecto == "B"] <- 50
df$trasc_m[df$transecto == "C"] <- 100
df$trasc_m[df$transecto == "D"] <- 150
df$trasc_m[df$transecto == "E"] <- 200
df$trasc_m[df$transecto == "F"] <- 250
df$trasc_m[df$transecto == "G"] <- 300
df$trasc_m[df$transecto == "H"] <- 350
df$trasc_m[df$transecto == "I"] <- 400
df$trasc_m[df$transecto == "J"] <- 450
df$trasc_m[df$transecto == "K"] <- 500
df$trasc_m[df$transecto == "L"] <- 550
df$trasc_m[df$transecto == "M"] <- 600
df$trasc_m[df$transecto == "N"] <- 650
df$trasc_m[df$transecto == "O"] <- 700
df$trasc_m[df$transecto == "P"] <- 750
df$trasc_m[df$transecto == "Q"] <- 800
df$trasc_m[df$transecto == "R"] <- 850
df$trasc_m[df$transecto == "S"] <- 900
df$trasc_m[df$transecto == "T"] <- 950
df$trasc_m[df$transecto == "U"] <- 1000

df$trasc_m <- replace(df$trasc_m,is.na(df$trasc_m),0)
df$nsdist <- replace(df$nsdist,is.na(df$nsdist),0)

df$trasc_m = as.numeric(df$trasc_m)
df$y = (df$trasc_m+(df$nsdist))

#Part 2 - Make X coordenates
df$x = df$metragem #Just no work for trees bigger 40cm


#Convert lines in meters
lmeter = seq(0,1500, by = 50)#Sequence and for to convert lines in meters
for (x in 1:31) {
  df$linha_m[df$linha_bl == x] <- lmeter[[x]]
}

#Convert X=NA to line distance in meters
for (z in 1:length(df$x)) {
  df$x[[z]] <- replace(df$x[[z]],is.na(df$x[[z]]),df$linha_m[[z]])
}

#Adjust with lodist values
for (z in 1:length(df$x)) {
  df$x[[z]][df$lo[[z]] == "O"] <- (df$x[[z]] + df$lodist[[z]]) #West difference
  df$x[[z]][df$lo[[z]] == "L"] <- (df$x[[z]] - df$lodist[[z]]) #East difference
}

#Transformation in UTM from Point A0 =======================================================
#Um metro em UTM = 0.000009
#A coordenada do ponto A0 = -52.37642001, -13.07375168
#Coordenada do shape Area-1 = -52.37688, -13.07417

df$x = -((df$x*0.000009)+52.37688)
df$y = -((df$y*0.000009)+13.07417)



#Tests ggplot
#x11()
eqm = c("red","orange", "blue")
ggplot(df, aes(x=x, y=y))+
  #stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
  geom_point(aes(col = parcela_bl), size = 3, alpha = 0.5)+
  #geom_vline(xintercept = 0)+
  #geom_vline(xintercept = 500)+
  #geom_vline(xintercept = 1000)+
  #geom_vline(xintercept = 1500)+
  scale_color_manual(values = eqm)+
  theme_light()+
  coord_fixed()

ggplot(df, aes(x=x, y=y))+
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
  stat_density_2d(col = "black")+
  #geom_point(aes(col = parcela_bl), size = 3, alpha = 0.5)+
  #geom_vline(xintercept = 0)+
  #geom_vline(xintercept = 500)+
  #geom_vline(xintercept = 1000)+
  #geom_vline(xintercept = 1500)+
  scale_fill_viridis_c()+
  theme_light()+
  coord_fixed()

#ggplot(df, aes(x=x, y=y))+
#  geom_hex()



#3D Map ===============================================================================
a = ggplot(df, aes(x=x, y=y))+
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
  #stat_density_2d(col = "black")+
  #geom_point(aes(col = parcela_bl), size = 3, alpha = 0.5)+
  #geom_vline(xintercept = 0)+
  #geom_vline(xintercept = 500)+
  #geom_vline(xintercept = 1000)+
  #geom_vline(xintercept = 1500)+
  scale_fill_viridis_c()+
  coord_fixed()


#library(rayshader)
#plot_gg(a, multicore = TRUE, raytrace = TRUE, width = 7, height = 7,
#        scale = 300, windowsize = c(900, 600), zoom = 0.6, phi = 30, theta = 30)







library(leaflet)
library(raster)
library(sp)
library(rgdal)

df2 = df[,c(33, 32, 14)] %>% na.omit()
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
             popup = loc_trees@data$tipo_de_dano,
             label = loc_trees@data$tipo_de_dano)
  
  

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




