###############################################
#       TEMPORAL SERIES PACKAGE (Tests)       #
#          => A Temporal scale <==            #
# By: Eduardo Q Marques   12-02-2020          #
###############################################

#This script is for construction and test my oun package to work with temporal series and satelitte images

library(raster)
library(rgdal)

#Data bank to test======================================================================
ndvi <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))


#Polygon to get values
polygons <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="polygons")

polygon <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="polygon")

points <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="points")

point <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="point")

lines <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="lines")

line <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Temporal series package",layer="line")

polygons = spTransform(polygons, crs(ndvi))
polygon = spTransform(polygon, crs(ndvi))
points = spTransform(points, crs(ndvi))
point = spTransform(point, crs(ndvi))
lines = spTransform(lines, crs(ndvi))
line = spTransform(line, crs(ndvi))

#Tests ===================================================================================
#Points test
time_point = df_point(ndvi, point)
View(time_point)

time_points = df_points(ndvi, points)
View(time_points)




















