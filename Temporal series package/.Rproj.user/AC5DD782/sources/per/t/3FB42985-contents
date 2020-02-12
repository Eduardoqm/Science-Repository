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
#Data frame
time_point = df_point(ndvi, point)
time_point

time_points = df_points(ndvi, points, "min")
time_points

#Plots
gg_point(ndvi, point, "line")
gg_point(ndvi, point, "point")
gg_point(ndvi, point, "boxplot")
gg_point(ndvi, point, "area")
gg_point(ndvi, point, "bar")


gg_points(ndvi, points, "mean", "line")
gg_points(ndvi, points, "median", "line")
gg_points(ndvi, points, "max", "line")
gg_points(ndvi, points, "min", "line")
gg_points(ndvi, points, "sd", "line")

gg_points(ndvi, points, "sd", "point")
gg_points(ndvi, points, "sd", "bar")
gg_points(ndvi, points, "sd", "boxplot")
gg_points(ndvi, points, "sd", "area")
gg_points(ndvi, points, "mean", "area")

#Polygon tests
time_poly = df_poly(ndvi, polygon, "mean")
time_poly

time_poly2 = df_poly(ndvi, polygons, "mean")
time_poly2

gg_poly(ndvi, polygon, "mean", "line")
gg_poly(ndvi, polygon, "median", "line")
gg_poly(ndvi, polygon, "max", "line")
gg_poly(ndvi, polygon, "min", "line")
gg_poly(ndvi, polygon, "sd", "line")

gg_poly(ndvi, polygon, "sd", "point")
gg_poly(ndvi, polygon, "sd", "bar")
gg_poly(ndvi, polygon, "sd", "area")
gg_poly(ndvi, polygon, "sd", "boxplot")
gg_poly(ndvi, polygons, "sd", "boxplot")

#All
gg_points(ndvi, points, "mean", "line")
gg_poly(ndvi, polygon, "mean", "line")
gg_points(ndvi, points, "median", "line")
gg_poly(ndvi, polygon, "median", "line")
gg_points(ndvi, points, "max", "line")
gg_poly(ndvi, polygon, "max", "line")
gg_points(ndvi, points, "min", "line")
gg_poly(ndvi, polygon, "min", "line")
gg_points(ndvi, points, "sd", "line")
gg_poly(ndvi, polygon, "sd", "line")
