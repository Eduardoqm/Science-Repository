##################################################
#Manipulation Data (Fragentation x IDH/IQV)      #
#================================================#
#Start: 04/04/2017 Finished:                     #
#By: Eduardo Q Marques - Plant Ecology Laboratory#
#E-mail: eduardobio2009@gmail.com                #
##################################################
#About:                                          #
#This script is made to manipulation and analysis#
#data of fragmentation in counties of River of   #
#Mortes Basin. How step to Juan's thesis.        #
##################################################

#Info: Até o momento consegui extrair o raster do município de NX usando o ArcGis.
#Após usei o R para reclassificar o raster e salvar em (.asc) e rodei a função ClassStat
#para obter as métricas da fragmentação para o município. 

#Packages requiries
library(raster)
library(SDMTools)

#Open raster
setwd("C:/Users/Eduardo Q Marques/Documents/Artigos/Fragmentação x IDH/Geodata/Bases/Municipios/NX")
dir()
class <- raster("2b_class_nx_2013.tif")
plot(class, col=topo.colors(20))

#Reclassify to change class
tab<-c(0,1,2,3,4,5,6,7,8,9,10,2,2,1,2,2,1,2,2,2,2,2);tab<-matrix(tab,11,2);tab #1= Natural, 2= Antropic
class2<-reclassify(class,tab)
plot(class2, col=topo.colors(20))

#Salving raster reclassified
writeRaster(class2, filename="2class_nx_2013.asc", format="ascii", 
            options="INTERLEAVE=BAND", overwrite=TRUE)

##Analysis of fragmentation
#Cleaner workspace
rm(list=ls(all=TRUE))

#Opening Data
setwd("C:/Users/Eduardo Q Marques/Documents/Artigos/Fragmentação x IDH/Geodata/Bases/Municipios/NX")
class <- raster("2class_nx_2013.asc")
color <- c("green", "red")
plot(class, col= color)

#Analysis
mat <- matrix(class)
result <-  ClassStat(mat2,  cellsize  =  1,  latlon  =  FALSE)

#Write sheet
setwd("C:/Users/Eduardo Q Marques/Documents/Artigos/Fragmentação x IDH/Geodata/Patch analyst")
write.csv(result,"Frag_2013_nx_r.csv")
