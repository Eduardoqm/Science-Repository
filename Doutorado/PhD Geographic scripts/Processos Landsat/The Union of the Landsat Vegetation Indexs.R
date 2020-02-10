###############################################
# THE UNION OF THE LANDSAT VEGETATION INDEXS  #
#          => A Temporal scale <==            #
#            => CORE and EDGE <=              #
# By: Eduardo Q Marques   06-02-2020          #
###############################################

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#NDVI ======================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

ndvi <- stack(list)

#EVI =======================

#VIG =======================

#NDII ======================

#NDWI ======================

#NBRI ======================