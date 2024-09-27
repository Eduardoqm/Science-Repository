library(raster)
library(sf)
library(spatstat)

## parque do Xingu
setwd("C:\\Users\\dvsil\\DadosTanguro Dropbox\\Divino Silverio\\UNEMAT_pos graduacao\\Orientacao\\Lorrayne Goncalves\\dados\\shapes_grid_pan_amazonia\\pan_amazonia")
dir(pattern = "*.shp$")
rg=rgdal::readOGR(".","panamazonia")
plot(rg)


ptshex <- spsample(rg, 5000, type = "hexagonal")

# prepare coordinates, data, and proj4string
coords <- ptshex@coords   # coordinates
data   <- data.frame(id=1:dim(coords)[1])# data
crs    <- CRS(projection(rg)) # proj4string of coords

# make the SpatialPointsDataFrame object
ptshex2 <- SpatialPointsDataFrame(coords      = coords,
                               data        = data, 
                               proj4string = crs)
head(ptshex2)

plot(ptshex2,add=T)



## importar ecorregioes
setwd("G:\\Meu Drive\\UFRA\\Pesquisa\\Producao_cientifica\\book_chapter_fire_springer\\mapa\\wwf_ecorregioes")
dir()
#eco=rgdal::readOGR(".","Ecorregiao_SA_tropical_forest_fim")
eco=read_sf("Ecorregiao_SA_tropical_forest_fim.shp")

plot(eco,border='red')


ppt2=st_as_sf(ptshex2)

bff<-sf::st_intersection(ppt2,eco)
head(bff)
plot(bff)

setwd("C:\\Users\\dvsil\\DadosTanguro Dropbox\\Divino Silverio\\UNEMAT_pos graduacao\\Orientacao\\Lorrayne Goncalves\\dados\\shapes_grid_pan_amazonia\\pan_amazonia")
st_write(bff,"panamazonia_points2.shp")



rgdal::writeOGR(ptshex2,dsn="panamazonia_points",
                layer="panamazonia_points", 
         driver="ESRI Shapefile",
         overwrite_layer=TRUE)
