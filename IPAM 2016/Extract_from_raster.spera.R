## corrige eerro em ler polygon dataframes
# muda acentos para pontos
#Sys.setlocale(category = "LC_ALL", locale = "C")

# carregar library
library(ggmap)
library(raster)
library(maptools)
library(spatial.tools)
library(snow)
library(rgdal)
# Diretorio do shape
setwd("/home/eduardo/Documents/public-ipam/data_geo/shapes/municipios_IBGE/municipios2_coord")
#setwd("/home/eduardo/Documents/public-ipam/data_geo/shapes/municipios_IBGE/municipios_dissolve")
dir()

# shape dos municipios
mun2001 <-readOGR(".",layer="Matopiba_2001_proj")
#mun2001
#proj = "+init=epsg:4326"
proj2=CRS("+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

#mun2001 <- readShapePoly("Mtopiba_2001.shp",verbose=TRUE, proj4string=proj2)
#########################################
##Quantas celulas de soja existem em cada municipio do matopiba entre os anos de 2001 a 2013?
#########################################
# diretorio dos dados da Spera
setwd("/home/eduardo/Documents/public-ipam/data_geo/Base_raster/Dados Spera/Mato Grosso (Cerrado)/only_tif")
dir()
x1=list.files(,pattern="*.tif");x1
spera = raster::stack(x1)

#mun_sp<-crop(spera,mun2001)
#mun_sp_mask<-mask(mun_sp,mun2001)

#reclassifying sp2001 to get just soybean cover types
#table(values(spera)) #conferindo tipos de cobertura presentes no raster
tab<-c(0,1,2,4,5,6,11,13,99,0,1,0,0,1,1,0,0,0);tab<-matrix(tab,9,2);tab
mun_soja<-reclassify(spera[[2]],tab)


# set NA values to "zero"
fun <- function(x) { x[is.na(x)] <- 0; return(x)} 
mun_soja2 <- calc(mun_soja, fun)

summary(getValues(mun_soja2))
#mun_soja.area=raster::area(mun_soja)
#plot(mun_soja)

#writeRaster(mun_soja.area,"areasoja.spera2002.tif")
#another way to reclass
#ff=function(x){x[x==0]=NA;x[x==2]=NA;x[x==4]=NA;x[x==11]=NA;x[x==13]=NA;x[x==99]=NA;return(x)}
#soja=calc(sp2001,fun=ff)
x11()
plot(mun_soja[[2]])


#start.time <- Sys.time()
beginCluster( detectCores()-1 ) #use all but one core


#o argumento sp dess função cria uma coluna na tabela de
#atributos do shape com a informação extraída
#Neste caso, foi extraíd@ numero de pixels
area(mun_soja2)
53664.67/10000
names(mun_soja2)="soja.sp2002"
teste <- raster::extract(mun_soja2, mun2001, fun = sum, na.rm = TRUE,sp=TRUE)




teste@data$area_ha=teste@data$layer*5.366467

hist(teste@data$area_ha)
subset(teste@data,teste@data$NOME=="Sorriso")

endCluster()


pixels<-data.frame()
area_pixel<-(res(mun_soja2)^2)/10000
area<-pixels*area_pixel[1]
barplot(area$teste.data.layer)

barplot(teste@data$layer)


#Criar e salvar o shape
writeOGR(teste, dsn = '/home/eduardo/Documents/public-ipam/data_geo/Base_raster/Dados Spera/Mato Grosso (Cerrado)/only_tif', layer ='teste', driver = 'ESRI Shapefile')

head(teste2)
###
head(mun2001)

mun2001$SoyArea
mun2001$mun=ipam::standardize(mun2001$NOME)
mun2001$id=paste(ipam::standardize(mun2001$ESTADO),mun2001$mun,sep="_")

setwd("/home/eduardo/Documents/public-ipam/producao_agricola_municipal_2002_2014/master_corrigido")
dir()

ibge02=read.csv("Produção_Agricola_corrigido_2002.csv")
unique(ibge02$municipio)
ibge02$cultura=as.character(ibge02$cultura)

head(ibge02)

s02=subset(ibge02,estado=="mt"|
      estado=="go"|estado=="to"|estado=="ma"|estado=="pi"|estado=="ba")
unique(s02$estado)

soja02=subset(s02,cultura=="soja(emgrao)")

xx=table(soja02$id)
xx[xx>1]
xx=table(mun2001$id)
xx[xx>1]

soja02$id=paste(soja02$estado,soja02$municipio,sep="_")
xx=subset(soja02,id=="go_ouro verde de goias");xx

head(soja02)

dff=merge(mun2001@data,soja02,by.x="id",by.y="id",all=TRUE)
dim(dff)
head(dff)

ggplot(dff,aes(x=SoyArea,y=area.plantada_ha))+
  geom_point()+
  ylim(c(0,400000))