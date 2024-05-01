library(rgdal)
library(raster)
library(spatial.tools)
require(XLConnect)
library(ipam)
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/municipios_IBGE/municipios_dissolve")

dir(,pattern="*.shp")

m01=readOGR(".","Mtopiba_2010")
m01@data$id=paste(m01@data$ESTADO,standardize(m01@data$NM),sep="_")
plot(m01)

head(m01@data)


## importa dados de produtividade do ibge
setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/master")
dir()
pm5=read.csv("Producao_agricola_Brasil2011.csv")
pm5$cultura2=gsub(" ","",as.character(pm5$cultura))
pm5$estado=as.character(pm5$estado)

##importa nomes dos municipios
setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/nomes_municipios")
dir()
est=read.csv("estados.csv")
est$estado=as.character(est$siglami) #Usado para fazer comparação dos estados
head(est)
##
##
pm05=merge(pm5,est[,c("estado","siglama")],by="estado",all.x=TRUE,all.y=FALSE)
pm05$id=paste(pm05$siglama,standardize(pm05$municipio),sep="_")
head(pm05)
str(pm05)

soja05=subset(pm05,cultura2=="Soja(emgrão)",select=c(id,siglama,area.plantada_ha,
                                                     area.colhida_ha,producao_t,produtividade_kg_ha))
head(soja05)
names(soja05)=c("id","est","sojaap05","sojaac05","sojap05","sojapr05")
soja05.2=subset(soja05,est=="GO"|est=="MT"|est=="MA"|est=="PI"|
                  est=="BA"|est=="TO"|est=="DF")
head(soja05.2)

#### juntar dados producao com shape
head(m01@data)
head(soja05.2)
mast=merge(m01@data,soja05.2,by="id",all.x=TRUE)
dim(m01@data)
dim(soja05.2)
dim(mast)


#sp@data = data.frame(sp@data, df[match(sp@data[,by], df[,by]),])
m01@data = data.frame(m01@data, soja05.2[match(m01@data[,"id"], soja05.2[,"id"]),])
####
head(m01@data)
spplot(m01,"sojaap05")

xx=table(m01@data$id)
xx[xx>1]

#Criar e salvar o shape
writeOGR(m01, dsn = '/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/shapes', layer ='Soja_2011', driver = 'ESRI Shapefile')
