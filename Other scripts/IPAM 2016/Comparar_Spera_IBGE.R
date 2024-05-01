############################################################
#Comparar dados de area plantada do IBGE com dados da Spera#
############################################################
# carregar library
library(ggmap)
library(raster)
library(maptools)
library(spatial.tools)
library(snow)
library(rgdal)
require(XLConnect)
library(ipam)
library(ggplot2)
library(dplyr)
# Diretorio do shape
setwd("/home/eduardo/Documents/public-ipam/data_geo/shapes/municipios_IBGE/municipios_dissolve/")
dir()
proj2=CRS("+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

mun2001 <- readShapePoly("Mtopiba_2001.shp",verbose=TRUE, proj4string=proj2)

#########################################
##Quantas celulas de soja existem em cada municipio do matopiba entre os anos de 2001 a 2013?
#########################################
# diretorio dos dados da Spera
setwd("/home/eduardo/Documents/public-ipam/data_geo/Base_raster/Dados Spera/Mato Grosso (Cerrado)/only_tif")
dir()
x1=list.files(,pattern="*.tif");x1
spera = raster::stack(x1)

#reclassifying sp2001 to get just soybean cover types
tab<-c(0,1,2,4,5,6,11,13,99,0,1,0,0,1,1,0,0,0);tab<-matrix(tab,9,2);tab
mun_soja<-reclassify(spera[[5]],tab)

# set NA values to "zero"
fun <- function(x) { x[is.na(x)] <- 0; return(x)} 
mun_soja2 <- calc(mun_soja, fun)

summary(getValues(mun_soja2))

#o argumento sp dess função cria uma coluna na tabela de
#atributos do shape com a informação extraída
#Neste caso, foi extraíd@ numero de pixels
area(mun_soja2)
53664.67/10000
names(mun_soja2)="soja.sp2002" #função que da nome a coluna layer
teste <- raster::extract(mun_soja2, mun2001, fun = sum, na.rm = TRUE,sp=TRUE)

#Converter o numero de pixels em area (ha)
teste@data$area_ha=teste@data$soja.sp2002*5.366467

plot(teste@data$area_ha)
spera_data=teste
#Criar e salvar o shape
writeOGR(spera_data, dsn = '/home/eduardo/Documents/public-ipam/data_geo/Base_raster/Dados Spera/Mato Grosso (Cerrado)/only_tif', layer ='Spera_muncipio', driver = 'ESRI Shapefile')

########################################
#Segunda parte (Dados do IBGE)
########################################
## importa dados de produtividade do IBGE
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/municipios_IBGE/municipios_dissolve")

dir(,pattern="*.shp")

m01=readOGR(".","Mtopiba_2001")
m01@data$id=paste(m01@data$ESTADO,standardize(m01@data$NOME),sep="_")
plot(m01)

head(m01@data)

## importa dados de produtividade do ibge
setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/master_corrigido")
dir()
pm5=read.csv("Produção_Agricola_corrigido_2005.csv")
pm5$cultura2=gsub(" ","",as.character(pm5$cultura))
pm5$estado=as.character(pm5$estado)

##importa nomes dos municipios
setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/nomes_municipios")
dir()
est=read.csv("estados.csv")
est$estado=as.character(est$estado2) #Usado para fazer comparação dos estados
head(est)
##
pm05=merge(pm5,est[,c("estado","siglama")],by="estado",all.x=TRUE,all.y=FALSE)
pm05$id=paste(pm05$siglama,standardize(pm05$municipio),sep="_")
head(pm05)
str(pm05)

soja05=subset(pm05,cultura2=="soja(emgrao)",select=c(id,siglama,area.plantada_ha))
head(soja05)
names(soja05)=c("id","est","sojaap05")
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
m01@data = data.frame(m01@data, soja05.2[match(m01@data[,"id"], soja05.2[,"id"]),])
####
head(m01@data)
spplot(m01,"sojaap05")

xx=table(m01@data$id)
xx[xx>1]
soja_ibge=m01

#####################################
#Fazendo o merge das planilhas
#####################################
a=soja_ibge@data$id #criar coluna para fazer o merge
spera_data@data$id=a #criar coluna para fazer o merge

setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/Comparação Spera IBGE")
dir()
write.csv(soja_ibge,"soja_ibge2005.csv")
write.csv(spera_data,"soja_spera2005.csv")

ibge2002=read.csv("soja_ibge2005.csv")
sper2002=read.csv("soja_spera2005.csv")

#Merge dos dados
comp2002 = merge(sper2002,ibge2002,by.y="id",by.x="id",all.y=FALSE,all.x=FALSE)
comp2002[is.na(comp2002)] <- 0
comp2002$comp02=comp2002$area_ha-comp2002$sojaap05
head(comp2002)

subset(comp2002,comp02>20000)
#####################################
#Comparando dados da Spera e do IBGE
#####################################
#ggplot(comp2002, aes(id_merge)) +                    # basic graphical object
 # geom_line(aes(y=sojaap05), colour="red") +  # first layer
  #geom_line(aes(y=area_ha), colour="green")+  # second layer
  #ylab("Área Plantada (ha)")+xlab("Municípios do Matopiba (ID)")

ggplot(comp2002, aes(sojaap05*0.01,area_ha*0.01)) +                    # basic graphical object
  geom_point()+  # first layer
  ylab("Área Plantada spera (km²)")+xlab("Área Plantada IBGE (km²)")+
  ylim(0,6000)+
  xlim(0,6000)+
  geom_abline(intercept=0, slope=1)
