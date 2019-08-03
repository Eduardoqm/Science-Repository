######################################################
# Araguaia Valley-Croplands x EVI (2013) Alternative #
#                                                    #
# Eduardo Queiroz Marques-IPAM Canarana-MT/Brazil    #
# E-mail: eduardobio2009@gmail.com                   #
# 2016 November                                      #
######################################################

#About this Script

#Packages
library(rgdal)
library(rgeos)
library(raster)
library(maptools)
library(MODIS)
library(sp)
library(gdalUtils)
library(breakpoint)
library(plotrix)
library(circular)

#limpando workspace
rm(list=ls(all=TRUE))
setwd("/home/eduardo/evi_araguaia")
a<-list.files(pattern='.tif')
mos<-stack(a[1:344])
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/Limite Araguaia/")
limite=readOGR(".","Arag_valley_limit_wgs")
mos.3<-crop(mos,limite)
mos.factor<-mos.3*0.0001;plot(mos.factor[[1:9]])
#===================================================================================================
##Salve EVI data
setwd("/home/eduardo/evi_araguaia2/")
writeRaster(mos.factor, filename="EVI_arag", options="INTERLEAVE=BAND", overwrite=TRUE)
#===================================================================================================
#Spera croplands data
setwd("/mnt/data/dados_publicos/Documents/data_geo/Base_raster/Matopiba_Latlong/Spera_latlong/")
xx<-list.files(,pattern=".tif");xx
spera<-stack(xx[1:13])

sp15=raster("GY2015.tif")

#Change the extent
sp15b=resample(sp15,spera[[1]], method='ngb')

#Make a stack of all spera datas
spera = stack(spera, sp15b) 

spera13 <- crop(spera[[13]], limite)#Aqui escolhemos o ano
spera13 <- mask(spera13, limite)

tab<-c(0,1,2,4,5,6,11,13,99,0,1,2,3,4,5,6,7,8);tab<-matrix(tab,9,2);tab
spera2<-reclassify(spera13,tab) 
spera3<-projectRaster(spera2,mos.factor,method="ngb")
#===================================================================================================
##Saving Spera data
setwd("/home/eduardo/evi_araguaia2/")
writeRaster(spera3, filename="Spera2013", options="INTERLEAVE=BAND", overwrite=TRUE)
#===================================================================================================
#NEW STEP
#===================================================================================================
##Loading data
setwd("/home/eduardo/evi_araguaia2/")
mos.factor=stack("EVI_arag.grd")
spera3=stack("Spera2013.grd")
#===================================================================================================
#Selecting land uses:
##Class (1=Soybean, 2=Corn, 3=Cotton, 4=Soybean and cotton, 5=Soybean and corn)

#Soybean
#Creating time series
mos.tm <-as.Date(substr(names(mos.factor),10,19),"%Y.%m.%d")

mos.factor <- setZ(mos.factor, mos.tm, 'data')

#Select time serie
evi.1<-subset(mos.factor, which(getZ(mos.factor)>as.Date("2012-09-15")&
                                  getZ(mos.factor)<as.Date("2013-07-15")))

evi.spera2013=stack(spera3,evi.1)

fun <- function(x) { x[x[[1]]!=1] <- 0; return(x) }
evi.soy.2013 <- calc(evi.spera2013, fun);evi.soy.2013 = evi.soy.2013[[-1]]
plot(evi.soy.2013)

#Soybean and corn
#Select time serie
evi.1<-subset(mos.factor, which(getZ(mos.factor)>as.Date("2012-09-01")&
                                  getZ(mos.factor)<as.Date("2013-08-30")))

evi.spera2013=stack(spera3,evi.1)

fun <- function(x) { x[x[[1]]!=5] <- 0; return(x) }
evi.soyc.2013 <- calc(evi.spera2013, fun);evi.soyc.2013 = evi.soyc.2013[[-1]]
plot(evi.soyc.2013)

#Corn
#Select time serie
evi.1<-subset(mos.factor, which(getZ(mos.factor)>as.Date("2013-02-01")&
                                  getZ(mos.factor)<as.Date("2013-08-30")))

evi.spera2013=stack(spera3,evi.1)

fun <- function(x) { x[x[[1]]!=2] <- 0; return(x) }
evi.corn.2013 <- calc(evi.spera2013, fun);evi.corn.2013 = evi.corn.2013[[-1]]
plot(evi.corn.2013)

#Soybean and cotton
#Select time serie
evi.1<-subset(mos.factor, which(getZ(mos.factor)>as.Date("2012-09-01")&
                                  getZ(mos.factor)<as.Date("2013-08-30")))

evi.spera2013=stack(spera3,evi.1)

fun <- function(x) { x[x[[1]]!=4] <- 0; return(x) }
evi.soycot.2013 <- calc(evi.spera2013, fun);evi.soycot.2013 = evi.soycot.2013[[-1]]
plot(evi.soycot.2013)
#===================================================================================================
#EVI values (max, mean, median)
evi.max=max(evi.1);names(evi.max)="maximo"
evi.mean=mean(evi.1);names(evi.mean)="media"
evi.median=calc(evi.1,fun=median);names(evi.median)="median"

#mm<-stackApply(evi.soy.2013,indices=1:23,fun=median)

#res=zonal(stack(evi.soy.2013),mun_arag,fun="median")
#res

#res=zonal(stack(evi.max,evi.mean,evi.mediana),spera3,fun="mean")
#res

## extrair valores para cada municipio
#Class (1=Soybean, 2=Corn, 3=Cotton, 4=Soybean and cotton, 5=Soybean and corn)
setwd("/home/eduardo/evi_araguaia/")
limit_mun=readOGR(".","Mun_arag_valley_wgs")

cll=c(1,2,4,5)

res1=list()

for(i in 1:4){
  cat("\r",i)  
  funf <- function(x) { x[x[[1]]!=cll[i]] <- NA;return(x)}
  
  evi.ff=calc(stack(spera3,evi.max,evi.mean,evi.median),fun=funf)
  limit_mun@data$evi.mean <- extract(evi.ff[[3]], limit_mun, fun=mean,na.rm=TRUE)
  limit_mun@data$evi.max <- extract(evi.ff[[2]], limit_mun, fun=mean,na.rm=TRUE)
  limit_mun@data$evi.median <- extract(evi.ff[[4]], limit_mun, fun=mean,na.rm=TRUE)
 
  evi.ff=calc(stack(spera3,evi.max,evi.mean,evi.median),fun=funf)
  
 
   df0=limit_mun@data
  df0$cultura=cll[i]
  res1[[i]]=df0
}

resf=do.call('rbind',res1)
resf$ano=2013
head(resf)
write.csv(resf)
setwd("/home/eduardo2/Arquivos_work_temp/")
write.csv(resf,file="EVI_mean_median_max _work2.csv")
#==================================================================================================
#RESULTS
#==================================================================================================

#boxplot
#setwd("/home/eduardo/evi_araguaia2/")
#dir()
#evi_dat=read.csv("EVI_mean_median_max _work.csv")

#Mean
boxplot(resf$evi.mean~resf$cultura,
        col="gray", outline=F, boxwex=0.50, ylab="EVI")

#Median
boxplot(resf$evi.median~resf$cultura,
        col="gray", outline=F, boxwex=0.50, ylab="EVI")

#Max
boxplot(resf$evi.max~resf$cultura,
        col="gray", outline=F, boxwex=0.50, ylab="EVI")
#====================================================================================================
#Union of EVI and IBGE
#====================================================================================================
setwd("/home/eduardo2/Arquivos_work_temp/")
resf2<-read.csv("EVI_mean_median_max_corn.csv")
corn.2<-read.csv("EVI mediana_corn_IBGE_arag.csv")

ibge.corn.2013=merge(resf2,corn.2,by="NM_MUNICIP",all.x=TRUE)

#Salve
write.csv(ibge.corn.2013,file="IBGE_EVI_corn_2013b.csv")
#====================================================================================================
#Analisys
#====================================================================================================
setwd("/home/eduardo2/Arquivos_work_temp/")
evi.ibge <- read.csv("IBGE_EVI_corn_2013b.csv")

EVI <- evi.ibge$evi.max
IBGE <- evi.ibge$rendimento_medio_da_producao

#A função para regressão é “lm” e não requer pacote estatístico (variavel resposta ~ variável preditora)
resmodelo<-lm(EVI~log(IBGE))

#Sumário dos resultados do modelo
summary(resmodelo)

#Teste para NORMALIDADE (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(resmodelo))   ##teste de shapiro wilk (normalidade)

# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha)
plot(rstudent(resmodelo) ~ fitted(resmodelo), pch = 19)
abline(h = 0, lty = 2)

#Visualização gráfica lty é o tipo da linha 1: linha contínua; 2: linha descontínua
plot(EVI~log(IBGE))
abline(resmodelo,lty=2)





