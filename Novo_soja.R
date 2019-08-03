#Ludmila Rattis
#handling EVI-MODIS data
#August 2016
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

##############################################################################
# title         : projHDF2GTiff.R
# purpose       : Converts desired HDF layer to a GeoTIFF and projects it 
# author        : Abdulhakim Abdi (@HakimAbdi)
# input         : HDF-EOS files
# output        : Projected GeoTIFF files 
##############################################################################
myloc = "/mnt/data/dados_publicos/Documents/MODIS_local/MODIS/MOD13Q1.006" # working directory
setwd(myloc)
datas=dir();datas
frm.srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" # original HDF SRS
to.srs = "+proj=longlat +datum=WGS84 +no_defs" # desired GeoTIFF SRS
s.nodata = -3000 # HDF nodata value
d.nodata = -3000 # GeoTIFF nodata value
lyr=2

#funcao para transformar hdf em tiff (deu certo!!)
##essa aqui deu certo!
for(j in 1:length(datas)){
  #  for(j in 1:3){
  #for (j in 254:length(datas)){
  setwd(paste("/mnt/data/dados_publicos/Documents/MODIS_local/MODIS/MOD13Q1.006",datas[j],sep="/")) # set the working directory to where the data is
  suppressWarnings(dir.create(paste(paste("/mnt/data/dados_publicos/Documents/MODIS_local/MODIS/MOD13Q1.006",datas[j],sep="/"),"Projected",sep="/"))) # create a directory to store projected files
  hdfs <- dir(pattern="hdf$")[!!grepl("h11v10|h12v10|h13v10|h11v09|h12v09|h13v09",dir(pattern="hdf$"))]
  gtiffs = gsub("hdf","tif",hdfs)
  for (i in 1:length(hdfs)){ 
    gdal_translate(hdfs[i],gtiffs[i],sd_index=lyr) # extract the specified HDF layer and save it as a Geotiff
    gdalwarp(gtiffs[i],paste(paste("/mnt/data/dados_publicos/Documents/MODIS_local/MODIS/MOD13Q1.006",datas[j],sep="/"),"Projected",gtiffs[i],sep="/"),s_srs=frm.srs,t_srs=to.srs,srcnodata=-3000,dstnodata=-3000,overwrite = T) # project geotiffs
    unlink(gtiffs[i]) # delete unprojected geotiffs to save space
  }
}

#--------------------------------------------#
#funcao para fazer o mosaico, deu certo!!
for(j in 1: length(datas)){
  #for(j in 1:3){
  #  for (j in 254:344){
  #setwd(paste("/mnt/data/dados_publicos/Documents/MODIS_local/MODIS/MOD13Q1.006",datas[j],sep="/")) # set the working directory to where the data is
  gdalwarp(list.files(paste("/mnt/data/dados_publicos/Documents/MODIS_local/MODIS/MOD13Q1.006",datas[j],sep="/","Projected"),pattern = ".tif", full.names = T),
           paste("/mnt/data/dados_publicos/Documents/MODIS_local/MATOPIBA/MATOPIBA - EVI_NDVI/","mosaic.",datas[j],".tif",sep=""),
           overwrite = TRUE)
}

#limpando workspace
rm(list=ls(all=TRUE))
setwd("~/Documents/public-ipam/data_geo/Base_raster/Matopiba_Latlong/Spera_tang_latlong")
spera.230<-stack(list.files(pattern='.tif'))
setwd("~/Documents/public-ipam/MODIS_local/MATOPIBA/MATOPIBA - EVI_NDVI")
a<-list.files(pattern='mosaic')
mos<-stack(a[1:344])
mos.2<-crop(arag_croplands,spera.230) #para a Tanguro
spera<-resample(spera.230,mos.2,method='ngb')
limite<-rasterToPolygons(arag_croplands[[1]], dissolve=TRUE)
mos.3<-crop(mos.2,limite)
mos.4<-mask(mos.3,limite)
plot(mos.4[[1:4]])
mos.factor<-mos.4*0.0001;plot(mos.factor[[1:9]])

#separando EVI por uso do solo
extent(spera)
extent(mos.factor)

(table<-matrix(c(0,1,2,4,5,6,11,99,0,1,2,3,4,5,6,7),8,2))
spera<-reclassify(spera,table)

evi.spera<-stack(spera,mos.factor)


evi.2001<-mos.factor[[1:23]];evi.spera2001=stack(spera[[1]],evi.2001);names(evi.spera2001)
evi.2002<-mos.factor[[24:46]];evi.spera2002=stack(spera[[2]],evi.2002);names(evi.spera2002)

evi.2003<-mos.factor[[47:69]];evi.spera2003=stack(spera[[3]],evi.2003);names(evi.spera2003)
evi.2004<-mos.factor[[70:92]];evi.spera2004=stack(spera[[4]],evi.2004);names(evi.spera2004)
#substituindo NAs por zero
#evi.spera2001[is.na(evi.spera2001)] <- 0
#substituir outras classes de uso, diferentes de 1 (soja), por zero
#n?o deu certo
#soy2001=stackApply(evi.spera2001,indices=c(2:24),fun = function(x) ifelse(x[1] <= 1, x, 0))
evi.spera2001[[1]]
#tabela dos valores do primeiro layer
table(getValues(evi.spera2001[[1]]))
table(getValues(evi.spera2002[[1]]))

plot(spera[[1]])


evi.spera2001


fun <- function(x) { x[x[[1]]!=1] <- 0; return(x) }
evi.soy.2001 <- calc(evi.spera2001, fun);evi.soy.2001 = evi.soy.2001[[-1]]
plot(evi.soy.2001)
evi.soy.2002 <- calc(evi.spera2002, fun);evi.soy.2002 = evi.soy.2002[[-1]]
plot(evi.soy.2002)
evi.soy.2003 <- calc(evi.spera2003, fun);evi.soy.2003 = evi.soy.2003[[-1]]
plot(evi.soy.2003)
evi.soy.2004 <- calc(evi.spera2004, fun);evi.soy.2004 = evi.soy.2004[[-1]]
plot(evi.soy.2004)

#paradinha para plotar os resultados e conferir se est? tudo certo
x11()
par(mfrow=c(1,3))
plot(evi.2001[[1]])
plot(spera[[1]])
plot(evi.soy.2001[[2]])

breakpoints <- c(0,1,2,3,4,5,6,7)
colors <- c("gray90","green","yellow","red","green4","blue","gray50","black")
arg <- list(at=c(0,1,2,3,4,5,6,7), labels=c("Cerrado or Pasture","Soy","Corn",
                                            "Cotton","Soy/Cotton","Soy/Corn",
                                            "Irrigated",
                                            "Unclassified"))
r.range <- c(minValue(spera), maxValue(spera))
par(mai=c(1,2,0.1,0.1))
plot(spera, col=colors, legend=FALSE, axes=FALSE,box=FALSE)      
plot(spera, legend.only=TRUE,inset=-0.10, col=colors,
     legend.width=1, legend.shrink=0.75,smallplot=c(0,.09, .3,.75),
     axis.args=arg,labels=seq(r.range[1], r.range[2], 25), 
     cex.axis=0.6,
     legend.args=list(text='', side=4, font=2, line=2.5, cex=0.4))

#ok, eu reclassifiquei os mapas de EVI para obter o dado de EVI somente para as ?reas de soja
#o que segue aqui n?o me ajuda muito
#eu fiz a m?dia mensal. Eu prefiro os dados de 16 em 16 dias, porque
#da? eu chego mais perto da data de plantio
sort(c(rep(c(1:9,11,12),2),10))
stackApply(evi.soy.2001,indices=c(sort(c(rep(c(1:9,11,12),2),10))),fun=mean)
plot(stackApply(evi.soy.2001,indices=c(sort(c(rep(c(1:9,11,12),2),10))),fun=mean))
evi.soy.2001
evi.spera2001
teste<-evi.spera2001[[-1]]
teste
evi.soy.2001.mon.mean<-stackApply(evi.soy.2001,indices=c(sort(c(rep(c(1:9,11,12),2),10))),fun=mean) #rasters das mÃ©dias mensais
par(mfrow=c(1,1))
plot(cellStats(evi.soy.2001,stat=mean))
evi.soy.2001.mon.mean<-stackApply(evi.soy.2001,indices=c(sort(c(rep(c(1:9,11,12),2),10))),fun=mean) #rasters das mÃ©dias mensais
plot(cellStats(evi.soy.2001,stat=mean))
plot(cellStats(evi.soy.2001.mon.mean,stat=mean))



#plotando os dados de EVI a cada 16 dias
plot(cellStats(evi.soy.2001,stat=mean))
(data.plantio = unique(substr(names(evi.soy.2001), 8, 17)))

evi.soy.2001[evi.soy.2001==0]<-NA
x11()
plot(cellStats(evi.soy.2001,stat=mean),type='l',xaxt='n',xlab='',ylab = "mean(EVI)")
axis(1, at=1:23, labels=data.plantio, las=2)

#-------------------------------------------------------------------------------------#

#ok, agora eu sei o EVI ao longo de todo o ano em todos os campos de soja 
#da Tanguro. Mas o ano agr?cola n?o come?a em janeiro, mas depois do vazio
#sanitario (15 de setembro). Logo, meu ano agr?cola vai come?ar em 29-08
names(mos.factor)
evi.2001<-mos.factor[[13:35]];evi.spera2001=stack(spera[[1]],evi.2001);names(evi.spera2001)
evi.2002<-mos.factor[[36:58]];evi.spera2002=stack(spera[[2]],evi.2002);names(evi.spera2002)
evi.2003<-mos.factor[[59:81]];evi.spera2003=stack(spera[[3]],evi.2003);names(evi.spera2003)
evi.2004<-mos.factor[[82:104]];evi.spera2004=stack(spera[[4]],evi.2004);names(evi.spera2004)
evi.2005<-mos.factor[[105:127]];evi.spera2005=stack(spera[[5]],evi.2005);names(evi.spera2005)
evi.2006<-mos.factor[[128:150]];evi.spera2006=stack(spera[[6]],evi.2006);names(evi.spera2006)
evi.2007<-mos.factor[[151:173]];evi.spera2007=stack(spera[[7]],evi.2007);names(evi.spera2007)
evi.2008<-mos.factor[[174:196]];evi.spera2008=stack(spera[[8]],evi.2008);names(evi.spera2008)
evi.2009<-mos.factor[[197:219]];evi.spera2009=stack(spera[[9]],evi.2009);names(evi.spera2009)
evi.2010<-mos.factor[[220:242]];evi.spera2010=stack(spera[[10]],evi.2010);names(evi.spera2010)
evi.2011<-mos.factor[[243:265]];evi.spera2011=stack(spera[[11]],evi.2011);names(evi.spera2011)
evi.2012<-mos.factor[[266:288]];evi.spera2012=stack(spera[[12]],evi.2012);names(evi.spera2012)
evi.2013<-mos.factor[[289:311]];evi.spera2013=stack(spera[[13]],evi.2013);names(evi.spera2013)
evi.2015<-mos.factor[[335:344]];evi.spera2015=stack(spera[[14]],evi.2015);names(evi.spera2015)

#substituindo NAs por zero
#evi.spera2001[is.na(evi.spera2001)] <- 0
#substituir outras classes de uso, diferentes de 1 (soja), por zero
#n?o deu certo
#soy2001=stackApply(evi.spera2001,indices=c(2:24),fun = function(x) ifelse(x[1] <= 1, x, 0))
evi.spera2001[[1]]
#tabela dos valores do primeiro layer
table(getValues(evi.spera2001[[1]]))
table(getValues(evi.spera2002[[1]]))
table(getValues(evi.spera2003[[1]]))
table(getValues(evi.spera2004[[1]]))
table(getValues(evi.spera2005[[1]]))
table(getValues(evi.spera2006[[1]]))
table(getValues(evi.spera2007[[1]]))
table(getValues(evi.spera2008[[1]]))
table(getValues(evi.spera2009[[1]]))
table(getValues(evi.spera2010[[1]]))
table(getValues(evi.spera2011[[1]]))
table(getValues(evi.spera2012[[1]]))
table(getValues(evi.spera2013[[1]]))
table(getValues(evi.spera2015[[1]]))

area.plantada<-c(39,38,191,2195,4537,4466,4871,5616,5334,5800,5622,2859,
                 2368,3729)
area.soja.ha<-area.plantada*6.25
plot(area.soja.ha,type='l',xaxt='n',xlab='',main="Ãrea soja da Fazenda Tanguro")
axis(1,at=1:14,labels=c(2001:2013,2015),las=2)

fun <- function(x) { x[x[[1]]!=1] <- 0; return(x) }
evi.soy.2001 <- calc(evi.spera2001, fun);evi.soy.2001 = evi.soy.2001[[-1]]
plot(evi.soy.2001)
evi.soy.2002 <- calc(evi.spera2002, fun);evi.soy.2002 = evi.soy.2002[[-1]]
plot(evi.soy.2002)
evi.soy.2003 <- calc(evi.spera2003, fun);evi.soy.2003 = evi.soy.2003[[-1]]
plot(evi.soy.2003)
evi.soy.2004 <- calc(evi.spera2004, fun);evi.soy.2004 = evi.soy.2004[[-1]]
plot(evi.soy.2004)
evi.soy.2005 <- calc(evi.spera2005, fun);evi.soy.2005 = evi.soy.2005[[-1]]
plot(evi.soy.2005)
evi.soy.2006 <- calc(evi.spera2006, fun);evi.soy.2006 = evi.soy.2006[[-1]]
plot(evi.soy.2006)
evi.soy.2007 <- calc(evi.spera2007, fun);evi.soy.2007 = evi.soy.2007[[-1]]
plot(evi.soy.2007)
evi.soy.2008 <- calc(evi.spera2008, fun);evi.soy.2008 = evi.soy.2008[[-1]]
plot(evi.soy.2008)
evi.soy.2009 <- calc(evi.spera2009, fun);evi.soy.2009 = evi.soy.2009[[-1]]
plot(evi.soy.2009)
evi.soy.2010 <- calc(evi.spera2010, fun);evi.soy.2010 = evi.soy.2010[[-1]]
plot(evi.soy.2010)
evi.soy.2011 <- calc(evi.spera2011, fun);evi.soy.2011 = evi.soy.2011[[-1]]
plot(evi.soy.2011)
evi.soy.2012 <- calc(evi.spera2012, fun);evi.soy.2012 = evi.soy.2012[[-1]]
plot(evi.soy.2012)
evi.soy.2013 <- calc(evi.spera2013, fun);evi.soy.2013 = evi.soy.2013[[-1]]
plot(evi.soy.2013)
evi.soy.2015 <- calc(evi.spera2015, fun);evi.soy.2015 = evi.soy.2015[[-1]]
plot(evi.soy.2015)

#paradinha para plotar os resultados e conferir se estÃ¡ tudo certo
x11()
par(mfrow=c(1,3))
plot(evi.2010[[1]])
plot(spera[[1]])
plot(evi.soy.2010[[2]])

#plotando os dados de EVI a cada 16 dias
plot(cellStats(evi.soy.2001,stat=mean))
(data.plantio = unique(substr(names(evi.soy.2001), 13, 17)))

soja <- stack(evi.soy.2001, evi.soy.2002, evi.soy.2003,
              evi.soy.2004, evi.soy.2005, evi.soy.2006,
              evi.soy.2007, evi.soy.2008, evi.soy.2009,
              evi.soy.2010, evi.soy.2011, evi.soy.2012,
              evi.soy.2013, evi.soy.2015)


writeRaster(soja,"soja.tif")

soja[soja==0]<-NA


df.soja <- as.data.frame(cellStats(soja, median))
head(df.soja)
df.soja2 <- data.frame(df.soja, do.call(rbind, strsplit(row.names(df.soja), "[.]")))
head(df.soja2)
df.soja2$Date <-  as.POSIXlt(with(df.soja2, 
                                  paste(X2,X3,X4, sep="/")), 
                             format = "%Y/%m/%d")
head(df.soja2)
df.soja2$Julian <- as.numeric(as.character(format(df.soja2$Date, "%j")))
names(df.soja2) <- c("m.evi", "ignore", "year", "month", "day","Date","Julian")


#criar uma coluna de dias agrÃ�colas e refazer o ggplot abaixo
#com os dias agrÃ�colas no eixo x
agri.day<-rep(seq(1,365,by=16),14);agri.day<-agri.day[-c(310:322)]
df.soja2$agri.day=agri.day

write.table(df.soja2,"df.soja2.txt",sep=',',col.names=TRUE)

library(ggplot2)
ggplot(df.soja2, aes(x = agri.day, y = m.evi, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  #  geom_smooth()+
  theme_bw()

#tentei fazer vÃ¡rios radial plot na mesma janela, mas
#nÃ£o consegui...

par(mar=c(0.1, 0.1, 0.1, 0.1)) #decrease default margin
layout(matrix(1:15, ncol=3)) #draw 4 plots to device
#loop over rows to draw them, add 1 as max and 0 as min for each var
seq(1,309,by=23)
lapply(seq(1,309,by=23), function(i) { 
  radial.plot(df.soja2[i,1], radial.pos = rad(df.soja2[i,8]),
              radlab = T,
              clockwise = T,
              rp.type = "p",
              lwd = 1,
              show.centroid = F,
              labels = df.soja2[i,4]
  )}
  
  #fazendo radial plots apenas para um ano (no caso 2001)
  s.2010 <- subset(df.soja2, year == 2001)
  radial.plot(s.2010$m.evi, radial.pos = rad(s.2010$agri.day),
              radlab = T,
              clockwise = T,
              rp.type = "p",
              lwd = 3,
              show.centroid = F,
              labels = (s.2010$agri.day))
  
  #radial plot de todos os anos
  s.2010 <- with(df.soja2, tapply(m.evi, list(year,month), mean,na.rm=T))
  s.2010[is.na(s.2010)] = 0
  radial.plot(s.2010, radial.pos = rad(seq(0, 360, 30)),
              radlab = F,
              clockwise = T,
              rp.type = "p",
              lwd = 3,
              show.centroid = F,
              labels = 1:12)
  
  #----------------------------------------------------------#
  #MÃ©todos para encontrar os breakpoints
  #----------------------------------------------------------#
  #MÃ©todo um: cross-entropy
  #hora de encontrar os breakpoints
  #tem que ser um data frame de apenas uma coluna
  evi<-as.data.frame(df.soja2[,1])
  cols<-c("Jan","Fev","Mar","Apr","May",
          "Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  rows<-c(2001:2013,2015)
  evi.ts<-data.frame(row.names=rows,col.names=cols)
  
  #"No Break-Points are Estimated"
  obj1 <- CE.NB(evi, distyp = 1, parallel = TRUE)
  #"No Break-Points are Estimated"
  obj1<-CE.NB(evi, Nmax = 10, eps = 0.01, rho = 0.05, M = 200, h = 5, a = 0.8, b = 0.8,
              distyp = 1, penalty = "BIC", parallel = TRUE)
  #"No Break-Points are Estimated"
  obj1<-CE.NB(evi, Nmax = 30, eps = 0.10, rho = 0.05, M = 200, h = 5, a = 0.8, b = 0.8,
              distyp = 1, penalty = "BIC", parallel = TRUE)
  #"No Break-Points are Estimated"
  obj2<-CE.NB(evi, Nmax = 50, eps = 0.10, rho = 0.05, M = 200, h = 5, a = 0.8, b = 0.8,
              distyp = 1, penalty = "BIC", parallel = TRUE)
  #"No Break-Points are Estimated"
  obj3<-CE.NB(evi, Nmax = 10, eps = 0.0010, rho = 0.05, M = 200, h = 5, a = 0.8, b = 0.8,
              distyp = 1, penalty = "BIC", parallel = TRUE)
  #"No Break-Points are Estimated"
  obj4<-CE.NB(evi, Nmax = 50, eps = 0.0010, rho = 0.05, M = 200, h = 5, a = 0.8, b = 0.8,
              distyp = 1, penalty = "BIC", parallel = TRUE)
  
  #-----------------------------------------------------------#
  #MÃ©todo 2: greenbrown package
  #-----------------------------------------------------------#
  #criando ts object
  #nÃ£o deu certo
  evi<-df.soja2[,1]
  evi.ate.2013<-evi[1:299]
  a<-matrix(evi.ate.2013,23,13)
  a<-as.data.frame(a)
  cols<-rep(c("Jan","Fev","Mar","Apr","May",
              "Jun","Jul","Aug","Sep","Oct","Nov","Dec"),each=2);cols=cols[-21]
  names(a)=c(2001:2013)
  ts3<-as.ts(a)
  years<-sort(rep(2001:2013,each=23))
  ab<-data.frame(cols,years,evi.ate.2013)
  evi.ts<-as.ts(ab)
  head(evi.ts)
  plot(evi.ts)
  
  #tentativa ts 2
  require(xts)
  evi2.ts <- as.ts(df.soja2$m.evi, order.by=as.Date(df.soja2$agri.day))
  class(evi2.ts)
  head(evi2.ts)
  evi3.ts<-ts(evi2.ts,frequency=23,start=c(2000,4))
  
  #tentativa ts3 - o arquivo df roda bem a anÃ¡lise de TRENDS
  evi<-df.soja2[,1]
  evi.ate.2013<-evi[1:299]
  library(lubridate)
  df <- ts(evi.ate.2013, start = decimal_date(as.Date("2000-08-28")), frequency = 23)
  #df2<- ts(evi.ate.2013, start = decimal_date(as.Date(df.soja2$agri.day)), frequency = 23)
  
  #tentativa ts4 - nÃ£o dÃ¡ certo
  evi.agri.day<-read.table("evi.agri.day.txt",sep='',header=TRUE);head(evi.agri.day)
  df2<-ts(evi.agri.day,start = decimal_date(as.Date("2000-08-28")), frequency = 23)
  
  # load the package and example data
  install.packages("greenbrown", repos="http://R-Forge.R-project.org")
  library(greenbrown)
  data(ndvi) # load the time series
  plot(ndvi) # plot the time series
  plot(df)
  # calculate trend (default method: TrendAAT)
  trd <- Trend(ndvi)
  trd
  
  trd2<- Trend(df)
  trd2
  
  # calculate trend but consider breakpoints
  trd <- Trend(ndvi, mosum.pval=1)
  plot(trd) 
  
  trd2 <- Trend(df, mosum.pval=1)
  plot(trd2)
  trd2
  
  trd3<-Trend(df, method = c("AAT", "STM", "SeasonalAdjusted"), mosum.pval = 0.05, 
              h = 0.15, breaks = NULL, funSeasonalCycle = MeanSeasonalCycle, 
              funAnnual = mean, sample.method = c("sample", "all", "none"), 
              sample.min.length = 0.75, sample.size = 30)
  
  plot(trd3)
  
  
  
  
  
  
  
  
  
  
  
  x11()
  plot(cellStats(evi.soy.2001,stat=mean, na.rm=TRUE),
       col=1,type='l',xaxt='n',xlab='',ylab = "mean(EVI)",ylim=c(0,1))
  par(new=T)
  plot(cellStats(evi.soy.2002,stat=mean, na.rm=TRUE),
       col=2,type='l',xaxt='n',xlab='',ylab = "mean(EVI)",ylim=c(0,1))
  lines(cellStats(evi.soy.2003,stat=mean, na.rm=TRUE),col=3)
  lines(cellStats(evi.soy.2004,stat=mean, na.rm=TRUE),col=4)
  lines(cellStats(evi.soy.2005,stat=mean, na.rm=TRUE),col=5)
  lines(cellStats(evi.soy.2006,stat=mean, na.rm=TRUE),col=6)
  lines(cellStats(evi.soy.2007,stat=mean, na.rm=TRUE),col=7)
  lines(cellStats(evi.soy.2008,stat=mean, na.rm=TRUE),col=8)
  lines(cellStats(evi.soy.2009,stat=mean, na.rm=TRUE),col=9)
  lines(cellStats(evi.soy.2010,stat=mean, na.rm=TRUE),col=10)
  lines(cellStats(evi.soy.2011,stat=mean, na.rm=TRUE),col=11)
  lines(cellStats(evi.soy.2012,stat=mean, na.rm=TRUE),col=12)
  lines(cellStats(evi.soy.2013,stat=mean, na.rm=TRUE),col=13)
  lines(cellStats(evi.soy.2015,stat=mean, na.rm=TRUE),col=14)
  axis(1, at=1:23, labels=data.plantio, las=2)
  legend("topright",1,c(2001:2013,2015),
         pch = 15,bty="n",cex=1, col = c(1:14),xjust=1.5,bg = "white")
  
  #otimo. Agora vou fazer no servidor, para todos os anos e plotar as curvas anuais
  #no mesmo grafico...
  
  
  