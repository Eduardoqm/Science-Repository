library(rGEDI)

# Study area boundary box coordinates
xmin<- -44.17246
xmax<- -44.0654
ymin<- -13.76913
ymax<- -13.67646

# Get path to GEDI data
gLevel1B<-gedifinder(level="GEDI01_B",xmin, xmax, ymin, ymax)
gLevel2A<-gedifinder(level="GEDI02_A",xmin, xmax, ymin, ymax)
gLevel2B<-gedifinder(level="GEDI02_B",xmin, xmax, ymin, ymax)

# Set output dir for downloading the files
outdir=setwd('C:\\Users\\Eduardo Q Marques\\Documents\\test_gedi')

# Downloading GEDI data
#LPDAACDataPool(filepath=gLevel1B,outdir=outdir)
#LPDAACDataPool(filepath=gLevel2A,outdir=outdir)
#LPDAACDataPool(filepath=gLevel2B,outdir=outdir)

#** Herein, we are using only a GEDI sample dataset for this tutorial.
# downloading zip file
download.file("https://github.com/carlos-alberto-silva/rGEDI/releases/download/datasets/examples.zip",destfile=paste0(outdir, "/examples.zip"))

# unzip file 
unzip(paste0(outdir,"\\examples.zip"))

# Reading GEDI data
gedilevel1b<-readLevel1B(level1Bpath = paste0(outdir,"\\GEDI01_B_2019108080338_O01964_T05337_02_003_01_sub.h5"))
gedilevel2a<-readLevel2A(level2Apath = paste0(outdir,"\\GEDI02_A_2019108080338_O01964_T05337_02_001_01_sub.h5"))
gedilevel2b<-readLevel2B(level2Bpath = paste0(outdir,"\\GEDI02_B_2019108080338_O01964_T05337_02_001_01_sub.h5"))

level1BGeo<-getLevel1BGeo(level1b=gedilevel1b,select=c("elevation_bin0"))
head(level1BGeo)

# Converting shot_number as "integer64" to "character"
level1BGeo$shot_number<-paste0(level1BGeo$shot_number)

# Converting level1BGeo as data.table to SpatialPointsDataFrame
library(sp)
level1BGeo_spdf<-SpatialPointsDataFrame(cbind(level1BGeo$longitude_bin0, level1BGeo$latitude_bin0),
                                        data=level1BGeo)

# Exporting level1BGeo as ESRI Shapefile ===========================================================
raster::shapefile(level1BGeo_spdf,paste0(outdir,"\\GEDI01_B_2019108080338_O01964_T05337_02_003_01_sub"))

library(leaflet)
library(leafsync)

leaflet() %>%
  addCircleMarkers(level1bGeo$longitude_bin0,
                   level1bGeo$latitude_bin0,
                   radius = 1,
                   opacity = 1,
                   color = "red")  %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addLegend(colors = "red", labels= "Samples",title ="GEDI Level1B")


# Extracting GEDI full-waveform for a giving shotnumber
wf <- getLevel1BWF(gedilevel1b, shot_number="19640521100108408")

par(mfrow = c(1,2), mar=c(4,4,1,1), cex.axis = 1.5)

plot(wf, relative=FALSE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
     xlab="Waveform Amplitude", ylab="Elevation (m)")
grid()
plot(wf, relative=TRUE, polygon=FALSE, type="l", lwd=2, col="forestgreen",
     xlab="Waveform Amplitude (%)", ylab="Elevation (m)")
grid()

# Get GEDI Elevation and Height Metrics
level2AM<-getLevel2AM(gedilevel2a)
head(level2AM[,c("beam","shot_number","elev_highestreturn","elev_lowestmode","rh100")])

# Converting shot_number as "integer64" to "character"
level2AM$shot_number<-paste0(level2AM$shot_number)

# Converting Elevation and Height Metrics as data.table to SpatialPointsDataFrame
level2AM_spdf<-SpatialPointsDataFrame(cbind(level2AM$lon_lowestmode,level2AM$lat_lowestmode),
                                      data=level2AM)

# Exporting Elevation and Height Metrics as ESRI Shapefile
raster::shapefile(level2AM_spdf,paste0(outdir,"\\GEDI02_A_2019108080338_O01964_T05337_02_001_01_sub"))

shot_number = "19640521100108408"

png("fig8.png", width = 8, height = 6, units = 'in', res = 300)
plotWFMetrics(gedilevel1b, gedilevel2a, shot_number, rh=c(25, 50, 75, 90))
dev.off()


#Get GEDI Vegetation Profile Biophysical Variables (GEDI Level2B) ==============================
level2BVPM<-getLevel2BVPM(gedilevel2b)
head(level2BVPM[,c("beam","shot_number","pai","fhd_normal","omega","pgap_theta","cover")])

# Converting shot_number as "integer64" to "character"
level2BVPM$shot_number<-paste0(level2BVPM$shot_number)

# Converting GEDI Vegetation Profile Biophysical Variables as data.table to SpatialPointsDataFrame
level2BVPM_spdf<-SpatialPointsDataFrame(cbind(level2BVPM$longitude_lastbin,level2BVPM$latitude_lastbin),
                                        data=level2BVPM)

# Exporting GEDI Vegetation Profile Biophysical Variables as ESRI Shapefile
raster::shapefile(level2BVPM_spdf,paste0(outdir,"\\GEDI02_B_2019108080338_O01964_T05337_02_001_01_sub_VPM"))

#Get Plant Area Index (PAI) and Plant Area Volume Density (PAVD) Profiles (GEDI Level2B) =======
level2BPAIProfile<-getLevel2BPAIProfile(gedilevel2b)
head(level2BPAIProfile[,c("beam","shot_number","pai_z0_5m","pai_z5_10m")])

level2BPAVDProfile<-getLevel2BPAVDProfile(gedilevel2b)
head(level2BPAVDProfile[,c("beam","shot_number","pavd_z0_5m","pavd_z5_10m")])

# Converting shot_number as "integer64" to "character"
level2BPAIProfile$shot_number<-paste0(level2BPAIProfile$shot_number)
level2BPAVDProfile$shot_number<-paste0(level2BPAVDProfile$shot_number)

# Converting PAI and PAVD Profiles as data.table to SpatialPointsDataFrame
level2BPAIProfile_spdf<-SpatialPointsDataFrame(cbind(level2BPAIProfile$lon_lowestmode,level2BPAIProfile$lat_lowestmode),
                                               data=level2BPAIProfile)
level2BPAVDProfile_spdf<-SpatialPointsDataFrame(cbind(level2BPAVDProfile$lon_lowestmode,level2BPAVDProfile$lat_lowestmode),
                                                data=level2BPAVDProfile)

# Exporting PAI and PAVD Profiles as ESRI Shapefile
raster::shapefile(level2BPAIProfile_spdf,paste0(outdir,"\\GEDI02_B_2019108080338_O01964_T05337_02_001_01_sub_PAIProfile"))
raster::shapefile(level2BPAVDProfile_spdf,paste0(outdir,"\\GEDI02_B_2019108080338_O01964_T05337_02_001_01_sub_PAVDProfile"))

#Plot Plant Area Index (PAI) and Plant Area Volume Density (PAVD) Profiles =====================
#specify GEDI beam
beam="BEAM0101"

# Plot Level2B PAI Profile
gPAIprofile<-plotPAIProfile(level2BPAIProfile, beam=beam, elev=TRUE)

# Plot Level2B PAVD Profile
gPAVDprofile<-plotPAVDProfile(level2BPAVDProfile, beam=beam, elev=TRUE)

#Clip GEDI data (h5 files; gedi.level1b, gedi.level2a and gedi.level2b objects) ================
## Clip GEDI data by coordinates
# Study area boundary box
xmin = -44.15036
xmax = -44.10066
ymin = -13.75831
ymax = -13.71244

## clipping GEDI data within boundary box
level1b_clip_bb <- clipLevel1B(gedilevel1b, xmin, xmax, ymin, ymax,output=paste0(outdir,"//level1b_clip_bb.h5"))
level2a_clip_bb <- clipLevel2A(gedilevel2a, xmin, xmax, ymin, ymax, output=paste0(outdir,"//level2a_clip_bb.h5"))
level2b_clip_bb <- clipLevel2B(gedilevel2b, xmin, xmax, ymin, ymax,output=paste0(outdir,"//level2b_clip_bb.h5"))

## Clipping GEDI data by geometry
# specify the path to shapefile for the study area
polygon_filepath <- system.file("extdata", "stands_cerrado.shp", package="rGEDI")

# Reading shapefile as SpatialPolygonsDataFrame object
library(rgdal)
polygon_spdf<-readOGR(polygon_filepath)
head(polygon_spdf@data) # column id name "id"
split_by="id"

# Clipping h5 files
level1b_clip_gb <- clipLevel1BGeometry(gedilevel1b,polygon_spdf,output=paste0(outdir,"//level1b_clip_gb.h5"), split_by=split_by)
level2a_clip_gb <- clipLevel2AGeometry(gedilevel2a,polygon_spdf,output=paste0(outdir,"//level2a_clip_gb.h5"), split_by=split_by)
level2b_clip_gb <- clipLevel2BGeometry(gedilevel2b,polygon_spdf,output=paste0(outdir,"//level2b_clip_gb.h5"), split_by=split_by)

#Clip GEDI data (data.table objects) ==========================================================
## Clipping GEDI data within boundary box
level1BGeo_clip_bb <-clipLevel1BGeo(level1bGeo, xmin, xmax, ymin, ymax)
level2AM_clip_bb <- clipLevel2AM(level2AM, xmin, xmax, ymin, ymax)
level2BVPM_clip_bb <- clipLevel2BVPM(level2BVPM, xmin, xmax, ymin, ymax)
level1BPAIProfile_clip_bb <- clipLevel2BPAIProfile(level2BPAIProfile, xmin, xmax, ymin, ymax)
level2BPAVDProfile_clip_bb <- clipLevel2BPAVDProfile(level2BPAVDProfile, xmin, xmax, ymin, ymax)

## Clipping GEDI data by geometry
level1BGeo_clip_gb <- clipLevel1BGeoGeometry(level1BGeo,polygon_spdf, split_by=split_by)
level2AM_clip_gb <- clipLevel2AMGeometry(level2AM,polygon_spdf, split_by=split_by)
level2BVPM_clip_gb <- clipLevel2BVPMGeometry(level2BVPM,polygon_spdf, split_by=split_by)
level1BPAIProfile_clip_gb <- clipLevel2BPAIProfileGeometry(level2BPAIProfile,polygon_spdf, split_by=split_by)
level2BPAVDProfile_clip_gb <- clipLevel2BPAVDProfileGeometry(level2BPAVDProfile,polygon_spdf, split_by=split_by)


## View GEDI clipped data by bbox
m1<-leaflet() %>%
  addCircleMarkers(level2AM$lon_lowestmode,
                   level2AM$lat_lowestmode,
                   radius = 1,
                   opacity = 1,
                   color = "red")  %>%
  addCircleMarkers(level2AM_clip_bb$lon_lowestmode,
                   level2AM_clip_bb$lat_lowestmode,
                   radius = 1,
                   opacity = 1,
                   color = "green")  %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery)  %>%
  addLegend(colors = c("red","green"), labels= c("All samples","Clip bbox"),title ="GEDI Level2A") 

## View GEDI clipped data by geometry
# color palette
pal <- colorFactor(
  palette = c('blue', 'green', 'purple', 'orange',"white","black","gray","yellow"),
  domain = level2AM_clip_gb$poly_id
)

m2<-leaflet() %>%
  addCircleMarkers(level2AM$lon_lowestmode,
                   level2AM$lat_lowestmode,
                   radius = 1,
                   opacity = 1,
                   color = "red")  %>%
  addCircleMarkers(level2AM_clip_gb$lon_lowestmode,
                   level2AM_clip_gb$lat_lowestmode,
                   radius = 1,
                   opacity = 1,
                   color = pal(level2AM_clip_gb$poly_id))  %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addPolygons(data=polygon_spdf,weight=1,col = 'white',
              opacity = 1, fillOpacity = 0) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addLegend(pal = pal, values = level2AM_clip_gb$poly_id,title ="Poly IDs" ) 

sync(m1, m2)

#Compute descriptive statistics of GEDI Level2A and Level2B data ===============================
# Define your own function
mySetOfMetrics = function(x)
{
  metrics = list(
    min =min(x), # Min of x
    max = max(x), # Max of x
    mean = mean(x), # Mean of x
    sd = sd(x)# Sd of x
  )
  return(metrics)
}

# Computing the maximum of RH100 stratified by polygon
rh100max_st<-polyStatsLevel2AM(level2AM_clip_gb,func=max(rh100), id="poly_id")
head(rh100max_st)

# Computing a serie statistics for GEDI metrics stratified by polygon
rh100metrics_st<-polyStatsLevel2AM(level2AM_clip_gb,func=mySetOfMetrics(rh100),
                                   id="poly_id")
head(rh100metrics_st)

# Computing the max of the Total Plant Area Index
pai_max<-polyStatsLevel2BVPM(level2BVPM_clip_gb,func=max(pai), id=NULL)
pai_max


# Computing a serie of statistics of Canopy Cover stratified by polygon
cover_metrics_st<-polyStatsLevel2BVPM(level2BVPM_clip_gb,func=mySetOfMetrics(cover),
                                      id="poly_id")
head(cover_metrics_st)

#Compute Grids with descriptive statistics of GEDI-derived Elevation and Height Metrics (Level2A)====
# Computing a serie of statistics of GEDI RH100 metric
rh100metrics<-gridStatsLevel2AM(level2AM = level2AM, func=mySetOfMetrics(rh100), res=0.005)

# View maps
library(rasterVis)
library(viridis)

rh100maps<-levelplot(rh100metrics,
                     layout=c(1, 4),
                     margin=FALSE,
                     xlab = "Longitude (degree)", ylab = "Latitude (degree)",
                     colorkey=list(
                       space='right',
                       labels=list(at=seq(0, 18, 2), font=4),
                       axis.line=list(col='black'),
                       width=1),
                     par.settings=list(
                       strip.border=list(col='gray'),
                       strip.background=list(col='gray'),
                       axis.line=list(col='gray')
                     ),
                     scales=list(draw=TRUE),
                     col.regions=viridis,
                     at=seq(0, 18, len=101),
                     names.attr=c("rh100 min","rh100 max","rh100 mean", "rh100 sd"))

# Exporting maps 
png("fig5.png", width = 6, height = 8, units = 'in', res = 300)
rh100maps
dev.off()

#Compute Grids with descriptive statistics of GEDI-derived Canopy Cover and Vertical Profile Metrics (Level2B) =================
# Computing a serie of statistics of Total Plant Area Index
level2BVPM$pai[level2BVPM$pai==-9999]<-NA # assing NA to -9999
pai_metrics<-gridStatsLevel2BVPM(level2BVPM = level2BVPM, func=mySetOfMetrics(pai), res=0.005)

# View maps
pai_maps<-levelplot(pai_metrics,
                    layout=c(1, 4),
                    margin=FALSE,
                    xlab = "Longitude (degree)", ylab = "Latitude (degree)",
                    colorkey=list(
                      space='right',
                      labels=list(at=seq(0, 1.5, 0.2), font=4),
                      axis.line=list(col='black'),
                      width=1),
                    par.settings=list(
                      strip.border=list(col='gray'),
                      strip.background=list(col='gray'),
                      axis.line=list(col='gray')
                    ),
                    scales=list(draw=TRUE),
                    col.regions=viridis,
                    at=seq(0, 1.5, len=101),
                    names.attr=c("PAI min","PAI max","PAI mean", "PAI sd"))

# Exporting maps 
png("fig6.png", width = 6, height = 8, units = 'in', res = 300)
pai_maps
dev.off()

#Simulating GEDI full-waveform data from Airborne Laser Scanning (ALS) 3-D point cloud and extracting canopy derived metrics ===============
# Specifying the path to ALS data
lasfile_amazon <- paste0(outdir, "\\Amazon.las")
lasfile_savanna <- paste0(outdir, "\\Savanna.las")

# Reading and plot ALS file
library(lidR)
library(plot3D)

las_amazon<-readLAS(lasfile_amazon)
las_savanna<-readLAS(lasfile_savanna)

# Extracting plot center geolocations
xcenter_amazon = mean(las_amazon@bbox[1,])
ycenter_amazon = mean(las_amazon@bbox[2,])
xcenter_savanna = mean(las_savanna@bbox[1,])
ycenter_savanna = mean(las_savanna@bbox[2,])

# Simulating GEDI full-waveform
wf_amazon<-gediWFSimulator(input=lasfile_amazon,output=paste0(getwd(),"//gediWF_amazon_simulation.h5"),coords = c(xcenter_amazon, ycenter_amazon))
wf_savanna<-gediWFSimulator(input=lasfile_savanna,output=paste0(getwd(),"//gediWF_savanna_simulation.h5"),coords = c(xcenter_savanna, ycenter_savanna))

# Plotting ALS and GEDI simulated full-waveform
png("gediWf.png", width = 8, height = 6, units = 'in', res = 300)

par(mfrow=c(2,2), mar=c(4,4,0,0), oma=c(0,0,1,1),cex.axis = 1.2)
scatter3D(las_amazon@data$X,las_amazon@data$Y,las_amazon@data$Z,pch = 16,colkey = FALSE, main="",
          cex = 0.5,bty = "u",col.panel ="gray90",phi = 30,alpha=1,theta=45,
          col.grid = "gray50", xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)")

plot(wf_amazon, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
     xlab="", ylab="Elevation (m)", ylim=c(90,140))
grid()
scatter3D(las_savanna@data$X,las_savanna@data$Y,las_savanna@data$Z,pch = 16,colkey = FALSE, main="",
          cex = 0.5,bty = "u",col.panel ="gray90",phi = 30,alpha=1,theta=45,
          col.grid = "gray50", xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)")

plot(wf_savanna, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="green",
     xlab="Waveform Amplitude (%)", ylab="Elevation (m)", ylim=c(815,835))
grid()
dev.off()

#Extracting GEDI full-waveform derived metrics without adding noise to the full-waveform ===========
wf_amazon_metrics<-gediWFMetrics(input=wf_amazon,
                                 outRoot=file.path(getwd(), "amazon"))
wf_savanna_metrics<-gediWFMetrics(input=wf_savanna,
                                  outRoot=file.path(getwd(), "savanna"))

metrics<-rbind(wf_amazon_metrics,wf_savanna_metrics)
rownames(metrics)<-c("Amazon","Savanna")
head(metrics[,1:8])

#Extracting GEDI full-waveform derived metrics after adding noise to the full-waveform =============
wf_amazon_metrics_noise<-gediWFMetrics(input=wf_amazon,
                                       outRoot=file.path(getwd(), "amazon"),
                                       linkNoise= c(3.0103,0.95),
                                       maxDN= 4096,
                                       sWidth= 0.5,
                                       varScale= 3)

wf_savanna_metrics_noise<-gediWFMetrics(
  input=wf_savanna,
  outRoot=file.path(getwd(), "savanna"),
  linkNoise= c(3.0103,0.95),
  maxDN= 4096,
  sWidth= 0.5,
  varScale= 3)

metrics_noise<-rbind(wf_amazon_metrics_noise,wf_savanna_metrics_noise)
rownames(metrics_noise)<-c("Amazon","Savanna")
head(metrics_noise[,1:8])

#Always close gedi objects, so HDF5 files won't be blocked! =======================
close(wf_amazon)
close(wf_savanna)
close(gedilevel1b)
close(gedilevel2a)
close(gedilevel2b)



#Edu LAS Exploration
library(lidR)
library(plot3D)
library(ggplot2)
library(plotly)
library(rayshader)
library(rasterVis)
library(viridis)

setwd('C:\\Users\\Eduardo Q Marques\\Documents\\test_gedi')
las_amazon<-readLAS("Amazon.las")
las_savanna<-readLAS("Savanna.las" )
#las_amazon<-readLAS(lasfile_amazon)
#las_savanna<-readLAS(lasfile_savanna)

plot(las_amazon)
amaz = as.data.frame(las_amazon@data)
sav = as.data.frame(las_savanna@data)

#Tryng rayshader 3D plots
gg = ggplot(amaz, aes(x=X, y=Y, col=Z))+
  geom_point(size = 1, alpha = 0.5)

plot_gg(gg,multicore=TRUE,width=10,height=10,scale=100,windowsize=c(1000,500),
        zoom = 0.55, phi = 50)

gg2 = ggplot(amaz, aes(x=X, y=Y, fill=Z))+
  geom_raster()+
  scale_fill_viridis()

plot_gg(gg2,multicore=TRUE,width=10,height=10,scale=100,windowsize=c(1000,500),
        zoom = 0.55, phi = 50)


#Tryng plotly 3D plots
plot_ly(x = amaz$X, y = amaz$Y, z = amaz$Z, color = amaz$Z, size = I(5))

plot_ly(x = sav$X, y = sav$Y, z = sav$Z, color = sav$Z, size = I(5))

        