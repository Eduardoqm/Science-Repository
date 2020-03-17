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

# Exporting level1BGeo as ESRI Shapefile
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

#Clip GEDI data (data.table objects)
## Clipping GEDI data within boundary box
level1BGeo_clip_bb <-clipLevel1BGeo(level1bGeo, xmin, xmax, ymin, ymax)
level2AM_clip_bb <- clipLevel2AM(level2AM, xmin, xmax, ymin, ymax)
level2BVPM_clip_bb <- clipLevel2BVPM(level2BVPM, xmin, xmax, ymin, ymax)
level1BPAIProfile_clip_bb <- clipLevel2BPAIProfile(level2BPAIProfile, xmin, xmax, ymin, ymax)
level2BPAVDProfile_clip_bb <- clipLevel2BPAVDProfile(level2BPAVDProfile, xmin, xmax, ymin, ymax)

## Clipping GEDI data by geometry
level1BGeo_clip_gb <- clipLevel1BGeoGeometry(level1bGeo,polygon_spdf, split_by=split_by)
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









