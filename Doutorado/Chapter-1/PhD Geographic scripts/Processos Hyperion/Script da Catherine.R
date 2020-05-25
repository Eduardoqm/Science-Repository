
#=================================== Pacotes ==================================#
if(!require(raster)){install.packages("raster")&library(raster)}else{library(raster)}
if(!require(rgdal)){install.packages("rgdal")&library(rgdal)}else{library(rgdal)}
if(!require(hsdar)){install.packages("hsdar")&library(hsdar)}else{library(hsdar)}
if(!require(data.table)){install.packages("data.table")&library(data.table)}else{library(data.table)}
if(!require(plyr)){install.packages("plyr")&library(plyr)}else{library(plyr)}
if(!require(dplyr)){install.packages("dplyr")&library(dplyr)}else{library(dplyr)}
if(!require(prospectr)){install.packages("prospectr")&library(prospectr)}else{library(prospectr)}
if(!require(zoo)){install.packages("zoo")&library(zoo)}else{library(zoo)}
if(!require(mdatools)){install.packages("mdatools")&library(mdatools)}else{library(mdatools)}
if(!require(ff)){install.packages("ff")&library(ff)}else{library(ff)}
if(!require(signal)){install.packages("signal")&library(signal)}else{library(signal)}
if(!require(tidyr)){install.packages("tidyr")&library(tidyr)}else{library(tidyr)}
if(!require(ggpubr)){install.packages("ggpubr")&library(ggpubr)}else{library(ggpubr)}

# VEGETATION INDEX -----
work_dir = "C:/Users/CatherineTorres/Desktop/INPE/3Tese/Process_biodiv/"
setwd(work_dir)
if(!file.exists("HyperData/2.veget_indices/VI_samples")){dir.create("HyperData/2.veget_indices/VI_samples")}

spectral_metadata <- read.csv("HyperData/spectral_metadata.csv", sep=";")

ivs <- c("CRI1","CRI2","CRI3","CRI4","DD","DDn",
         "DWSI1","DWSI2","DWSI3","DWSI4","DWSI5",
         "EVI","Green NDVI","LWVI1","LWVI2","MSI","NDLI","NDNI",
         "NDVI","NDVI2","NDWI","PRI","PSRI","PWI","REP_Li","SR","SRWI",
         "Vogelmann","Vogelmann2")
self_index <- c("(1/R550) - (1/R700)", # ARI1
                "((1/R550) - (1/R700))*R800", # ARI2
                "R1725 - R970", # Dlai
                "2.5*(R800-R680)/(R800+2.4*R680+1)", # EVI2
                "(R2160-R1540)/(R2160+R1540)", # NDBleaf
                "(R925-R710)/(R925+R710)", # NDchl
                "((R714+R752)/2)-R733", # RVSI
                "(R550-R680)/(R550+R680)", # VIgreen
                # IVs adicionados em 20/05/19:
                "(R818-R1651)/(R818+R1651)", # NDII
                "(R818-R2127)/(R818+R2127)", # NBR
                "(R978-R2100)/(R978+R2100)") # NBR2
vegindices = c(ivs, self_index)

# Raster file
infile <- list.files("HyperData/1.Ref_subset/Ref_samples/", full.names = T,
                     pattern = ".tif")
infile <- infile[-grep(".aux", infile)]

out_file <- list.files("HyperData/1.Ref_subset/Ref_samples/", pattern = ".tif")
out_file <- out_file[-grep(".aux", out_file)]

# sem chunks:
for(i in 1:length(infile)){
  crop_image = brick(infile[i])/10000
  ra <- speclib(crop_image, wavelength= spectral_metadata[,2]*1000, 
                fwhm= spectral_metadata[,3]*1000)
  ra_mask <- ra
  mask(ra_mask) <- c(379.97,455, #até b12
                     1050,1052, # b101
                     1186,1198, # b125-127
                     1327,1501, # b150 a b181
                     1756,2089, # b227-287
                     2111,2155, # b292-b299
                     2165,2505.4) # b301-b363
  
  VI <- vegindex(ra_mask, index= vegindices, weighted = F)
  
  VI_raster = VI@spectra@spectra_ra
  CAI = 0.5*(crop_image[[278]]+crop_image[[307]])-crop_image[[289]]
  
  VI38 = addLayer(VI_raster, CAI)
  
  writeRaster(VI38, paste0("HyperData/2.veget_indices/VI_samples/",out_file[i]), 
              overwrite = TRUE, format="GTiff")
}


# ABSORPTION FEATURES -----
setwd(work_dir)
if(!file.exists("HyperData/3.Absortion_Features/3.5.AF_samples")){dir.create("HyperData/3.Absortion_Features/3.5.AF_samples")}

# 3.4- Absorption Features extraction: smoothing reflectance ----

contRemoval <- function(x, wav) {
  crfun <- function(x, wav) {
    if(is.infinite(max(x, na.rm = T))) x <- rep(1,length(x))
    id <- sort(chull(c(0, x, 0)))
    id <- id[-c(1, length(id))] - 1
    cont <- approx(x = wav[id], y = x[id], xout = wav, method = "linear")$y
    return(cont)
  }
  cont <- t(apply(x, 1, function(x) crfun(x, wav)))
  cr <- x/cont
  # colnames(cr) <- wav
  return(cr)
}

spectral_metadata <- read.csv("HyperData/spectral_metadata.csv", sep=";")

abs_features <- function(infile, outfile, le_band, ri_band, sg_window,
                         out_dir = "HyperData/3.Absortion_Features/3.5.AF_samples/"){
  for(i in 1:length(infile)){
    crop_image = brick(infile[i])
    x <- as.matrix(crop_image)
    x[,100:102] <- NA
    x[,125:127] <- NA
    at <- attr(x, "dimnames")
    # Preencher NAs:
    if(anyNA(x) & !is.infinite(max(x, na.rm = T))){
      fill = try(x <- na.approx(t(x)), silent = T)
      if(anyNA(x) & class(fill) == "try-error"){
        x <- t(na.locf(na.approx(t(x), na.rm = F), na.rm = F))
        if(anyNA(x)){
          x <- t(na.locf(t(x), fromLast= T))
        }} else{
          x <- t(fill)
        }}
    for(l in 1:nrow(x)){
      if(!is.infinite(max(x[l,], na.rm = T)))
        x[l,which(is.na(x[l,]))] <- 0
    }
    attr(x, "dimnames") <- at
    
    sg1 <- savitzkyGolay(x[,9:337], w = sg_window, p = 1, m= 0)
    coord <- coordinates(crop_image)
    
    # Subset the interest region
    sg <- sg1[,(le_band-(8+floor(sg_window/2))):(ri_band-(8+floor(sg_window/2)))]
    wav <- spectral_metadata$wavelength[le_band:ri_band]*1000 # nm
    # cont <- unname(contRemoval(sg, wav))
    # Features
    bd <- unname(1 - contRemoval(sg, wav))
    wav_position = apply(bd, 1, function(x) if(length(wav[which.max(x)])==0) NA 
                         else wav[which.max(x)])
    depth = apply(bd, 1, max, na.rm= T)
    
    bd_left = alply(bd, 1, function(x) tryCatch(x[1:which.max(x)], error=function(e) NA))
    bd_right = alply(bd, 1, function(x) tryCatch(x[which.max(x):length(wav)], error=function(e) NA))
    wav_left = alply(bd, 1, function(x) tryCatch(wav[1:which.max(x)], error=function(e) NA))
    wav_right = alply(bd, 1, function(x) tryCatch(wav[which.max(x):length(wav)], error=function(e) NA))
    
    interp_left = mapply(function(x, y, n) tryCatch(approx(x, y, n= length(wav_left[[1]])*50), error=function(e) NA), x= wav_left, y= bd_left, SIMPLIFY = F)
    interp_right = mapply(function(x, y, n) tryCatch(approx(x, y, n= length(wav_right[[1]])*50), error=function(e) NA), x= wav_right, y= bd_right, SIMPLIFY = F)
    
    wav_fwhm_left = as.vector(mapply(function(interp_ref,mid_depth) tryCatch(interp_ref$x[which.min(abs(interp_ref$y-mid_depth))], error=function(e) NA), interp_left, depth/2))
    wav_fwhm_right = as.vector(mapply(function(interp_ref,mid_depth) tryCatch(interp_ref$x[which.min(abs(interp_ref$y-mid_depth))], error=function(e) NA), interp_right, depth/2))
    
    width = wav_fwhm_right - wav_fwhm_left
    
    n_left = as.vector(wav_position - wav[1])
    n_right = as.vector(wav[length(wav)] - wav_position)
    
    left_points = list()
    for(j in 1:length(bd_left)){
      left_points[[j]] = tryCatch(approx(x= wav_left[[j]], y= bd_left[[j]], n= n_left[j])$y, 
                                  error=function(e) NA)
    }
    right_points = list()
    for(k in 1:length(bd_right)){
      right_points[[k]] = tryCatch(approx(x= wav_right[[k]], y= bd_right[[k]], n= n_right[k])$y, 
                                   error=function(e) NA)
    }
    area = mapply(function(left_points,right_points) tryCatch(sum(c(left_points, right_points)), error=function(e) NA),
                  left_points,right_points)
    
    area_left = sapply(left_points, function(x) sum(x))
    area_right = sapply(right_points, function(x) sum(x))
    assymetry = area_left/area_right
    
    df <- cbind(coord, wav_position, depth, width, area, assymetry)
    dfr <- rasterFromXYZ(df, res= res(crop_image), crs= crs(crop_image))
    writeRaster(dfr, filename= paste0(out_dir, outfile[i]), 
                format="GTiff", overwrite=TRUE)
  }
}

# Raster file
infile <- list.files("HyperData/1.Ref_subset/Ref_samples/", full.names = T,
                     pattern = ".tif")
infile <- infile[-grep(".aux", infile)]

out_file <- list.files("HyperData/1.Ref_subset/Ref_samples/", pattern = ".tif")
out_file <- out_file[-grep(".aux", out_file)]

abs_features(infile, outfile= paste0("sg500_",out_file), 13, 24, 5)
abs_features(infile, outfile= paste0("sg680_",out_file), 27, 55, 5)
abs_features(infile, outfile= paste0("sg980_",out_file), 76, 105, 5)
abs_features(infile, outfile= paste0("sg1200_",out_file), 109, 139, 5)
abs_features(infile, outfile= paste0("sg2100_",out_file), 278, 307, 5)

# Mixture Analysis ------
if(!file.exists("HyperData/4.SMA/Endm_samples/")){dir.create("HyperData/4.SMA/Endm_sample/")}

spectral_metadata <- read.csv("HyperData/spectral_metadata.csv", sep=";")

# Endmembers
endmembers_avg <- read.table("HyperData/endmembers_avg.txt", header= T)
endmembers_avg[,2:6] <- endmembers_avg[,2:6]/10000

endm_speclib <- speclib(as.matrix(endmembers_avg[,c(2,3,6)]), 
                        wavelength= endmembers_avg[,1]*1000) # nm

endm_speclib_mask <- endm_speclib
mask(endm_speclib_mask) <- c(1192,1193, # b126
                             2061,2089, # b282-287
                             2304,2500) # b327-b334

plot(endm_speclib, FUN=1, ylim= c(0,0.6), xlim= c(min(endmembers_avg[,1]*1000),max(endmembers_avg[,1]*1000)), col=2)
plot(endm_speclib_mask, FUN=1, ylim= c(0,0.6), xlim= c(min(endmembers_avg[,1]*1000),max(endmembers_avg[,1]*1000)))
plot(endm_speclib, FUN=2, ylim= c(0,0.6), xlim= c(min(endmembers_avg[,1]*1000),max(endmembers_avg[,1]*1000)))
plot(endm_speclib_mask, FUN=2, ylim= c(0,0.6), xlim= c(min(endmembers_avg[,1]*1000),max(endmembers_avg[,1]*1000)))

# Raster file
infile <- list.files("HyperData/1.Ref_subset/Ref_samples/", full.names = T,
                     pattern = ".tif")
infile <- infile[-grep(".aux", infile)]

out_file <- list.files("HyperData/1.Ref_subset/Ref_samples/", pattern = ".tif")
out_file <- out_file[-grep(".aux", out_file)]

for(i in 1:length(infile)){
  crop_image = brick(infile[i])/10000
  crop_image[[282]]@data@values[which(is.na(crop_image[[282]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[283]]@data@values[which(is.na(crop_image[[283]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[284]]@data@values[which(is.na(crop_image[[284]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[285]]@data@values[which(is.na(crop_image[[285]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[286]]@data@values[which(is.na(crop_image[[286]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[287]]@data@values[which(is.na(crop_image[[287]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[292]]@data@values[which(is.na(crop_image[[292]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[295]]@data@values[which(is.na(crop_image[[295]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[304]]@data@values[which(is.na(crop_image[[304]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[305]]@data@values[which(is.na(crop_image[[305]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[306]]@data@values[which(is.na(crop_image[[306]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[308]]@data@values[which(is.na(crop_image[[308]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[309]]@data@values[which(is.na(crop_image[[309]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[310]]@data@values[which(is.na(crop_image[[310]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[311]]@data@values[which(is.na(crop_image[[311]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[312]]@data@values[which(is.na(crop_image[[312]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[313]]@data@values[which(is.na(crop_image[[313]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[314]]@data@values[which(is.na(crop_image[[314]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[315]]@data@values[which(is.na(crop_image[[315]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[316]]@data@values[which(is.na(crop_image[[316]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[317]]@data@values[which(is.na(crop_image[[317]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[318]]@data@values[which(is.na(crop_image[[318]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[319]]@data@values[which(is.na(crop_image[[319]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[320]]@data@values[which(is.na(crop_image[[320]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[321]]@data@values[which(is.na(crop_image[[321]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[322]]@data@values[which(is.na(crop_image[[322]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[323]]@data@values[which(is.na(crop_image[[323]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[324]]@data@values[which(is.na(crop_image[[324]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[325]]@data@values[which(is.na(crop_image[[325]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  crop_image[[326]]@data@values[which(is.na(crop_image[[326]]@data@values) & !is.na(crop_image[[50]]@data@values))] <- 0
  
  ra <- speclib(crop_image, wavelength= spectral_metadata[,2]*1000, 
                fwhm= spectral_metadata[,3]*1000)
  ra_mask <- ra
  mask(ra_mask) <- c(379.97,455, #até b12
                     1050,1052, # b101
                     1186,1198, # b125-127
                     1327,1501, # b150 a b181
                     1756,2089, # b227-287
                     2304,2505.4) # b327-b363
  PLOT_unmix <- unmix(ra_mask, endm_speclib_mask)
  writeRaster(PLOT_unmix, filename= paste0("HyperData/4.SMA/Endm_samples/",out_file[i]), 
              format="GTiff", overwrite=TRUE)
}