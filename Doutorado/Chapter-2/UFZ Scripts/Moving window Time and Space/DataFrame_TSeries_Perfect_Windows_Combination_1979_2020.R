#---------------------------------------------------------------------------
# Data Frame from optimal Temp/Space window combination
#
# Eduardo Q Marques 06-10-2022
#----------------------------------------------------------------------------

library(raster)
#Essential Functions ----------------------------------------------------------------------
#Function to plot data in EVE server
plotx = function(w) {
    setwd("/home/queirozm/eqm_eth_ufz/Temp_maps")
    png(paste0(deparse(substitute(w)), ".png"))
    plot(w)
    dev.off()
}

#Functions to moving x days
move = function(day, wnd, vari) { # Target day, size of window (number of days before and after), Variable to be blocked
  w = day-wnd
  k = day+wnd
  max(vari[w:k], na.rm=T)
}

#Load data ---------------------------------------------------------------------
#Wind Gust from ERA5 Single Levels
setwd("/home/queirozm/eqm_eth_ufz/Data")
ws1a = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-1979_1989.nc")
ws1b = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-1990_2000.nc")
ws1c = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-2001_2011.nc")
ws1d = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-2012_2020.nc")

#Wind Gust Bloked 3 pixels
ws3a = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_3pixels_1979_1989.nc")
ws3b = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_3pixels_1990_2000.nc")
ws3c = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_3pixels_2001_2011.nc")
ws3d = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_3pixels_2012_2020.nc")


#Wind Gust Bloked 5 pixels
ws5a = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_5pixels_1979_1989.nc")
ws5b = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_5pixels_1990_2000.nc")
ws5c = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_5pixels_2001_2011.nc")
ws5d = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_5pixels_2012_2020.nc")


#Wind Gust Bloked 7 pixels
ws7a = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_7pixels_1979_1989.nc")
ws7b = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_7pixels_1990_2000.nc")
ws7c = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_7pixels_2001_2011.nc")
ws7d = brick("ERA5_SL/Blocked_ERA5SL/ERA_SL_WG_block_7pixels_2012_2020.nc")

print("WS load")


#Recurrent Precipitation from ERA5 Land
tp1 = brick("ERA5_land/Total_Precipitation_ERA5-Land_1979_1989.nc")
tp2 = brick("ERA5_land/Total_Precipitation_ERA5-Land_1990_2000.nc")
tp3 = brick("ERA5_land/Total_Precipitation_ERA5-Land_2001_2011.nc")
tp4 = brick("ERA5_land/Total_Precipitation_ERA5-Land_2012_2020.nc") #Total Precipitation from ERA5 Land

#Precipitation Bloked 1 pixel
ppt1a = brick("ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_1979_1989.nc")
ppt1b = brick("ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_1990_2000.nc")
ppt1c = brick("ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_2001_2011.nc")
ppt1d = brick("ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_2012_2020.nc")

#Precipitation Bloked 3 pixel
ppt3a = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_3pixels_1979_1989.nc")
ppt3b = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_3pixels_1990_2000.nc")
ppt3c = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_3pixels_2001_2011.nc")
ppt3d = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_3pixels_2012_2020.nc")

#Precipitation Bloked 5 pixel
ppt5a = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_5pixels_1979_1989.nc")
ppt5b = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_5pixels_1990_2000.nc")
ppt5c = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_5pixels_2001_2011.nc")
ppt5d = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_5pixels_2012_2020.nc")

#Precipitation Bloked 7 pixel
ppt7a = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_7pixels_1979_1989.nc")
ppt7b = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_7pixels_1990_2000.nc")
ppt7c = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_7pixels_2001_2011.nc")
ppt7d = brick("ERA5_land/Blocked_ERA5land/ERA_Land_Prec_block_7pixels_2012_2020.nc")

print("PPT load")

library(rgdal)
pont = readOGR(dsn = "Shapes", layer = "Area1_ponto") #Area-1 point
xing = readOGR(dsn = "Shapes", layer = "Xingu_MT") #Area-1 point

ws1a = crop(ws1a, xing); ws1b = crop(ws1b, xing); ws1c = crop(ws1c, xing); ws1d = crop(ws1d, xing)
ws3a = crop(ws3a, xing); ws3b = crop(ws3b, xing); ws3c = crop(ws3c, xing); ws3d = crop(ws3d, xing)
ws5a = crop(ws5a, xing); ws5b = crop(ws5b, xing); ws5c = crop(ws5c, xing); ws5d = crop(ws5d, xing)
ws7a = crop(ws7a, xing); ws7b = crop(ws7b, xing); ws7c = crop(ws7c, xing); ws7d = crop(ws7d, xing)
print("WS cropped!")

#ws1 = stack(ws1a, ws1b, ws1c, ws1d); print("WS1 pixel stacked")
#ws3 = stack(ws3a, ws3b, ws3c, ws3d); print("WS3 pixel stacked")
#ws5 = stack(ws5a, ws5b, ws5c, ws5d); print("WS5 pixel stacked")
#ws7 = stack(ws7a, ws7b, ws7c, ws7d); print("WS7 pixel stacked")


ws3a@data@names = ws1a@data@names; ws5a@data@names = ws1a@data@names; ws7a@data@names = ws1a@data@names
ws3b@data@names = ws1b@data@names; ws5b@data@names = ws1b@data@names; ws7b@data@names = ws1b@data@names
ws3c@data@names = ws1c@data@names; ws5c@data@names = ws1c@data@names; ws7c@data@names = ws1c@data@names
ws3d@data@names = ws1d@data@names; ws5d@data@names = ws1d@data@names; ws7d@data@names = ws1d@data@names

#ws3@data@names = ws1@data@names #Names em ws1 stack
#ws5@data@names = ws1@data@names
#ws7@data@names = ws1@data@names

print("WS Renamed!")

#plotx(ws1); plotx(ws3); plotx(ws5); plotx(ws7)


ppt1a = crop(ppt1a, xing); ppt1b = crop(ppt1b, xing); ppt1c = crop(ppt1c, xing); ppt1d = crop(ppt1d, xing)
ppt3a = crop(ppt3a, xing); ppt3b = crop(ppt3b, xing); ppt3c = crop(ppt3c, xing); ppt3d = crop(ppt3d, xing)
ppt5a = crop(ppt5a, xing); ppt5b = crop(ppt5b, xing); ppt5c = crop(ppt5c, xing); ppt5d = crop(ppt5d, xing)
ppt7a = crop(ppt7a, xing); ppt7b = crop(ppt7b, xing); ppt7c = crop(ppt7c, xing); ppt7d = crop(ppt7d, xing)

#tp1 = crop(tp1, xing); tp2 = crop(tp2, xing); tp3 = crop(tp3, xing); tp4 = crop(tp4, xing)
print("PPT cropped!")

#tp = stack(tp1, tp2, tp3, tp4); print("TP stacked")
#ppt1 = stack(ppt1a, ppt1b, ppt1c, ppt1d); print("PPT1 pixel stacked")
#ppt3 = stack(ppt3a, ppt3b, ppt3c, ppt3d); print("PPT3 pixel stacked")
#ppt5 = stack(ppt5a, ppt5b, ppt5c, ppt5d); print("PPT5 pixel stacked")
#ppt7 = stack(ppt7a, ppt7b, ppt7c, ppt7d); print("PPT7 pixel stacked")


ppt1a@data@names = tp1@data@names; ppt3a@data@names = tp1@data@names; ppt5a@data@names = tp1@data@names; ppt7a@data@names = tp1@data@names
ppt1b@data@names = tp2@data@names; ppt3b@data@names = tp2@data@names; ppt5b@data@names = tp2@data@names; ppt7b@data@names = tp2@data@names
ppt1c@data@names = tp3@data@names; ppt3c@data@names = tp3@data@names; ppt5c@data@names = tp3@data@names; ppt7c@data@names = tp3@data@names
ppt1d@data@names = tp4@data@names; ppt3d@data@names = tp4@data@names; ppt5d@data@names = tp4@data@names; ppt7d@data@names = tp4@data@names


#ppt1@data@names = tp@data@names #Names em tp stack
#ppt3@data@names = tp@data@names
#ppt5@data@names = tp@data@names
#ppt7@data@names = tp@data@names
print("PPT Renamed!")

#plotx(ppt1); plotx(ppt3); plotx(ppt5); plotx(ppt7)

#Create points to every pixel --------------------------------------------------
coord = xyFromCell(ppt1a, 1:ncell(ppt1a)) #Extract coordinates by each cell
ponto = SpatialPoints(coord) #Convert coordinates in points to after make buffer
plotx(ponto)

#Calculating CHI and otimal windows (converting to dataframes)
library(tidyverse)
library(reshape2)
library(extRemes)

pix1 = function(pix){
        #Wind Speed
        w1 = raster::extract(ws1a, ponto[1])
        w1 = melt(w1)
        w1 = w1[,-1]

        w2 = raster::extract(ws1b, ponto[1])
        w2 = melt(w2)
        w2 = w2[,-1]

        w3 = raster::extract(ws1c, ponto[1])
        w3 = melt(w3)
        w3 = w3[,-1]

        w4 = raster::extract(ws1d, ponto[1])
        w4 = melt(w4)
        w4 = w1[,-1]

        wsdf = rbind(w1,w2,w3,w4)

        colnames(wsdf) = c("Date", "Wind")
        wsdf$Date = substr(wsdf$Date, 2, 14)

        #Precipitation
        p1 = raster::extract(ppt1a, ponto[1])
        p1 = melt(p1)
        p1 = p1[,-1]

        p2 = raster::extract(ppt1b, ponto[1])
        p2 = melt(p2)
        p2 = p2[,-1]

        p3 = raster::extract(ppt1c, ponto[1])
        p3 = melt(p3)
        p3 = p3[,-1]

        p4 = raster::extract(ppt1d, ponto[1])
        p4 = melt(p4)
        p4 = p1[,-1]

        pptdf = rbind(p1,p2,p3,p4)

        colnames(pptdf) = c("Date", "Prec")
        pptdf$Date = substr(pptdf$Date, 2, 14)

        #Cleannig recalculate erros from precipitation
        pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate

        #First hour of day is not right calculated (deleting)
        pptdf$time = substr(pptdf$Date, 12, 13)
        pptdf = pptdf %>% filter(time != "01")
        pptdf = pptdf[,-3]

        #Summarize ---------------------------------------------------------------------
        #ERA5 Land Wind Speed
        wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
        wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
        wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))

        #ERA5 Land Total Precipitation
        pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
        pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
        pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))

        #Unite and Correlate data ------------------------------------------------------
        df = full_join(wsdf, pptdf, id = "Date")

        df$Prec = df$Prec*1000
        df$year = substr(as.factor(df$Date), 1, 4)
    
    #Block Maxima  ------------------------------------------------------------------
        df3 = df
        df3$date = substr(df3$Date, 1, 10)
        df3$date = as.Date(df3$date, format = "%Y.%m.%d")
        df3 = df3 %>% na.omit()

    #Block by 1 day (is not necessary a moving window)
        d1 = df3 %>%
            group_by(date, m) %>%
            summarize(Prec = max(Prec, na.rm = T))

        d1b = df3 %>%
            group_by(date, m) %>%
            summarize(Wind = max(Wind, na.rm = T))

        d1$Wind = d1b$Wind; d1$window = c("1d")

        #Block by 3 days
        d3 = d1 #df3 will receive the modifications
        d3$window = c("3d")

        #Loop to modify all dataframe
        for (x in 2:length(d3$date)) {
        d3$Prec[x] = move(x, 1, d1$Prec)
        d3$Wind[x] = move(x, 1, d1$Wind)
        }

        #Block by 5 days
        d5 = d1 #df3 will receive the modifications
        d5$window = c("5d")

        #Loop to modify all dataframe
        for (x in 3:length(d5$date)) {
        d5$Prec[x] = move(x, 2, d1$Prec)
        d5$Wind[x] = move(x, 2, d1$Wind)
        }

        #Join data frames
        multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
        multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
        multd[multd == -Inf] <- NA
        multd$x = xmin(ponto[1])
        multd$y = ymin(ponto[1])

        multd1 = multd %>% filter(window == "1d")
        multd3 = multd %>% filter(window == "3d")
        multd5 = multd %>% filter(window == "5d")

    t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
    t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
    t5 = taildep(multd5$Prec, multd5$Wind, 0.9)

    multd1$CHI = t1[[1]]
    multd3$CHI = t3[[1]]
    multd5$CHI = t5[[1]]

    multdfull = rbind(multd1, multd3, multd5)
    multdfull$degrees = pix

    for (z in 2:length(ponto)) {
        print(length(ponto) - z)
    
    #Wind Speed
        w1 = raster::extract(ws1a, ponto[z])
        w1 = melt(w1)
        w1 = w1[,-1]

        w2 = raster::extract(ws1b, ponto[z])
        w2 = melt(w2)
        w2 = w2[,-1]

        w3 = raster::extract(ws1c, ponto[z])
        w3 = melt(w3)
        w3 = w3[,-1]

        w4 = raster::extract(ws1d, ponto[z])
        w4 = melt(w4)
        w4 = w1[,-1]

        wsdf = rbind(w1,w2,w3,w4)

        colnames(wsdf) = c("Date", "Wind")
        wsdf$Date = substr(wsdf$Date, 2, 14)

        #Precipitation
        p1 = raster::extract(ppt1a, ponto[z])
        p1 = melt(p1)
        p1 = p1[,-1]

        p2 = raster::extract(ppt1b, ponto[z])
        p2 = melt(p2)
        p2 = p2[,-1]

        p3 = raster::extract(ppt1c, ponto[z])
        p3 = melt(p3)
        p3 = p3[,-1]

        p4 = raster::extract(ppt1d, ponto[z])
        p4 = melt(p4)
        p4 = p1[,-1]

        pptdf = rbind(p1,p2,p3,p4)

        colnames(pptdf) = c("Date", "Prec")
        pptdf$Date = substr(pptdf$Date, 2, 14)

        #Cleannig recalculate erros from precipitation
        pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate

        #First hour of day is not right calculated (deleting)
        pptdf$time = substr(pptdf$Date, 12, 13)
        pptdf = pptdf %>% filter(time != "01")
        pptdf = pptdf[,-3]

        #Summarize ---------------------------------------------------------------------
        #ERA5 Land Wind Speed
        wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
        wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
        wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))

        #ERA5 Land Total Precipitation
        pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
        pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
        pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))

        #Unite and Correlate data ------------------------------------------------------
        df = full_join(wsdf, pptdf, id = "Date")

        df$Prec = df$Prec*1000
        df$year = substr(as.factor(df$Date), 1, 4)
    
    #Block Maxima  ------------------------------------------------------------------
        df3 = df
        df3$date = substr(df3$Date, 1, 10)
        df3$date = as.Date(df3$date, format = "%Y.%m.%d")
        df3 = df3 %>% na.omit()

    #Block by 1 day (is not necessary a moving window)
        d1 = df3 %>%
            group_by(date, m) %>%
            summarize(Prec = max(Prec, na.rm = T))

        d1b = df3 %>%
            group_by(date, m) %>%
            summarize(Wind = max(Wind, na.rm = T))

        d1$Wind = d1b$Wind; d1$window = c("1d")

        #Block by 3 days
        d3 = d1 #df3 will receive the modifications
        d3$window = c("3d")

        #Loop to modify all dataframe
        for (x in 2:length(d3$date)) {
        d3$Prec[x] = move(x, 1, d1$Prec)
        d3$Wind[x] = move(x, 1, d1$Wind)
        }

        #Block by 5 days
        d5 = d1 #df3 will receive the modifications
        d5$window = c("5d")

        #Loop to modify all dataframe
        for (x in 3:length(d5$date)) {
        d5$Prec[x] = move(x, 2, d1$Prec)
        d5$Wind[x] = move(x, 2, d1$Wind)
        }

        #Join data frames
        multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
        multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
        multd[multd == -Inf] <- NA
        multd$x = xmin(ponto[z])
        multd$y = ymin(ponto[z])

        multd1 = multd %>% filter(window == "1d")
        multd3 = multd %>% filter(window == "3d")
        multd5 = multd %>% filter(window == "5d")

    t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
    t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
    t5 = taildep(multd5$Prec, multd5$Wind, 0.9)

    multd1$CHI = t1[[1]]
    multd3$CHI = t3[[1]]
    multd5$CHI = t5[[1]]

    multdfull2 = rbind(multd1, multd3, multd5)
    multdfull2$degrees = pix

    multdfull = rbind(multdfull, multdfull2)
    }
    return(multdfull)
}

pix3 = function(pix){
  #Wind Speed
  w1 = raster::extract(ws3a, ponto[1])
  w1 = melt(w1)
  w1 = w1[,-1]
  
  w2 = raster::extract(ws3b, ponto[1])
  w2 = melt(w2)
  w2 = w2[,-1]
  
  w3 = raster::extract(ws3c, ponto[1])
  w3 = melt(w3)
  w3 = w3[,-1]
  
  w4 = raster::extract(ws3d, ponto[1])
  w4 = melt(w4)
  w4 = w1[,-1]
  
  wsdf = rbind(w1,w2,w3,w4)
  
  colnames(wsdf) = c("Date", "Wind")
  wsdf$Date = substr(wsdf$Date, 2, 14)
  
  #Precipitation
  p1 = raster::extract(ppt3a, ponto[1])
  p1 = melt(p1)
  p1 = p1[,-1]
  
  p2 = raster::extract(ppt3b, ponto[1])
  p2 = melt(p2)
  p2 = p2[,-1]
  
  p3 = raster::extract(ppt3c, ponto[1])
  p3 = melt(p3)
  p3 = p3[,-1]
  
  p4 = raster::extract(ppt3d, ponto[1])
  p4 = melt(p4)
  p4 = p1[,-1]
  
  pptdf = rbind(p1,p2,p3,p4)
  
  colnames(pptdf) = c("Date", "Prec")
  pptdf$Date = substr(pptdf$Date, 2, 14)
  
  #Cleannig recalculate erros from precipitation
  pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate
  
  #First hour of day is not right calculated (deleting)
  pptdf$time = substr(pptdf$Date, 12, 13)
  pptdf = pptdf %>% filter(time != "01")
  pptdf = pptdf[,-3]
  
  #Summarize ---------------------------------------------------------------------
  #ERA5 Land Wind Speed
  wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
  wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
  wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))
  
  #ERA5 Land Total Precipitation
  pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
  pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
  pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))
  
  #Unite and Correlate data ------------------------------------------------------
  df = full_join(wsdf, pptdf, id = "Date")
  
  df$Prec = df$Prec*1000
  df$year = substr(as.factor(df$Date), 1, 4)
  
  #Block Maxima  ------------------------------------------------------------------
  df3 = df
  df3$date = substr(df3$Date, 1, 10)
  df3$date = as.Date(df3$date, format = "%Y.%m.%d")
  df3 = df3 %>% na.omit()
  
  #Block by 1 day (is not necessary a moving window)
  d1 = df3 %>%
    group_by(date, m) %>%
    summarize(Prec = max(Prec, na.rm = T))
  
  d1b = df3 %>%
    group_by(date, m) %>%
    summarize(Wind = max(Wind, na.rm = T))
  
  d1$Wind = d1b$Wind; d1$window = c("1d")
  
  #Block by 3 days
  d3 = d1 #df3 will receive the modifications
  d3$window = c("3d")
  
  #Loop to modify all dataframe
  for (x in 2:length(d3$date)) {
    d3$Prec[x] = move(x, 1, d1$Prec)
    d3$Wind[x] = move(x, 1, d1$Wind)
  }
  
  #Block by 5 days
  d5 = d1 #df3 will receive the modifications
  d5$window = c("5d")
  
  #Loop to modify all dataframe
  for (x in 3:length(d5$date)) {
    d5$Prec[x] = move(x, 2, d1$Prec)
    d5$Wind[x] = move(x, 2, d1$Wind)
  }
  
  #Join data frames
  multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
  multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
  multd[multd == -Inf] <- NA
  multd$x = xmin(ponto[1])
  multd$y = ymin(ponto[1])
  
  multd1 = multd %>% filter(window == "1d")
  multd3 = multd %>% filter(window == "3d")
  multd5 = multd %>% filter(window == "5d")
  
  t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
  t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
  t5 = taildep(multd5$Prec, multd5$Wind, 0.9)
  
  multd1$CHI = t1[[1]]
  multd3$CHI = t3[[1]]
  multd5$CHI = t5[[1]]
  
  multdfull = rbind(multd1, multd3, multd5)
  multdfull$degrees = pix
  
  for (z in 2:length(ponto)) {
    print(length(ponto) - z)
    
    #Wind Speed
    w1 = raster::extract(ws3a, ponto[z])
    w1 = melt(w1)
    w1 = w1[,-1]
    
    w2 = raster::extract(ws3b, ponto[z])
    w2 = melt(w2)
    w2 = w2[,-1]
    
    w3 = raster::extract(ws3c, ponto[z])
    w3 = melt(w3)
    w3 = w3[,-1]
    
    w4 = raster::extract(ws3d, ponto[z])
    w4 = melt(w4)
    w4 = w1[,-1]
    
    wsdf = rbind(w1,w2,w3,w4)
    
    colnames(wsdf) = c("Date", "Wind")
    wsdf$Date = substr(wsdf$Date, 2, 14)
    
    #Precipitation
    p1 = raster::extract(ppt3a, ponto[z])
    p1 = melt(p1)
    p1 = p1[,-1]
    
    p2 = raster::extract(ppt3b, ponto[z])
    p2 = melt(p2)
    p2 = p2[,-1]
    
    p3 = raster::extract(ppt3c, ponto[z])
    p3 = melt(p3)
    p3 = p3[,-1]
    
    p4 = raster::extract(ppt3d, ponto[z])
    p4 = melt(p4)
    p4 = p1[,-1]
    
    pptdf = rbind(p1,p2,p3,p4)
    
    colnames(pptdf) = c("Date", "Prec")
    pptdf$Date = substr(pptdf$Date, 2, 14)
    
    #Cleannig recalculate erros from precipitation
    pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate
    
    #First hour of day is not right calculated (deleting)
    pptdf$time = substr(pptdf$Date, 12, 13)
    pptdf = pptdf %>% filter(time != "01")
    pptdf = pptdf[,-3]
    
    #Summarize ---------------------------------------------------------------------
    #ERA5 Land Wind Speed
    wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
    wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
    wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))
    
    #ERA5 Land Total Precipitation
    pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
    pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
    pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))
    
    #Unite and Correlate data ------------------------------------------------------
    df = full_join(wsdf, pptdf, id = "Date")
    
    df$Prec = df$Prec*1000
    df$year = substr(as.factor(df$Date), 1, 4)
    
    #Block Maxima  ------------------------------------------------------------------
    df3 = df
    df3$date = substr(df3$Date, 1, 10)
    df3$date = as.Date(df3$date, format = "%Y.%m.%d")
    df3 = df3 %>% na.omit()
    
    #Block by 1 day (is not necessary a moving window)
    d1 = df3 %>%
      group_by(date, m) %>%
      summarize(Prec = max(Prec, na.rm = T))
    
    d1b = df3 %>%
      group_by(date, m) %>%
      summarize(Wind = max(Wind, na.rm = T))
    
    d1$Wind = d1b$Wind; d1$window = c("1d")
    
    #Block by 3 days
    d3 = d1 #df3 will receive the modifications
    d3$window = c("3d")
    
    #Loop to modify all dataframe
    for (x in 2:length(d3$date)) {
      d3$Prec[x] = move(x, 1, d1$Prec)
      d3$Wind[x] = move(x, 1, d1$Wind)
    }
    
    #Block by 5 days
    d5 = d1 #df3 will receive the modifications
    d5$window = c("5d")
    
    #Loop to modify all dataframe
    for (x in 3:length(d5$date)) {
      d5$Prec[x] = move(x, 2, d1$Prec)
      d5$Wind[x] = move(x, 2, d1$Wind)
    }
    
    #Join data frames
    multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
    multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
    multd[multd == -Inf] <- NA
    multd$x = xmin(ponto[z])
    multd$y = ymin(ponto[z])
    
    multd1 = multd %>% filter(window == "1d")
    multd3 = multd %>% filter(window == "3d")
    multd5 = multd %>% filter(window == "5d")
    
    t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
    t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
    t5 = taildep(multd5$Prec, multd5$Wind, 0.9)
    
    multd1$CHI = t1[[1]]
    multd3$CHI = t3[[1]]
    multd5$CHI = t5[[1]]
    
    multdfull2 = rbind(multd1, multd3, multd5)
    multdfull2$degrees = pix
    
    multdfull = rbind(multdfull, multdfull2)
  }
  return(multdfull)
}

pix5 = function(pix){
  #Wind Speed
  w1 = raster::extract(ws5a, ponto[1])
  w1 = melt(w1)
  w1 = w1[,-1]
  
  w2 = raster::extract(ws5b, ponto[1])
  w2 = melt(w2)
  w2 = w2[,-1]
  
  w3 = raster::extract(ws5c, ponto[1])
  w3 = melt(w3)
  w3 = w3[,-1]
  
  w4 = raster::extract(ws5d, ponto[1])
  w4 = melt(w4)
  w4 = w1[,-1]
  
  wsdf = rbind(w1,w2,w3,w4)
  
  colnames(wsdf) = c("Date", "Wind")
  wsdf$Date = substr(wsdf$Date, 2, 14)
  
  #Precipitation
  p1 = raster::extract(ppt5a, ponto[1])
  p1 = melt(p1)
  p1 = p1[,-1]
  
  p2 = raster::extract(ppt5b, ponto[1])
  p2 = melt(p2)
  p2 = p2[,-1]
  
  p3 = raster::extract(ppt5c, ponto[1])
  p3 = melt(p3)
  p3 = p3[,-1]
  
  p4 = raster::extract(ppt5d, ponto[1])
  p4 = melt(p4)
  p4 = p1[,-1]
  
  pptdf = rbind(p1,p2,p3,p4)
  
  colnames(pptdf) = c("Date", "Prec")
  pptdf$Date = substr(pptdf$Date, 2, 14)
  
  #Cleannig recalculate erros from precipitation
  pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate
  
  #First hour of day is not right calculated (deleting)
  pptdf$time = substr(pptdf$Date, 12, 13)
  pptdf = pptdf %>% filter(time != "01")
  pptdf = pptdf[,-3]
  
  #Summarize ---------------------------------------------------------------------
  #ERA5 Land Wind Speed
  wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
  wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
  wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))
  
  #ERA5 Land Total Precipitation
  pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
  pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
  pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))
  
  #Unite and Correlate data ------------------------------------------------------
  df = full_join(wsdf, pptdf, id = "Date")
  
  df$Prec = df$Prec*1000
  df$year = substr(as.factor(df$Date), 1, 4)
  
  #Block Maxima  ------------------------------------------------------------------
  df3 = df
  df3$date = substr(df3$Date, 1, 10)
  df3$date = as.Date(df3$date, format = "%Y.%m.%d")
  df3 = df3 %>% na.omit()
  
  #Block by 1 day (is not necessary a moving window)
  d1 = df3 %>%
    group_by(date, m) %>%
    summarize(Prec = max(Prec, na.rm = T))
  
  d1b = df3 %>%
    group_by(date, m) %>%
    summarize(Wind = max(Wind, na.rm = T))
  
  d1$Wind = d1b$Wind; d1$window = c("1d")
  
  #Block by 3 days
  d3 = d1 #df3 will receive the modifications
  d3$window = c("3d")
  
  #Loop to modify all dataframe
  for (x in 2:length(d3$date)) {
    d3$Prec[x] = move(x, 1, d1$Prec)
    d3$Wind[x] = move(x, 1, d1$Wind)
  }
  
  #Block by 5 days
  d5 = d1 #df3 will receive the modifications
  d5$window = c("5d")
  
  #Loop to modify all dataframe
  for (x in 3:length(d5$date)) {
    d5$Prec[x] = move(x, 2, d1$Prec)
    d5$Wind[x] = move(x, 2, d1$Wind)
  }
  
  #Join data frames
  multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
  multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
  multd[multd == -Inf] <- NA
  multd$x = xmin(ponto[1])
  multd$y = ymin(ponto[1])
  
  multd1 = multd %>% filter(window == "1d")
  multd3 = multd %>% filter(window == "3d")
  multd5 = multd %>% filter(window == "5d")
  
  t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
  t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
  t5 = taildep(multd5$Prec, multd5$Wind, 0.9)
  
  multd1$CHI = t1[[1]]
  multd3$CHI = t3[[1]]
  multd5$CHI = t5[[1]]
  
  multdfull = rbind(multd1, multd3, multd5)
  multdfull$degrees = pix
  
  for (z in 2:length(ponto)) {
    print(length(ponto) - z)
    
    #Wind Speed
    w1 = raster::extract(ws5a, ponto[z])
    w1 = melt(w1)
    w1 = w1[,-1]
    
    w2 = raster::extract(ws5b, ponto[z])
    w2 = melt(w2)
    w2 = w2[,-1]
    
    w3 = raster::extract(ws5c, ponto[z])
    w3 = melt(w3)
    w3 = w3[,-1]
    
    w4 = raster::extract(ws5d, ponto[z])
    w4 = melt(w4)
    w4 = w1[,-1]
    
    wsdf = rbind(w1,w2,w3,w4)
    
    colnames(wsdf) = c("Date", "Wind")
    wsdf$Date = substr(wsdf$Date, 2, 14)
    
    #Precipitation
    p1 = raster::extract(ppt5a, ponto[z])
    p1 = melt(p1)
    p1 = p1[,-1]
    
    p2 = raster::extract(ppt5b, ponto[z])
    p2 = melt(p2)
    p2 = p2[,-1]
    
    p3 = raster::extract(ppt5c, ponto[z])
    p3 = melt(p3)
    p3 = p3[,-1]
    
    p4 = raster::extract(ppt5d, ponto[z])
    p4 = melt(p4)
    p4 = p1[,-1]
    
    pptdf = rbind(p1,p2,p3,p4)
    
    colnames(pptdf) = c("Date", "Prec")
    pptdf$Date = substr(pptdf$Date, 2, 14)
    
    #Cleannig recalculate erros from precipitation
    pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate
    
    #First hour of day is not right calculated (deleting)
    pptdf$time = substr(pptdf$Date, 12, 13)
    pptdf = pptdf %>% filter(time != "01")
    pptdf = pptdf[,-3]
    
    #Summarize ---------------------------------------------------------------------
    #ERA5 Land Wind Speed
    wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
    wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
    wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))
    
    #ERA5 Land Total Precipitation
    pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
    pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
    pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))
    
    #Unite and Correlate data ------------------------------------------------------
    df = full_join(wsdf, pptdf, id = "Date")
    
    df$Prec = df$Prec*1000
    df$year = substr(as.factor(df$Date), 1, 4)
    
    #Block Maxima  ------------------------------------------------------------------
    df3 = df
    df3$date = substr(df3$Date, 1, 10)
    df3$date = as.Date(df3$date, format = "%Y.%m.%d")
    df3 = df3 %>% na.omit()
    
    #Block by 1 day (is not necessary a moving window)
    d1 = df3 %>%
      group_by(date, m) %>%
      summarize(Prec = max(Prec, na.rm = T))
    
    d1b = df3 %>%
      group_by(date, m) %>%
      summarize(Wind = max(Wind, na.rm = T))
    
    d1$Wind = d1b$Wind; d1$window = c("1d")
    
    #Block by 3 days
    d3 = d1 #df3 will receive the modifications
    d3$window = c("3d")
    
    #Loop to modify all dataframe
    for (x in 2:length(d3$date)) {
      d3$Prec[x] = move(x, 1, d1$Prec)
      d3$Wind[x] = move(x, 1, d1$Wind)
    }
    
    #Block by 5 days
    d5 = d1 #df3 will receive the modifications
    d5$window = c("5d")
    
    #Loop to modify all dataframe
    for (x in 3:length(d5$date)) {
      d5$Prec[x] = move(x, 2, d1$Prec)
      d5$Wind[x] = move(x, 2, d1$Wind)
    }
    
    #Join data frames
    multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
    multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
    multd[multd == -Inf] <- NA
    multd$x = xmin(ponto[z])
    multd$y = ymin(ponto[z])
    
    multd1 = multd %>% filter(window == "1d")
    multd3 = multd %>% filter(window == "3d")
    multd5 = multd %>% filter(window == "5d")
    
    t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
    t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
    t5 = taildep(multd5$Prec, multd5$Wind, 0.9)
    
    multd1$CHI = t1[[1]]
    multd3$CHI = t3[[1]]
    multd5$CHI = t5[[1]]
    
    multdfull2 = rbind(multd1, multd3, multd5)
    multdfull2$degrees = pix
    
    multdfull = rbind(multdfull, multdfull2)
  }
  return(multdfull)
}

pix7 = function(pix){
  #Wind Speed
  w1 = raster::extract(ws7a, ponto[1])
  w1 = melt(w1)
  w1 = w1[,-1]
  
  w2 = raster::extract(ws7b, ponto[1])
  w2 = melt(w2)
  w2 = w2[,-1]
  
  w3 = raster::extract(ws7c, ponto[1])
  w3 = melt(w3)
  w3 = w3[,-1]
  
  w4 = raster::extract(ws7d, ponto[1])
  w4 = melt(w4)
  w4 = w1[,-1]
  
  wsdf = rbind(w1,w2,w3,w4)
  
  colnames(wsdf) = c("Date", "Wind")
  wsdf$Date = substr(wsdf$Date, 2, 14)
  
  #Precipitation
  p1 = raster::extract(ppt7a, ponto[1])
  p1 = melt(p1)
  p1 = p1[,-1]
  
  p2 = raster::extract(ppt7b, ponto[1])
  p2 = melt(p2)
  p2 = p2[,-1]
  
  p3 = raster::extract(ppt7c, ponto[1])
  p3 = melt(p3)
  p3 = p3[,-1]
  
  p4 = raster::extract(ppt7d, ponto[1])
  p4 = melt(p4)
  p4 = p1[,-1]
  
  pptdf = rbind(p1,p2,p3,p4)
  
  colnames(pptdf) = c("Date", "Prec")
  pptdf$Date = substr(pptdf$Date, 2, 14)
  
  #Cleannig recalculate erros from precipitation
  pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate
  
  #First hour of day is not right calculated (deleting)
  pptdf$time = substr(pptdf$Date, 12, 13)
  pptdf = pptdf %>% filter(time != "01")
  pptdf = pptdf[,-3]
  
  #Summarize ---------------------------------------------------------------------
  #ERA5 Land Wind Speed
  wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
  wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
  wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))
  
  #ERA5 Land Total Precipitation
  pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
  pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
  pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))
  
  #Unite and Correlate data ------------------------------------------------------
  df = full_join(wsdf, pptdf, id = "Date")
  
  df$Prec = df$Prec*1000
  df$year = substr(as.factor(df$Date), 1, 4)
  
  #Block Maxima  ------------------------------------------------------------------
  df3 = df
  df3$date = substr(df3$Date, 1, 10)
  df3$date = as.Date(df3$date, format = "%Y.%m.%d")
  df3 = df3 %>% na.omit()
  
  #Block by 1 day (is not necessary a moving window)
  d1 = df3 %>%
    group_by(date, m) %>%
    summarize(Prec = max(Prec, na.rm = T))
  
  d1b = df3 %>%
    group_by(date, m) %>%
    summarize(Wind = max(Wind, na.rm = T))
  
  d1$Wind = d1b$Wind; d1$window = c("1d")
  
  #Block by 3 days
  d3 = d1 #df3 will receive the modifications
  d3$window = c("3d")
  
  #Loop to modify all dataframe
  for (x in 2:length(d3$date)) {
    d3$Prec[x] = move(x, 1, d1$Prec)
    d3$Wind[x] = move(x, 1, d1$Wind)
  }
  
  #Block by 5 days
  d5 = d1 #df3 will receive the modifications
  d5$window = c("5d")
  
  #Loop to modify all dataframe
  for (x in 3:length(d5$date)) {
    d5$Prec[x] = move(x, 2, d1$Prec)
    d5$Wind[x] = move(x, 2, d1$Wind)
  }
  
  #Join data frames
  multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
  multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
  multd[multd == -Inf] <- NA
  multd$x = xmin(ponto[1])
  multd$y = ymin(ponto[1])
  
  multd1 = multd %>% filter(window == "1d")
  multd3 = multd %>% filter(window == "3d")
  multd5 = multd %>% filter(window == "5d")
  
  t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
  t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
  t5 = taildep(multd5$Prec, multd5$Wind, 0.9)
  
  multd1$CHI = t1[[1]]
  multd3$CHI = t3[[1]]
  multd5$CHI = t5[[1]]
  
  multdfull = rbind(multd1, multd3, multd5)
  multdfull$degrees = pix
  
  for (z in 2:length(ponto)) {
    print(length(ponto) - z)
    
    #Wind Speed
    w1 = raster::extract(ws7a, ponto[z])
    w1 = melt(w1)
    w1 = w1[,-1]
    
    w2 = raster::extract(ws7b, ponto[z])
    w2 = melt(w2)
    w2 = w2[,-1]
    
    w3 = raster::extract(ws7c, ponto[z])
    w3 = melt(w3)
    w3 = w3[,-1]
    
    w4 = raster::extract(ws7d, ponto[z])
    w4 = melt(w4)
    w4 = w1[,-1]
    
    wsdf = rbind(w1,w2,w3,w4)
    
    colnames(wsdf) = c("Date", "Wind")
    wsdf$Date = substr(wsdf$Date, 2, 14)
    
    #Precipitation
    p1 = raster::extract(ppt7a, ponto[z])
    p1 = melt(p1)
    p1 = p1[,-1]
    
    p2 = raster::extract(ppt7b, ponto[z])
    p2 = melt(p2)
    p2 = p2[,-1]
    
    p3 = raster::extract(ppt7c, ponto[z])
    p3 = melt(p3)
    p3 = p3[,-1]
    
    p4 = raster::extract(ppt7d, ponto[z])
    p4 = melt(p4)
    p4 = p1[,-1]
    
    pptdf = rbind(p1,p2,p3,p4)
    
    colnames(pptdf) = c("Date", "Prec")
    pptdf$Date = substr(pptdf$Date, 2, 14)
    
    #Cleannig recalculate erros from precipitation
    pptdf = pptdf[c(-1,-2),] #First and second data from stack is not accurate
    
    #First hour of day is not right calculated (deleting)
    pptdf$time = substr(pptdf$Date, 12, 13)
    pptdf = pptdf %>% filter(time != "01")
    pptdf = pptdf[,-3]
    
    #Summarize ---------------------------------------------------------------------
    #ERA5 Land Wind Speed
    wsdf$y = as.numeric(substr(wsdf$Date, 1, 4))
    wsdf$m = as.numeric(substr(wsdf$Date, 6, 7))
    wsdf$d = as.numeric(substr(wsdf$Date, 9, 10))
    
    #ERA5 Land Total Precipitation
    pptdf$y = as.numeric(substr(pptdf$Date, 1, 4))
    pptdf$m = as.numeric(substr(pptdf$Date, 6, 7))
    pptdf$d = as.numeric(substr(pptdf$Date, 9, 10))
    
    #Unite and Correlate data ------------------------------------------------------
    df = full_join(wsdf, pptdf, id = "Date")
    
    df$Prec = df$Prec*1000
    df$year = substr(as.factor(df$Date), 1, 4)
    
    #Block Maxima  ------------------------------------------------------------------
    df3 = df
    df3$date = substr(df3$Date, 1, 10)
    df3$date = as.Date(df3$date, format = "%Y.%m.%d")
    df3 = df3 %>% na.omit()
    
    #Block by 1 day (is not necessary a moving window)
    d1 = df3 %>%
      group_by(date, m) %>%
      summarize(Prec = max(Prec, na.rm = T))
    
    d1b = df3 %>%
      group_by(date, m) %>%
      summarize(Wind = max(Wind, na.rm = T))
    
    d1$Wind = d1b$Wind; d1$window = c("1d")
    
    #Block by 3 days
    d3 = d1 #df3 will receive the modifications
    d3$window = c("3d")
    
    #Loop to modify all dataframe
    for (x in 2:length(d3$date)) {
      d3$Prec[x] = move(x, 1, d1$Prec)
      d3$Wind[x] = move(x, 1, d1$Wind)
    }
    
    #Block by 5 days
    d5 = d1 #df3 will receive the modifications
    d5$window = c("5d")
    
    #Loop to modify all dataframe
    for (x in 3:length(d5$date)) {
      d5$Prec[x] = move(x, 2, d1$Prec)
      d5$Wind[x] = move(x, 2, d1$Wind)
    }
    
    #Join data frames
    multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
    multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
    multd[multd == -Inf] <- NA
    multd$x = xmin(ponto[z])
    multd$y = ymin(ponto[z])
    
    multd1 = multd %>% filter(window == "1d")
    multd3 = multd %>% filter(window == "3d")
    multd5 = multd %>% filter(window == "5d")
    
    t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
    t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
    t5 = taildep(multd5$Prec, multd5$Wind, 0.9)
    
    multd1$CHI = t1[[1]]
    multd3$CHI = t3[[1]]
    multd5$CHI = t5[[1]]
    
    multdfull2 = rbind(multd1, multd3, multd5)
    multdfull2$degrees = pix
    
    multdfull = rbind(multdfull, multdfull2)
  }
  return(multdfull)
}

p1 = pix1("0.25°"); print("pix1 done!")
p3 = pix3("0.75°"); print("pix3 done!")
p5 = pix5("1.25°"); print("pix5 done!")
p7 = pix7("1.75°"); print("pix7 done!")

setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
write.csv(p1, "ERA5_Block_1pixel_1979_2020.csv", row.names = F)
write.csv(p3, "ERA5_Block_3pixel_1979_2020.csv", row.names = F)
write.csv(p5, "ERA5_Block_5pixel_1979_2020.csv", row.names = F)
write.csv(p7, "ERA5_Block_7pixel_1979_2020.csv", row.names = F)

df = rbind(p1, p3, p5, p7)

#Creating classes ----------------------------------------------------------------------------
df2 = df %>%
  group_by(x, y, date) %>%
  #summarize(CHI = max(CHI)) %>%
  slice(which.max(CHI))%>%
  unite("Class", window, degrees, sep = ", ") %>%
  ungroup()

#df2 = df2 %>%
#    filter(date != "1979-01-01") %>%
#    filter(date != "1979-01-02")
df2 = df2[,-2]

setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
write.csv(df2, "Optimal_ERA5_Block_1979_2020.csv", row.names = F)
write.csv(df, "ALL_ERA5_Block_combination_1979_2020.csv", row.names = F)

df3 = df2 %>%
    group_by(x, y, Class) %>%
    summarize(CHI = mean(CHI))

c_class = ggplot(df3)+
      geom_raster(aes(x, y, fill = Class))+
      coord_fixed()+
      #geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_manual(values = c('#6baed6','#3182bd','#08519c','#bae4b3','#74c476','#238b45','#006d2c','#fcae91','#fb6a4a','#cb181d','#a50f15'))+
      labs(x=NULL, y=NULL, title = "Optimal Windowns")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_1979_2020.png", plot = c_class,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
  width = 15, height = 20, units = "cm", dpi = 300)




