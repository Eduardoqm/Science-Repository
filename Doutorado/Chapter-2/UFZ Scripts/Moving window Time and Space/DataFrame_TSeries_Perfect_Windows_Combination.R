#---------------------------------------------------------------------------
# Data Frame from optimal Temp/Space window combination
#
# Eduardo Q Marques 16-08-2022
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
setwd("/home/queirozm/eqm_eth_ufz/Data")
ws1 = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-2012_2020.nc") #Wind Gust from ERA5 Single Levels
ws3 = brick("ERA5_SL/ERA_SL_WG_block_3pixels.nc") #Wind Gust Bloked
ws5 = brick("ERA5_SL/ERA_SL_WG_block_5pixels.nc") #Wind Gust Bloked
ws7 = brick("ERA5_SL/ERA_SL_WG_block_7pixels.nc") #Wind Gust Bloked

ws3@data@names = ws1@data@names #Names em ws1 stack
ws5@data@names = ws1@data@names
ws7@data@names = ws1@data@names

ppt1 = brick("ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_2012_2020.nc") #Recurrent Precipitation from ERA5 Land
ppt3 = brick("ERA5_land/ERA_Land_Prec_block_3pixels.nc") #Precipitation Bloked
ppt5 = brick("ERA5_land/ERA_Land_Prec_block_5pixels.nc") #Precipitation Bloked
ppt7 = brick("ERA5_land/ERA_Land_Prec_block_7pixels.nc") #Precipitation Bloked

tp = brick("ERA5_land/Total_Precipitation_ERA5-Land_2012_2020.nc") #Total Precipitation from ERA5 Land
ppt1@data@names = tp@data@names #Names em tp stack
ppt3@data@names = tp@data@names
ppt5@data@names = tp@data@names
ppt7@data@names = tp@data@names

library(rgdal)
pont = readOGR(dsn = "Shapes", layer = "Area1_ponto") #Area-1 point
xing = readOGR(dsn = "Shapes", layer = "Xingu_MT") #Area-1 point

#Crop data to Xingu limits -----------------------------------------------------
ws1 = crop(ws1, xing); ppt1 = crop(ppt1, xing)
ws3 = crop(ws3, xing); ppt3 = crop(ppt3, xing)
ws5 = crop(ws5, xing); ppt5 = crop(ppt5, xing)
ws7 = crop(ws7, xing); ppt7 = crop(ppt7, xing)

#Create points to every pixel --------------------------------------------------
coord = xyFromCell(ppt1, 1:ncell(ppt1)) #Extract coordinates by each cell
ponto = SpatialPoints(coord) #Convert coordinates in points to after make buffer
plotx(ponto)

#Calculating CHI and otimal windows (converting to dataframes)
library(tidyverse)
library(reshape2)
library(extRemes)

blz = function(ws_img, ppt_img, pix){
        #Wind Speed
        wsdf <- raster::extract(ws_img, ponto[1])
        wsdf = melt(wsdf)
        wsdf = wsdf[,-1]

        colnames(wsdf) = c("Date", "Wind")
        wsdf$Date = substr(wsdf$Date, 2, 14)

        #Precipitation
        pptdf <- raster::extract(ppt_img, ponto[1])
        pptdf = melt(pptdf)
        pptdf = pptdf[,-1]

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
        wsdf <- raster::extract(ws_img, ponto[z])
        wsdf = melt(wsdf)
        wsdf = wsdf[,-1]

        colnames(wsdf) = c("Date", "Wind")
        wsdf$Date = substr(wsdf$Date, 2, 14)

        #Precipitation
        pptdf <- raster::extract(ppt_img, ponto[z])
        pptdf = melt(pptdf)
        pptdf = pptdf[,-1]

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

p1 = blz(ws1, ppt1, "0.25°")
p3 = blz(ws3, ppt3, "0.75°")
p5 = blz(ws5, ppt5, "1.25°")
p7 = blz(ws7, ppt7, "1.75°")

df = rbind(p1, p3, p5, p7)

#Creating classes ----------------------------------------------------------------------------
df2 = df %>%
  group_by(x, y, date) %>%
  #summarize(CHI = max(CHI)) %>%
  slice(which.max(CHI))%>%
  unite("Class", window, degrees, sep = ", ") %>%
  ungroup()

df2 = df2 %>%
    filter(date != "2012-01-01") %>%
    filter(date != "2012-01-02")
df2 = df2[,-2]

setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
#write.csv(df2, "Optimal_ERA5_Block.csv", row.names = F)

df3 = df2 %>%
    group_by(x, y, Class) %>%
    summarize(CHI = mean(CHI))

c_class = ggplot(df3)+
      geom_raster(aes(x, y, fill = Class))+
      coord_fixed()+
      #geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
      scale_fill_manual(values = c('#6baed6','#3182bd','#08519c','#bae4b3','#74c476','#238b45','#006d2c','#fcae91','#fb6a4a','#cb181d','#a50f15'))+
      labs(x=NULL, y=NULL, title = "Optimal Windowns 2")+
      theme_minimal()+
      theme(text = element_text(size = 14))

ggsave(filename = "Xingu_Optimal_CHIMap_classes_2.png", plot = c_class,
  path = "/home/queirozm/eqm_eth_ufz/Temp_figures/Result_maps",
  width = 15, height = 20, units = "cm", dpi = 300)






































#Wind Speed
    wsdf <- raster::extract(ws1, ponto[1])
    wsdf = melt(wsdf)
    wsdf = wsdf[,-1]

    colnames(wsdf) = c("Date", "Wind")
    wsdf$Date = substr(wsdf$Date, 2, 14)

    #Precipitation
    pptdf <- raster::extract(ppt1, ponto[1])
    pptdf = melt(pptdf)
    pptdf = pptdf[,-1]

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
  multdfull$degrees = "0.25°"

for (z in 2:length(ponto)) {
    print(length(ponto) - z)
  
  #Wind Speed
    wsdf <- raster::extract(ws1, ponto[z])
    wsdf = melt(wsdf)
    wsdf = wsdf[,-1]

    colnames(wsdf) = c("Date", "Wind")
    wsdf$Date = substr(wsdf$Date, 2, 14)

    #Precipitation
    pptdf <- raster::extract(ppt1, ponto[z])
    pptdf = melt(pptdf)
    pptdf = pptdf[,-1]

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
  multdfull2$degrees = "0.25°"

  multdfull = rbind(multdfull, multdfull2)
}

