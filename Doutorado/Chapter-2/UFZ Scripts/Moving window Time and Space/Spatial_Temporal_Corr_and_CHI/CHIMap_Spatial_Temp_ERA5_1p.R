#---------------------------------------------------------------------------
# Map of CHI values from ERA5 Precipitation and Wind Gust (1 pixel) 
#
# Eduardo Q Marques 10-08-2022
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
ws = brick("ERA5_SL/ERA5_Single_Levels-Wind_Gust-2012_2020.nc") #Wind Gust from ERA5 Single Levels

ppt = brick("ERA5_land/Resampled_Recurrent_Prec_ERA5-Land_2012_2020.nc") # Recurrent Precipitation from ERA5 Land
tp = brick("ERA5_land/Total_Precipitation_ERA5-Land_2012_2020.nc") #Total Precipitation from ERA5 Land
ppt@data@names = tp@data@names #Names em ppt stack

library(rgdal)
pont = readOGR(dsn = "Shapes", layer = "Area1_ponto") #Area-1 point
tang = readOGR(dsn = "Shapes", layer = "Tanguro_limites") #Area-1 point
xing = readOGR(dsn = "Shapes", layer = "Xingu_MT") #Area-1 point

#Crop data to Xingu limits -----------------------------------------------------
ws2 = crop(ws, xing)
ppt2 = crop(ppt, xing)

#Plot to verify data -----------------------------------------------------------
#plotx(ws)
#plotx(ppt)
#plotx(ws2)
#plotx(ppt2)

#Create points to every pixel --------------------------------------------------
coord = xyFromCell(ppt2, 1:ncell(ppt2)) #Extract coordinates by each cell
ponto = SpatialPoints(coord) #Convert coordinates in points to after make buffer
plotx(ponto)

#Loop with CHI for each pixel in time series -----------------------------------
#Create a rasters to receive CHI values
xingu1 = ppt2[[1]]; xingu1@data@values = c(0)
xingu3 = ppt2[[1]]; xingu3@data@values = c(0)
xingu5 = ppt2[[1]]; xingu5@data@values = c(0)


library(tidyverse)
library(reshape2)
library(extRemes)

for (z in 1:length(ponto)) {
    print(length(ponto) - z)
  
  #Wind Speed
    wsdf <- raster::extract(ws, ponto[z])
    wsdf = melt(wsdf)
    wsdf = wsdf[,-1]

    colnames(wsdf) = c("Date", "Wind")
    wsdf$Date = substr(wsdf$Date, 2, 14)

    #Precipitation
    pptdf <- raster::extract(ppt, ponto[z])
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

    multd1 = multd %>% filter(window == "1d")
    multd3 = multd %>% filter(window == "3d")
    multd5 = multd %>% filter(window == "5d")

  t1 = taildep(multd1$Prec, multd1$Wind, 0.9)
  t3 = taildep(multd3$Prec, multd3$Wind, 0.9)
  t5 = taildep(multd5$Prec, multd5$Wind, 0.9)

  taild1 = t1[[1]]
  taild3 = t3[[1]]
  taild5 = t5[[1]]
  
  xingu1[z] = taild1
  xingu3[z] = taild3
  xingu5[z] = taild5
}

#Stacking and mask Xingu ----------------------------------------------------------
xingu = stack(xingu1, xingu3, xingu5)
xingu = mask(xingu, xing)
plotx(xingu)

#Beautiful Maps --------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(viridis)

xingudf = na.omit(as.data.frame(xingu, xy = T))
colnames(xingudf) = c("x","y","1d","3d","5d")
xingudf = melt(xingudf, id.vars = c("x", "y"))
colnames(xingudf) = c("x","y","Days","CHI")
write.csv(xingudf, "/home/queirozm/eqm_eth_ufz/Data/DataFrames/CHIs/CHI_Xingu_1p.csv", row.names = F)

pont2 = as.data.frame(pont)
colnames(pont2) = c("id","x","y")

map = ggplot()+
  geom_raster(data = xingudf, aes(x, y, fill = CHI))+
  coord_fixed()+
  geom_point(data = pont2, aes(x, y), col = "white", shape = 2, size = 3, stroke = 2)+
  scale_fill_viridis(option = "plasma", direction = -1, limits=c(0, 1), name = "Chi q = 0.9")+
  labs(x=NULL, y=NULL, title = "Tail Dependence (0.25°)")+
  facet_wrap(~Days, nrow = 1)+
  theme_minimal()+
  theme(text = element_text(size = 14))

  ggsave(filename = "Xingu_CHIMap_1p.png", plot = map,
       path = "/home/queirozm/eqm_eth_ufz/Temp_figures/Result_maps",
       width = 37, height = 20, units = "cm", dpi = 300)