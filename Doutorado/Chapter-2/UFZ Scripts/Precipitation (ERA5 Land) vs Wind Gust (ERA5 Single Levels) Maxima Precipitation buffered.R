#---------------------------------------------------------------------------------------------------
# Precipitation (ERA5 Land) vs Wind Gust (ERA5 Single Levels) Maxima Precipitation buffered
#
# Eduardo Q Marques 23-05-2022
#--------------------------------------------------------------------------------------------------

library(raster)
library(rgdal)
library(sf)
library(ncdf4)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(extRemes)
library(boot)

#rasterOptions(tmpdir='/net/so4/landclim/equeiroz/tmp/') #Temporary folder on server

#Load data ---------------------------------------------------------------------
ws = brick("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/ERA5_Single_Levels/ERA5_SL-Wind_Gust-2012_2020.nc") #Wind Gust from ERA5 Single Levels
ppt = brick("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/ERA5_land/Recurrent_Precipitation_ERA5-Land_2012_2020.nc") # Recurrent Precipitation from ERA5 Land
tp = brick("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/UFZ Data/ERA5_land/Total_Precipitation_ERA5-Land_2012_2020.nc") #Total Precipitation from ERA5 Land

ponto = readOGR(dsn = "~/Data/Shapes", layer = "Area1_ponto") #Area-1 point
tang = readOGR(dsn = "~/Data/Shapes", layer = "Tanguro_limites") #Area-1 point
xing = readOGR(dsn = "~/Data/Shapes", layer = "Xingu_MT") #Area-1 point

#Names em ppt stack
ppt@data@names = tp@data@names


#Plot to verify data -----------------------------------------------------------
plot(ws)
plot(ppt)
plot(ws[[1]])
plot(ppt[[10]])
plot(xing, add = T)
plot(tang, add = T)
plot(ponto, add = T)

buff = buffer(ponto, width=100000) #Buffer 100 km
plot(buff, add = T)

#Extract info from ERA5 with buffer --------------------------------------------
#Wind Speed
wsdf <- raster::extract(ws, buff)
wsdf = melt(wsdf)
wsdf = wsdf %>% 
  group_by(Var2) %>% 
  summarise(value = max(value))

colnames(wsdf) = c("Date", "Wind")
wsdf$Date = substr(wsdf$Date, 2, 14)

#Precipitation
pptdf <- raster::extract(ppt, buff)
pptdf = melt(pptdf)
pptdf = pptdf %>% 
  group_by(Var2) %>% 
  summarise(value = max(value))

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

df2 = df[,c(1,2,6,3,4)]; colnames(df2) = c("date", "ws", "ppt", "year", "month")
df2$month = as.character(df2$month)
df2 = df2 %>% filter(month %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate

df2$ppt = df2$ppt*1000
df2$Date = as.factor(df2$year)

#Plot data
#eqm = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')
eqm = c('#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

rawppt = ggplot(df2, aes(x=ppt, y=ws))+
  geom_point(aes(col = Date), alpha = 0.7, size = 3)+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Precipitation (mm)", y = "Wind Gust (m/s)", title = "A")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  ylim(0, 15)+
  theme(legend.position = c(30, 30)); rawppt

rawppt2 = ggplot(df2, aes(x=ppt, y=ws))+
  geom_point(aes(col = Date), alpha = 0.9, size = 2)+
  geom_smooth(method = "lm", col = "black")+
  labs( x = "Precipitation (mm)", y = NULL, title = "B")+
  stat_cor(show.legend = F)+
  facet_wrap(~year)+
  scale_color_manual(values = eqm)+
  theme_bw()+
  ylim(0, 15)+
  theme(legend.position = c(30, 30)); rawppt2

rawppt3 = ggarrange(rawppt, rawppt2, ncol = 2); rawppt3


#Block Maxima diary precipitation -----------------------------------------------
df3 = df2
df3$date = substr(df3$date, 1, 10)
df3$date = as.Date(df3$date, format = "%Y.%m.%d")
df3 = na.omit(df3)

df3 <- blockmaxxer(df3, blocks = df3$date, which="ppt") #Function only blocking precipitation
#df4 <- blockmaxxer(df3, blocks = df3$date, which="ppt")
#df4b <- blockmaxxer(df3, blocks = df3$date, which="ws") #Function only blocking wind speed
#df4$ws = df4b$ws
#df3 = df4

maxppt = ggplot(df3, aes(x=ppt, y=ws))+
  geom_point(aes(col = Date), alpha = 0.7, size = 3)+
  geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = "Wind Gust (m/s)", title = "A")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30)); maxppt

maxppt2 = ggplot(df3, aes(x=ppt, y=ws))+
  geom_point(aes(col = Date), alpha = 0.7, size = 2)+
  geom_smooth(method = "lm", col = "black")+
  labs( x = "Maximum Precipitation (mm)", y = NULL, title = "B")+
  stat_cor(show.legend = F)+
  scale_color_manual(values = eqm)+
  facet_wrap(~year)+
  theme_bw()+
  theme(legend.position = c(30, 30)); maxppt2

maxppt3 = ggarrange(maxppt, maxppt2, ncol = 2); maxppt3

#img1 = ggarrange(rawppt ,maxppt); img1
#img2 = ggarrange(img1 ,maxppt2, ncol = 1); img2



ggsave(filename = "WG(ERA-SL)-Prec(ERAland)_Buffer.png", plot = maxppt3,
       path = "~/Figures/Correlations", width = 25, height = 12.5, units = "cm", dpi = 300)


#=======================================================================================================================================================================


#Extract tail dependence values and Bootstrapping for all years together -------------
df2 = df3
tq = seq(.05, 1, .01); length(tq)

t1 = taildep(df2$ppt, df2$ws, 0.05)
taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
colnames(taild) = c("quant", "chi", "chibar")

#Chi ---------------------------------------------------------------------------------
#Function to extract Chi data from tail dependence
chifun = function(formula, data, indices) {
  df2 <- data[indices,] # selecting sample with boot 
  fit <- taildep(df2$ppt, df2$ws, 0.05)
  return(fit[[1]])
} 

f1 =c(df2$ppt, df2$ws, 0.05)
chifun(formula = f1, data = df2) #Just a test

#Performing 1000 replications with boot 
output <- boot(data=df2, statistic=chifun, 
               R=1000, formula=f1)

#Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chi = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chi) = c("quant", "value", "low", "upp")

#Loop to do all Chi quantiles
for (z in 2:96) {
  print(tq[[z]])
  chifun = function(formula, data, indices) {
    df2 <- data[indices,]
    fit <- taildep(df2$ppt, df2$ws, (tq[[z]]))
    return(fit[[1]])
  }
  
  f1 =c(df2$ppt, df2$ws, (tq[[z]]))
  
  output <- boot(data=df2, statistic=chifun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chi = rbind(chi, t2)
}


#Chibar ---------------------------------------------------------------------------------
chibarfun = function(formula, data, indices) {
  df2 <- data[indices,] # selecting sample with boot 
  fit <- taildep(df2$ppt, df2$ws, 0.05)
  return(fit[[2]])
} 

f1 =c(df2$ppt, df2$ws, 0.05)
chibarfun(formula = f1, data = df2) #Just a test

#Performing 1000 replications with boot 
output <- boot(data=df2, statistic=chibarfun, 
               R=1000, formula=f1)

#Obtaining a confidence interval of 95%
inter = boot.ci(output, type="perc")

chibar = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
colnames(chibar) = c("quant", "value", "low", "upp")

#Loop to do all Chibar quantiles
for (z in 2:96) {
  print(tq[[z]])
  chibarfun = function(formula, data, indices) {
    df2 <- data[indices,]
    fit <- taildep(df2$ppt, df2$ws, (tq[[z]]))
    return(fit[[2]])
  }
  
  f1 =c(df2$ppt, df2$ws, (tq[[z]]))
  
  output <- boot(data=df2, statistic=chibarfun, 
                 R=1000, formula=f1)
  inter = boot.ci(output, type="perc")
  t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(t2) = c("quant", "value", "low", "upp")
  chibar = rbind(chibar, t2)
}

#Plot results ------------------------------------------------------------------------------
chi_plot = ggplot(chi, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chi")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "#1f78b4")+
  ylim(0, 1)+
  theme_bw(); chi_plot

chibar_plot = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "#1f78b4")+
  ylim(-1, 0.7)+
  theme_bw(); chibar_plot




#=======================================================================================================================================================================




#Extract tail dependence values and Bootstrapping facet by years -----------------------
df2 = df3

#Chi ===================================================================================
mstchi = function(df, yr){
  #Extract tail dependence values and Bootstrapping ------------------------------------
  df2 = df2 %>% filter(year == yr)
  tq = seq(.05, 1, .01); length(tq)
  
  t1 = taildep(df2$ppt, df2$ws, 0.05)
  taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
  colnames(taild) = c("quant", "chi", "chibar")
  
  #Chi ---------------------------------------------------------------------------------
  #Function to extract Chi data from tail dependence
  chifun = function(formula, data, indices) {
    df2 <- data[indices,] # selecting sample with boot 
    fit <- taildep(df2$ppt, df2$ws, 0.05)
    return(fit[[1]])
  } 
  
  f1 =c(df2$ppt, df2$ws, 0.05)
  chifun(formula = f1, data = df2) #Just a test
  
  #Performing 1000 replications with boot 
  output <- boot(data=df2, statistic=chifun, 
                 R=1000, formula=f1)
  
  #Obtaining a confidence interval of 95%
  inter = boot.ci(output, type="perc")
  
  chi = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(chi) = c("quant", "value", "low", "upp")
  
  #Loop to do all Chi quantiles
  for (z in 2:95) {
    print(chi)
    chifun = function(formula, data, indices) {
      df2 <- data[indices,]
      fit <- taildep(df2$ppt, df2$ws, (tq[[z]]))
      return(fit[[1]])
    }
    
    f1 =c(df2$ppt, df2$ws, (tq[[z]]))
    
    output <- boot(data=df2, statistic=chifun, 
                   R=1000, formula=f1)
    inter = boot.ci(output, type="perc")
    t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
    colnames(t2) = c("quant", "value", "low", "upp")
    chi = rbind(chi, t2)
  }
  as.data.frame(chi)
}


chi = mstchi(df2, 2012); chi$Date = c(2012)

for (z in 2013:2020) {
  print(z)
  chix = mstchi(df2, z)
  chix$Date = z
  chi = rbind(chi, chix)
}


#ChiBar ====================================================================================
mstchibar = function(df, yr){
  #Extract tail dependence values and Bootstrapping ------------------------------------
  df2 = df2 %>% filter(year == yr)
  tq = seq(.05, 1, .01); length(tq)
  
  t1 = taildep(df2$ppt, df2$ws, 0.05)
  taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
  colnames(taild) = c("quant", "chi", "chibar")
  
  #Chibar ---------------------------------------------------------------------------------
  chibarfun = function(formula, data, indices) {
    df2 <- data[indices,] # selecting sample with boot 
    fit <- taildep(df2$ppt, df2$ws, 0.05)
    return(fit[[2]])
  } 
  
  f1 =c(df2$ppt, df2$ws, 0.05)
  chibarfun(formula = f1, data = df2) #Just a test
  
  #Performing 1000 replications with boot 
  output <- boot(data=df2, statistic=chibarfun, 
                 R=1000, formula=f1)
  
  #Obtaining a confidence interval of 95%
  inter = boot.ci(output, type="perc")
  
  chibar = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(chibar) = c("quant", "value", "low", "upp")
  
  #Loop to do all Chibar quantiles
  for (z in 2:95) {
    print(chibar)
    chibarfun = function(formula, data, indices) {
      df2 <- data[indices,]
      fit <- taildep(df2$ppt, df2$ws, (tq[[z]]))
      return(fit[[2]])
    }
    
    f1 =c(df2$ppt, df2$ws, (tq[[z]]))
    
    output <- boot(data=df2, statistic=chibarfun, 
                   R=1000, formula=f1)
    inter = boot.ci(output, type="perc")
    t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
    colnames(t2) = c("quant", "value", "low", "upp")
    chibar = rbind(chibar, t2)
  }
  as.data.frame(chibar)
}

chibar = mstchibar(df2, 2012); chibar$Date = c(2012)

for (z in 2013:2020) {
  print(z)
  chibarx = mstchibar(df2, z)
  chibarx$Date = z
  chibar = rbind(chibar, chibarx)
}


#Plot results ------------------------------------------------------------------------------
chi_year = ggplot(chi, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chi")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "#1f78b4")+
  facet_wrap(~Date)+
  ylim(0, 1)+
  theme_bw()

chibar_year = ggplot(chibar, aes(quant, value))+
  geom_line(size = 1)+
  labs(x = "Quantile theshold q", y = "Chibar")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3, fill = "#1f78b4")+
  facet_wrap(~Date)+
  ylim(-1, 1)+
  theme_bw()






img1 = ggarrange(chi_plot + ggtitle("A"), chibar_plot + ggtitle("B"), ncol = 1); img1
img2 = ggarrange(chi_year + ggtitle("C"), chibar_year + ggtitle("D"), ncol = 1); img2

img3 = ggarrange(img1, img2, ncol = 2); img3


#ggsave(filename = "WG(ERA-SL)-Prec(ERAland)_chi_chibar.png", plot = img3,
#             path = "~/Figures/Correlations", width = 25, height = 20, units = "cm", dpi = 300)


#Export data -----------------------------------------------------------------------------
#chi$data = c("ERA5")
#chibar$data = c("ERA5")

#write.csv(chi, "~/Data/CHI_ERA5.csv", sep = ",", row.names = F)
#write.csv(chibar, "~/Data/CHIBAR_ERA5.csv", sep = ",", row.names = F)





plot(ppt[[71133]]*1000, col = viridis(100))
plot(xing, border = "yellow", add = T)
plot(ponto, col = "white", add = T)
plot(buff, border = "white", add = T)
























