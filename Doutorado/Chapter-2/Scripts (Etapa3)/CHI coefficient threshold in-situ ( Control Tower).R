#####################################################
# CHI coefficient threshold in-situ (Control Tower) #
#                                                   #
# Eduardo Q Marques 20-12-2022                      #
#####################################################

#rm(list =ls())
#.libPaths()
#install.packages("extRemes", type = "binary")

#.libPaths("C:/Users/queirozm/AppData/Local/Temp/Rtmpestoic/downloaded_packages")

# Description: This script is the multi temporal block by 1, 3 and 5 days. For the Maximum WS from
# Control Tower and Precipitation from Darro station.
# Here we block by differente windowns, plot correlation and calculate de CHI value of results.

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(extRemes)
library(boot)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames/Plot_level")
tower = read.csv("Dados_Vento_Torre_Controle.csv", sep = ",")
tower = tower %>% select(datetime, y, m, d, max_speed)
colnames(tower) = c("Date", "y", "m", "d", "Wind")
tower$year = as.factor(tower$y)

darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")
darro = darro %>% select(Date2, year, Month, Day, ppt, Year)
colnames(darro) = c("Date", "y", "m", "d", "Prec", "year")
darro$year = as.factor(darro$year)

#Block Maxima  ------------------------------------------------------------------
tower$date = substr(tower$Date, 1, 10)
tower$date = as.Date(tower$date, format = "%Y-%m-%d")

darro$date = substr(darro$Date, 1, 10)
darro$date = as.Date(darro$date, format = "%Y-%m-%d")

#RAW ------------------------------------------------------------------------------
#Getting the blowdown moments
dfb = full_join(darro, tower, id = "date")
dfb = dfb %>% filter(year >= 2014)
dfb = dfb %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
dfb = dfb %>% filter(Prec < 70) #Remove outlier from precipitation
blw19 = dfb %>% filter(date == "2019-02-02")

#Raw Data --------------------------------------------
eqm = c('#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

raw = ggplot(dfb, aes(x=Prec, y=Wind))+
  geom_point(aes(col = year), alpha = 0.7, size = 3)+
  #geom_smooth(aes(col = window), method = "lm")+
  geom_point(data=blw19, aes(x=Prec, y=Wind), fill="black", size=3, shape = 22)+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = "Maximum Wind Speed (m/s)", title = "Raw Data from Tower/Darro Station (Maximum WS)")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30))

ggsave(filename = "Tower-Darro_raw_MaxWS-Prec.png", plot = raw,
       path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
       width = 12.5, height = 12.5, units = "cm", dpi = 300)

#Block by 1 day (is not necessary a moving window)
darro3 = darro %>%
  group_by(date, m, y) %>%
  summarize(Prec = max(Prec, na.rm = T))

tower3 = tower %>%
  group_by(date, m, y) %>%
  summarize(Wind = max(Wind, na.rm = T))

d1 = full_join(darro3, tower3, id = "date")
d1 = d1 %>% filter(y >= 2014)
d1$window = c("1d")
d1 = d1[,-3]

#Functions to moving x days
move = function(day, wnd, vari) { # Target day, size of window (number of days before and after), Variable to be blocked
  w = day-wnd
  k = day+wnd
  max(vari[w:k], na.rm=T)
}

#Block by 3 days
d3 = d1 #d3 will receive the modifications
d3$window = c("3d")

#Loop to modify all dataframe
for (x in 2:length(d3$date)) {
  d3$Prec[x] = move(x, 1, d1$Prec)
  d3$Wind[x] = move(x, 1, d1$Wind)
}

#Block by 5 days
d5 = d1 #d5 will receive the modifications
d5$window = c("5d")

#Loop to modify all dataframe
for (x in 3:length(d5$date)) {
  d5$Prec[x] = move(x, 2, d1$Prec)
  d5$Wind[x] = move(x, 2, d1$Wind)
}

#Join data frames
multd = rbind(d1, d3[-1,], d5[c(-1, -2),])
multd = multd %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
multd = multd %>% filter(Prec < 70) #Remove outlier from precipitation
multd[multd == -Inf] <- NA
#multd = multd %>% filter(Wind < 30) #Remove outlier from wind speed
#multd = multd %>% filter(Wind >= 8) #Limiar of bloedown suggested by Marra
#write.csv(multd, "multd.csv")

#Getting the blowdown moments
blw19 = multd %>% filter(date == "2019-02-02")

#Correlation by different blocked days --------------------------------------------
multd_plot = ggplot(multd, aes(x=Prec, y=Wind))+
  geom_point(aes(col = window), alpha = 0.1, size = 3)+
  #geom_smooth(aes(col = window), method = "lm")+
  geom_point(data=blw19, aes(x=Prec, y=Wind, fill=window), size=3, shape = 22)+
  stat_cor(aes(col = window), show.legend = F, label.y.npc="top", label.x.npc = 0.65)+
  labs( x = "Maximum Precipitation (mm)", y = "Maximum Wind Speed (m/s)", title = "b) Tower (Max WS)/Darro Station")+
  scale_color_manual(values = c("orange", "blue", "red"))+
  scale_fill_manual(values = c("orange", "blue", "red"))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.57))

ggsave(filename = "Tower_MaxWS-Prec_spatial_temporal_max_1-3-5days.png", plot = multd_plot,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)


#Extract tail dependence values and Bootstrapping for all years together --------------
tails = function(zet){
  df2 = zet
  df2 = na.omit(df2)
  tq = seq(.5, 1, .01); length(tq)
  
  t1 = taildep(df2$Prec, df2$Wind, 0.5)
  taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
  colnames(taild) = c("quant", "chi", "chibar")
  
  #Chi ---------------------------------------------------------------------------------
  #Function to extract Chi data from tail dependence
  chifun = function(formula, data, indices) {
    df2 <- data[indices,] # selecting sample with boot 
    fit <- taildep(df2$Prec, df2$Wind, 0.5)
    return(fit[[1]])
  } 
  
  f1 =c(df2$Prec, df2$Wind, 0.5)
  chifun(formula = f1, data = df2) #Just a test
  
  #Performing 1000 replications with boot 
  output <- boot(data=df2, statistic=chifun, 
                 R=1000, formula=f1)
  
  #Obtaining a confidence interval of 95%
  inter = boot.ci(output, type="perc")
  
  chi = data.frame(tq[[1]], inter$t0, inter$percent[1,4], inter$percent[1,5])
  colnames(chi) = c("quant", "value", "low", "upp")
  
  #Loop to do all Chi quantiles
  for (z in 2:50) {
    print(tq[[z]])
    #print(chi)
    chifun = function(formula, data, indices) {
      df2 <- data[indices,]
      fit <- taildep(df2$Prec, df2$Wind, (tq[[z]]))
      return(fit[[1]])
    }
    
    f1 =c(df2$Prec, df2$Wind, (tq[[z]]))
    
    output <- boot(data=df2, statistic=chifun, 
                   R=1000, formula=f1)
    inter = boot.ci(output, type="perc")
    t2 = data.frame(tq[[z]], inter$t0, inter$percent[1,4], inter$percent[1,5])
    colnames(t2) = c("quant", "value", "low", "upp")
    chi = rbind(chi, t2)
  }
  return(chi)
}

chi_d1 = tails(d1); chi_d1$window = c("1d")
chi_d3 = tails(d3); chi_d3$window = c("3d")
chi_d5 = tails(d5); chi_d5$window = c("5d")

chis = rbind(chi_d1, chi_d3, chi_d5)
#write.csv(chis, file = "CHI_1-3-5days.csv")

test = ggplot(chis, aes(quant, value, fill = window, linetype = window))+
  geom_line(aes(col = window))+
  labs(x = "Quantile theshold q", y = "Chi", title = "d) Tail Dependence (Maximum Tower)")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2)+
  scale_fill_manual(values = c("orange", "blue", "red"))+
  scale_color_manual(values = c("orange", "blue", "red"))+
  ylim(0, 0.75)+
  theme_bw()+
  theme(legend.position = c(0.85, 0.8))

ggsave(filename = "Tower_CHI_MaxWS-Prec_spatial_temporal_max_1-3-5days.png", plot = test,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)






