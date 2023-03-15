#####################################################
# CHI coefficient threshold in-situ (Darro Station) #
#                                                   #
# Eduardo Q Marques 20-12-2022                      #
#####################################################

#rm(list =ls())
#.libPaths()
#install.packages("extRemes", type = "binary")

#.libPaths("C:/Users/queirozm/AppData/Local/Temp/Rtmpestoic/downloaded_packages")

# Description: This script is the multi temporal block by 1, 3 and 5 days. Only for the Darro station
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
df = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")

df = df %>% select(Date2, windSpd, year, Month, Day, ppt, Year)
colnames(df) = c("Date", "Wind", "y", "m", "d", "Prec", "year")
df$year = as.factor(df$year)
df = df%>% filter(y >= 2010)

#Getting the blowdown moments
dfb = df
dfb$date = substr(dfb$Date, 1, 10)
dfb = dfb %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate
dfb = dfb %>% filter(Prec < 70) #Remove outlier from precipitation
blw12 = dfb %>% filter(date == "2012-10-25")
blw19 = dfb %>% filter(date == "2019-02-02")

#Raw Data --------------------------------------------
eqm = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

#Block Maxima  ------------------------------------------------------------------
df3 = df
df3$date = substr(df3$Date, 1, 10)
df3$date = as.Date(df3$date, format = "%Y-%m-%d")
df3 = df3 %>% filter(Prec < 70) #Remove outlier from precipitation

#Block by 1 day (is not necessary a moving window)
#d1 <- blockmaxxer(df3, blocks = df3$date, which="Prec")
#d1b <- blockmaxxer(df3, blocks = df3$date, which="Wind")

d1 = df3 %>%
  group_by(date, m) %>%
  summarize(Prec = max(Prec, na.rm = T))

d1b = df3 %>%
  group_by(date, m) %>%
  summarize(Wind = max(Wind, na.rm = T))


d1$Wind = d1b$Wind; d1$window = c("1d")

#Functions to moving x days
move = function(day, wnd, vari) { # Target day, size of window (number of days before and after), Variable to be blocked
  w = day-wnd
  k = day+wnd
  max(vari[w:k], na.rm=T)
}

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
#multd = multd %>% filter(Wind >= 8) #Limiar of bloedown suggested by Marra

#Getting the blowdown moments
blw12 = multd %>% filter(date == "2012-10-25")
blw19 = multd %>% filter(date == "2019-02-02")

#Correlation by different blocked days --------------------------------------------
multd_plot = ggplot(multd, aes(x=Prec, y=Wind))+
  geom_point(aes(col = window), alpha = 0.1, size = 3)+
  #geom_smooth(aes(col = window), method = "lm")+
  geom_point(data=blw12, aes(x=Prec, y=Wind, fill=window), size=4, shape = 24)+
  geom_point(data=blw19, aes(x=Prec, y=Wind, fill=window), size=4, shape = 22)+
  stat_cor(aes(col = window), show.legend = F, label.y.npc="top", label.x.npc = 0.6)+
  labs( x = "Precipitação Máxima (mm)", y = "Velocidade do Vento (m/s)", title = "a)", fill = "Janela", col = "Janela", linetype = "Janela")+
  scale_color_manual(values = c("orange", "blue", "red"))+
  scale_fill_manual(values = c("orange", "blue", "red"))+
  ylim(0, 10)+
  theme_bw()+
  theme(legend.position = c(30, 0.50)); multd_plot


ggsave(filename = "Darro_WS-Prec_spatial_temporal_max_1-3-5days_port.png", plot = multd_plot,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
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
  labs(x = "Quantil", y = "Chi", title = "c)",
       fill = "Janela", col = "Janela", linetype = "Janela")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2)+
  scale_fill_manual(values = c("orange", "blue", "red"))+
  scale_color_manual(values = c("orange", "blue", "red"))+
  ylim(0, 0.75)+
  theme_bw()+
  theme(legend.position = c(0.85, 0.8)); test

ggsave(filename = "Darro_CHI_WG-Prec_spatial_temporal_max_1-3-5days_port.png", plot = test,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Tese_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)





























