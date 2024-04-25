##########################################
# CHI coefficient threshold (1979-2020)  #
#                                        #
# Eduardo Q Marques 10-10-2022           #
##########################################

#rm(list =ls())
#.libPaths()
#install.packages("extRemes", type = "binary")

#.libPaths("C:/Users/queirozm/AppData/Local/Temp/Rtmpestoic/downloaded_packages")

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(extRemes)
library(boot)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames")

df1p = read.csv("ERA5_Block_1pixel_1979_2020.csv", sep = ",")
df3p = read.csv("ERA5_Block_3pixel_1979_2020.csv", sep = ",")
df5p = read.csv("ERA5_Block_5pixel_1979_2020.csv", sep = ",")
df7p = read.csv("ERA5_Block_7pixel_1979_2020.csv", sep = ",")

df1p$degrees = c("0.25?")
df3p$degrees = c("0.75?")
df5p$degrees = c("1.25?")
df7p$degrees = c("1.75?")

#df1p = df1p %>% separate(xy, c("x","y"), sep = "_")
#df3p = df3p %>% separate(xy, c("x","y"), sep = "_")
#df5p = df5p %>% separate(xy, c("x","y"), sep = "_")
#df7p = df7p %>% separate(xy, c("x","y"), sep = "_")

#Filter data by pixel overlapped with Tanguro farm -----------------------------
df1p2 = df1p %>% filter(xy == "-52.25_-13")
df3p2 = df3p %>% filter(xy == "-52.25_-13")
df5p2 = df5p %>% filter(xy == "-52.25_-13")
df7p2 = df7p %>% filter(xy == "-52.25_-13")

#Correlation by different blocked days -----------------------------------------
corx = function(a, b){
  df = a
  blw12 = df %>% filter(date == "2012-10-25")
  blw19 = df %>% filter(date == "2019-02-02")
  
  ggplot(df, aes(x=Prec, y=Wind))+
    geom_point(aes(col = window), alpha = 0.1, size = 3)+
    #geom_smooth(aes(col = window), method = "lm")+
    geom_point(data=blw12, aes(x=Prec, y=Wind, fill=window), size=4, shape = 24)+
    geom_point(data=blw19, aes(x=Prec, y=Wind, fill=window), size=4, shape = 22)+
    stat_cor(aes(col = window), show.legend = F, label.y.npc="top", label.x.npc = 0.6)+
    labs( x = "Maximum Precipitation (mm)", y = "Wind Gust (m/s)", title = b)+
    scale_color_manual(values = c("orange", "blue", "red"))+
    scale_fill_manual(values = c("orange", "blue", "red"))+
    ylim(3, 16)+ xlim(0, 31)+
    theme_bw()+
    theme(legend.position = c(0.9, 0.25))
}

a1 = corx(df1p2, "a) ERA5 - Spatial/Temporal Block Maxima (0.25?)")
a2 = corx(df3p2, "b) ERA5 - Spatial/Temporal Block Maxima (0.75?)")
a3 = corx(df5p2, "c) ERA5 - Spatial/Temporal Block Maxima (1.25?)")
a4 = corx(df7p2, "d) ERA5 - Spatial/Temporal Block Maxima (1.75?)")


ggsave(filename = "ERA_Corr_1p_1-3-5d.png", plot = a1,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_Corr_3p_1-3-5d.png", plot = a2,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_Corr_5p_1-3-5d.png", plot = a3,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

ggsave(filename = "ERA_Corr_7p_1-3-5d.png", plot = a4,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 10, units = "cm", dpi = 300)

#Calculating the CHI Coefficient Threshold -------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames/CHIs")

tails = function(zet, dia){
  df2 = zet
  df2 = df2 %>% filter(window == dia)
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
  chi$window = dia
  return(chi)
}

#Tail Dependence for 1 pixel data (0.25)
#chip1d1 = tails(df1p2, "1d"); chip1d3 = tails(df1p2, "3d"); chip1d5 = tails(df1p2, "5d")
#chis1p = rbind(chip1d1, chip1d3, chip1d5)
#write.csv(chis1p, file = "CHI_1p.csv", row.names = F)

#Tail Dependence for 3 pixels data (0.75)
#chip3d1 = tails(df3p2, "1d"); chip3d3 = tails(df3p2, "3d"); chip3d5 = tails(df3p2, "5d")
#chis3p = rbind(chip3d1, chip3d3, chip3d5)
#write.csv(chis3p, file = "CHI_3p.csv", row.names = F)

#Tail Dependence for 5 pixels data (1.25)
#chip5d1 = tails(df5p2, "1d"); chip5d3 = tails(df5p2, "3d"); chip5d5 = tails(df5p2, "5d")
#chis5p = rbind(chip5d1, chip5d3, chip5d5)
#write.csv(chis5p, file = "CHI_5p.csv", row.names = F)

#Tail Dependence for 7 pixels data (1.75)
#chip7d1 = tails(df7p2, "1d"); chip7d3 = tails(df7p2, "3d"); chip7d5 = tails(df7p2, "5d")
#chis7p = rbind(chip7d1, chip7d3, chip7d5)
#write.csv(chis7p, file = "CHI_7p.csv", row.names = F)

#Plot Tail Dependence Threshold ------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Backup UFZ Server/Data/DataFrames/CHIs")
dir()

chip1p = read.csv("CHI_1p.csv", sep = ",")
chip3p = read.csv("CHI_3p.csv", sep = ",")
chip5p = read.csv("CHI_5p.csv", sep = ",")
chip7p = read.csv("CHI_7p.csv", sep = ",")

chiplot = function(a, b){
  ggplot(a, aes(quant, value, fill = window, linetype = window))+
    geom_line(aes(col = window))+
    #labs(x = "Quantile theshold q", y = "Chi", title = b)+
    labs(x = "Quantile theshold q", y = "Chi")+
    annotate("text", x = 0.5, y = 0.57, label = b, size = 6)+
    geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2)+
    scale_fill_manual(values = c("orange", "blue", "red"))+
    scale_color_manual(values = c("orange", "blue", "red"))+
    ylim(0, 0.57)+
    theme_bw()+
    theme(legend.position = c(0.85, 0.8))
}

b1 = chiplot(chip1p, "a)"); b1
b2 = chiplot(chip3p, "b)"); b2
b3 = chiplot(chip5p, "c)"); b3
b4 = chiplot(chip7p, "d)"); b4


ggsave(filename = "ERA_CHI_1p_1-3-5d.tiff", plot = b1,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 9.5, units = "cm", dpi = 300)

ggsave(filename = "ERA_CHI_3p_1-3-5d.tiff", plot = b2,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 9.5, units = "cm", dpi = 300)

ggsave(filename = "ERA_CHI_5p_1-3-5d.tiff", plot = b3,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 9.5, units = "cm", dpi = 300)

ggsave(filename = "ERA_CHI_7p_1-3-5d.tiff", plot = b4,
       path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo2/Figuras/Paper_Figures",
       width = 12, height = 9.5, units = "cm", dpi = 300)

















