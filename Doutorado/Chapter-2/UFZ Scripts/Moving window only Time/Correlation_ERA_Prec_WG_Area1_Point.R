#---------------------------------------------------------------------------------------------------
# Correlations Precipitation (ERA5 Land) vs Wind Gust (ERA5 Single Levels) Maxima Area1 buffered
#
# Eduardo Q Marques 25-07-2022
#--------------------------------------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridis)
library(extRemes)
library(boot)

#Load data ---------------------------------------------------------------------
setwd("/home/queirozm/eqm_eth_ufz/Data/DataFrames")
df = read.csv("ERA5_Prec_WG_Area1_Point.csv", sep = ",")

df$Prec = df$Prec*1000
df$year = substr(as.factor(df$Date), 1, 4)
df$date = substr(df$Date, 1, 10)
df$date = as.Date(df$Date, format = "%Y.%m.%d")

#Plot data
#eqm = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')
eqm = c('#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928')

blw12 = df %>% filter(date == "2012-10-25")
blw19 = df %>% filter(date == "2019-02-02")

raw = df %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate

rawppt = ggplot(raw, aes(x=Prec, y=Wind))+
  geom_point(aes(col = year), alpha = 0.7, size = 3)+
  geom_point(data=blw12, aes(x=Prec, y=Wind), fill="black", size=3, shape = 24)+
  geom_point(data=blw19, aes(x=Prec, y=Wind), fill="black", size=3, shape = 22)+
  #geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F, label.y.npc="top", label.x.npc = 0.62)+
  labs( x = "Precipitation (mm)", y = "Wind Gust (m/s)", title = "Raw Data from ERA5")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  ylim(0, 13)+
  theme(legend.position = c(30, 30))#; rawppt

rawppt2 = ggplot(raw, aes(x=Prec, y=Wind))+
  geom_point(aes(col = year), alpha = 0.9, size = 2)+
  geom_point(data=blw12, aes(x=Prec, y=Wind), fill="black", size=3, shape = 24)+
  geom_point(data=blw19, aes(x=Prec, y=Wind), fill="black", size=3, shape = 22)+
  #geom_smooth(method = "lm", col = "black")+
  labs( x = "Precipitation (mm)", y = NULL, title = "")+
  stat_cor(show.legend = F)+
  facet_wrap(~year)+
  scale_color_manual(values = eqm)+
  theme_bw()+
  ylim(0, 15)+
  theme(legend.position = c(30, 30))#; rawppt2

rawppt3 = ggarrange(rawppt, rawppt2, ncol = 2)
ggsave(filename = "ERA_raw_WG-Prec.png", plot = rawppt3, path = "/home/queirozm/eqm_eth_ufz/Temp_figures", width = 25, height = 12.5, units = "cm", dpi = 300)

#Block Maxima diary precipitation -----------------------------------------------
df3 = df
df3$date = substr(df3$Date, 1, 10)
df3$date = as.Date(df3$date, format = "%Y.%m.%d")

#df3 = na.omit(df3)
#d1 = blockmaxxer(df3, blocks = df3$date, which = "Prec")

d1 = df3 %>%
    group_by(date, m) %>%
    summarize(Prec = max(Prec, na.rm = T))

d1b = df3 %>%
    group_by(date, m) %>%
    summarize(Wind = max(Wind, na.rm = T))

d1$Wind = d1b$Wind; d1$window = c("1d")

df4 = d1
df4$year = substr(df4$date, 1, 4)
#df4c = df4 %>% filter(Wind >= 8)

blw12 = df4 %>% filter(date == "2012-10-25")
blw19 = df4 %>% filter(date == "2019-02-02")

df4 = df4 %>% filter(m %in% c("10","11","12","1","2","3","4")) #Rainy months to AW climate

maxppt = ggplot(df4, aes(x=Prec, y=Wind))+
  geom_point(aes(col = year), alpha = 0.7, size = 3)+
  geom_point(data=blw12, aes(x=Prec, y=Wind), fill="black", size=3, shape = 24)+
  geom_point(data=blw19, aes(x=Prec, y=Wind), fill="black", size=3, shape = 22)+
  #geom_smooth(method = "lm", col = "black")+
  stat_cor(show.legend = F)+
  labs( x = "Maximum Precipitation (mm)", y = "Wind Gust (m/s)", title = "ERA5 - Temporal Block Maxima")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(legend.position = c(30, 30))

maxppt2 = ggplot(df4, aes(x=Prec, y=Wind))+
  geom_point(aes(col = year), alpha = 0.7, size = 2)+
  geom_point(data=blw12, aes(x=Prec, y=Wind), fill="black", size=2, shape = 24)+
  geom_point(data=blw19, aes(x=Prec, y=Wind), fill="black", size=2, shape = 22)+
  #geom_smooth(method = "lm", col = "black")+
  labs( x = "Maximum Precipitation (mm)", y = NULL, title = "")+
  stat_cor(show.legend = F)+
  scale_color_manual(values = eqm)+
  facet_wrap(~year)+
  theme_bw()+
  theme(legend.position = c(30, 30))

maxppt3 = ggarrange(maxppt, maxppt2, ncol = 2)

ggsave(filename = "WG-Prec_BOTH_temporal_max.png", plot = maxppt3, path = "/home/queirozm/eqm_eth_ufz/Temp_figures", width = 25, height = 12.5, units = "cm", dpi = 300)


#Extract tail dependence values and Bootstrapping for all years together --------------
tails = function(zet){
  df2 = zet
  df2 = na.omit(df2)
  tq = seq(.5, 1, .01); length(tq)

  t1 = taildep(df2$Prec, df2$Wind, 0.05)
  taild = data.frame(tq[[1]], t1[[1]], t1[[2]])
  colnames(taild) = c("quant", "chi", "chibar")

  #Chi ---------------------------------------------------------------------------------
  #Function to extract Chi data from tail dependence
  chifun = function(formula, data, indices) {
    df2 <- data[indices,] # selecting sample with boot 
    fit <- taildep(df2$Prec, df2$Wind, 0.05)
    return(fit[[1]])
  } 

  f1 =c(df2$Prec, df2$Wind, 0.05)
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

chi = tails(df4)

test = ggplot(chi, aes(quant, value))+
  geom_line()+
  labs(x = "Quantile theshold q", y = "Chi", title = "Tail Dependence (ERA5)")+
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "orange", alpha = 0.2)+
  ylim(0, 1)+
  theme_bw()+
  theme(legend.position = c(0.85, 0.8))

ggsave(filename = "ERA_CHI and CHIBAR_WG-Prec_temporal_max.png", plot = test,
path = "/home/queirozm/eqm_eth_ufz/Temp_figures",
width = 14, height = 10, units = "cm", dpi = 300)