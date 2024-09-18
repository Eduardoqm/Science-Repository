#########################################################
# Hyperion Time series for mean and relative difference #
# Confidence Intervals                                  #
# Eduardo Q Marques 05-07-2023                          #
#########################################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(mgcv)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

#Data ============================================================
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

#Modify Data =====================================================
df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Change names format
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "vig"] <- c("VIG")
df$index[df$index == "vari"] <- c("VARI")
df$index[df$index == "nirv"] <- c("NIRv")
df$index[df$index == "lwvi2"] <- c("LWVI2")
df$index[df$index == "msi"] <- c("MSI")
df$index[df$index == "ndwi"] <- c("NDWI")
df$index[df$index == "pssr"] <- c("PSSR")
df$index[df$index == "psri"] <- c("PSRI")
df$index[df$index == "sipi"] <- c("SIPI")
df$index[df$index == "wbi"] <- c("WBI")
df$index[df$index == "pri"] <- c("PRI")
df$index[df$index == "rendvi"] <- c("RENDVI")
df$index[df$index == "nbr"] <- c("NBR")
df$index[df$index == "nbr2"] <- c("NBR2")

df$treat[df$treat == "control"] <- c("Control")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")

eqm = c("orange", "red", "blue") #My color palette

#Mean time series =================================================
df_m = df %>% 
  na.omit() %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

#df_m$value[df_m$index == "NDWI"] <- (df_m$value)/10
df_m2 = df_m #To not have problem in diff...I dont know why!
colnames(df_m2) = c("Ano", "Treatment", "Indice", "Valor")

ggplot(df_m2, aes(x=Ano, y=Valor, color = Treatment))+
  geom_line(aes(group=Treatment), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Treatment))+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  facet_wrap(vars(Indice), scales = "free")+
  xlab(NULL)+ylab(NULL)+
  theme_bw()+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = c(0.5, 0.1))

#Diference by sbtraction ===========================================
#Calculate difference in relation of control
df_crt = filter(df_m, treat == "Control")
df_b3yr = filter(df_m, treat == "B3yr")
df_b1yr = filter(df_m, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Ano", "Treatment", "Indice", "Valor")

df_diff2 = df_diff
df_diff2$Indice <- factor(df_diff2$Indice,      # Reordering group factor levels
                        levels = c("PSRI","VIG","VARI","MSI","PSSR",
                                   "LWVI2","NDII","NBR2","NBR","NIRv",
                                   "NDWI","EVI","NDVI","PRI","RENDVI",
                                   "SIPI","WBI"))




all_vis = ggplot(df_diff2, aes(x=Ano, y=Valor, color = Treatment))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Treatment), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_wrap(vars(Indice), scales = "free", ncol = 5)+
  labs(x = NULL, y = NULL, title =  NULL)+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  theme(legend.position = c(0.5, 0.1)); all_vis


#ggsave(filename = "all_diff.png", plot = all_vis,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series",
#      width = 30, height = 20, units =  "cm", dpi = 300)





# Confidence intervals ----------------------------------------------------
desconfidence = function(ind){
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B1yr") %>% 
    filter(year == 2004)
  
  model = lm(value ~ 1, grnd_df)
  
  confint(model, level = 0.95)
  
  
  condef = data.frame(grnd_df$index[1], grnd_df$treat[1], grnd_df$year[1], mean(grnd_df$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2]) 
  colnames(condef) = c("index", "treat", "year", "mean", "low", "upper")
  
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B1yr")
  
  ylp = c(2005, 2006, 2008, 2010, 2011, 2012)
  
  
  for (z in ylp) {
    data = grnd_df %>%
      filter(year == z)
    
    model = lm(value ~ 1, data)
    
    
    condefx = data.frame( grnd_df$index[1], grnd_df$treat[1], z, mean(data$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2])
    colnames(condefx) = c("index", "treat", "year", "mean", "low", "upper")
    
    condef = rbind(condef, condefx)
    
    print(confint(model, level = 0.95))
  }
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B3yr") %>% 
    filter(year == 2004)
  
  model = lm(value ~ 1, grnd_df)
  
  confint(model, level = 0.95)
  
  
  condefB = data.frame(grnd_df$index[1], grnd_df$treat[1], grnd_df$year[1], mean(grnd_df$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2]) 
  colnames(condefB) = c("index", "treat", "year", "mean", "low", "upper")
  
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "B3yr")
  
  ylp = c(2005, 2006, 2008, 2010, 2011, 2012)
  
  
  for (z in ylp) {
    data = grnd_df %>%
      filter(year == z)
    
    model = lm(value ~ 1, data)
    
    
    condefxB = data.frame( grnd_df$index[1], grnd_df$treat[1], z, mean(data$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2])
    colnames(condefxB) = c("index", "treat", "year", "mean", "low", "upper")
    
    condefB = rbind(condefB, condefxB)
    
    print(confint(model, level = 0.95))
  }
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "Control") %>% 
    filter(year == 2004)
  
  model = lm(value ~ 1, grnd_df)
  
  confint(model, level = 0.95)
  
  
  condefC = data.frame(grnd_df$index[1], grnd_df$treat[1], grnd_df$year[1], mean(grnd_df$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2]) 
  colnames(condefC) = c("index", "treat", "year", "mean", "low", "upper")
  
  
  grnd_df = df %>% 
    na.omit() %>% 
    filter(index == ind) %>% 
    filter(treat == "Control")
  
  ylp = c(2005, 2006, 2008, 2010, 2011, 2012)
  
  
  for (z in ylp) {
    data = grnd_df %>%
      filter(year == z)
    
    model = lm(value ~ 1, data)
    
    
    condefxC = data.frame( grnd_df$index[1], grnd_df$treat[1], z, mean(data$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2])
    colnames(condefxC) = c("index", "treat", "year", "mean", "low", "upper")
    
    condefC = rbind(condefC, condefxC)
    
    print(confint(model, level = 0.95))
  }
  
  condefD = rbind(condef, condefB, condefC)
  return(condefD)
}


ind_list = c("VIG","VARI","MSI","PSSR", "LWVI2","NDII","NBR2","NBR","NIRv",
  "NDWI","EVI","NDVI","PRI","RENDVI", "SIPI","WBI")


hyconf = desconfidence("PSRI")

for (xx in ind_list) {
  print(xx)
  hyconfx = desconfidence(xx)
  hyconf = rbind(hyconf, hyconfx)
}


ggplot(hyconf, aes(x=year, y=mean, color = treat))+
  geom_line(aes(group=treat), size = 0.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 1, alpha = 0.5, aes(group=treat))+
  geom_errorbar(aes(ymin=low, ymax=upper), width=0.5)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  facet_wrap(vars(index), scales = "free")+
  xlab(NULL)+ylab(NULL)+
  theme_bw()+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = c(0.5, 0.1))



#Diference by sbtraction ===========================================
#Calculate difference in relation of control
confi_crt = filter(hyconf, treat == "Control")
confi_b3yr = filter(hyconf, treat == "B3yr")
confi_b1yr = filter(hyconf, treat == "B1yr")

confi_b3yr$value = 100 - ((confi_b3yr$mean*100)/confi_crt$mean)
confi_b3yr$low = 100 - ((confi_b3yr$low*100)/confi_crt$mean)
confi_b3yr$upper = 100 - ((confi_b3yr$upper*100)/confi_crt$mean)

confi_b1yr$value = 100 - ((confi_b1yr$mean*100)/confi_crt$mean)
confi_b1yr$low = 100 - ((confi_b1yr$low*100)/confi_crt$mean)
confi_b1yr$upper = 100 - ((confi_b1yr$upper*100)/confi_crt$mean)


confi_diff = rbind(confi_b3yr, confi_b1yr)
colnames(confi_diff) = c( "Indice", "Treatment", "Year","mean", "low", "upper", "Valor")

confi_diff$Indice <- factor(confi_diff$Indice,      # Reordering group factor levels
                          levels = c("PSRI","VIG","VARI","MSI","PSSR",
                                     "LWVI2","NDII","NBR2","NBR","NIRv",
                                     "NDWI","EVI","NDVI","PRI","RENDVI",
                                     "SIPI","WBI"))


all_vis = ggplot(confi_diff, aes(x = Year, y = Valor, color = Treatment))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Treatment), size = 1.5, alpha = 0.5)+
  geom_point(size = 1.5, alpha = 0.8)+
  geom_errorbar(aes(ymin=low, ymax=upper), width=0.5)+
  facet_wrap(vars(Indice), scales = "free", ncol = 5)+
  labs(x = NULL, y = NULL, title =  NULL)+
  scale_color_manual(values=c( "orange", "red"))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  xlim(2003.9, 2012.9)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  theme(legend.position = c(0.5, 0.1)); all_vis


ggsave(filename = "all_diff_confidence_intervals.png", plot = all_vis,
      path = "C:/Users/Workshop/Documents/Research/Doutorado/Capitulo1/Figuras/Hyperion Time Series",
      width = 30, height = 20, units =  "cm", dpi = 300)



diff_11 = confi_diff %>% 
  filter(Year == 2011)

