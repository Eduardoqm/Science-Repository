########################################################
# Landsat Time series for mean and relative difference #
# BOOTSTRAP                                            #
#                                                      #
# Eduardo Q Marques 19-06-2023                         #
########################################################

library(tidyverse)
library(ggplot2)
library(mgcv)
library(boot)
font_import()
loadfonts(device = "win", quiet = TRUE)
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

#Data ============================================================
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
dfm = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify Data =====================================================
dfm$year = substr(dfm$year, 1,4)
dfm$year = as.numeric(dfm$year)
dfm$index = as.character(dfm$index)
dfm$treat = as.character(dfm$treat)

#Modify elements of dataframe
dfm$index[dfm$index == "evi2"] <- c("EVI")
dfm$index[dfm$index == "ndvi"] <- c("NDVI")
dfm$index[dfm$index == "ndii"] <- c("NDII")
dfm$index[dfm$index == "grnd"] <- c("GRND")
dfm$index[dfm$index == "nbr"] <- c("NBR")
dfm$index[dfm$index == "nbr2"] <- c("NBR2")

dfm$treat[dfm$treat == "control"] <- c("Control")
dfm$treat[dfm$treat == "b3yr"] <- c("B3yr")
dfm$treat[dfm$treat == "b1yr"] <- c("B1yr")

#Resume repeat years with mean ====================================
df = dfm %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value))

summary(df$y)
min(df$y) #-13.08343 = 1000 meters
max(df$y) #-13.07422 = 0 meters

diffy = min(df$y) - max(df$y)
df$dist = ((max(df$y) - df$y)/diffy)*1000
df$dist = abs(df$dist)

summary(df$dist)
#evi = df %>% filter(index == "EVI")
#ggplot(evi, aes(x=value, y=dist, col=treat))+
#  geom_point(alpha = 0.35, size = 3)+
#  scale_color_manual(values = c("orange","red","blue"))


df$dist2 = c("a")
df$dist2[df$dist <= 250] = c("Borda")
df$dist2[df$dist > 250] = c("Interior")

#evi = df %>% filter(index == "EVI")
#ggplot(evi, aes(x=value, y=dist, col=treat))+
#  geom_point(aes(shape = dist2), alpha = 0.35, size = 3)+
#  scale_color_manual(values = c("orange","red","blue"))

# Confidence intervals ----------------------------------------------------
grnd_df = df %>% 
  na.omit() %>% 
  filter(index == "GRND") %>% 
  filter(treat == "B1yr") %>% 
  filter(year == 1985)

model = lm(value ~ 1, grnd_df)

confint(model, level = 0.95)


condef = data.frame(grnd_df$year[1], mean(grnd_df$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2]) 
colnames(condef) = c("year", "mean", "low", "upper")


grnd_df = df %>% 
  na.omit() %>% 
  filter(index == "GRND") %>% 
  filter(treat == "B1yr")

ylp = c(1986:2011, 2013:2019)


for (z in ylp) {
  data = grnd_df %>%
    filter(year == z)
  
  model = lm(value ~ 1, data)
  
  
  condefx = data.frame(z, mean(data$value), confint(model, level = 0.95)[1], confint(model, level = 0.95)[2])
  colnames(condefx) = c("year", "mean", "low", "upper")

    condef = rbind(condef, condefx)
  
  print(confint(model, level = 0.95))
  
}


ggplot(condef, aes(x=year, y=mean))+
  geom_line()+
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2)





#Smooth time series ===============================================
df_smt = df #%>% select(x, y, index, year, treat, value)
#df_smt$year = as.character(df_smt$year)
#df_smt$value = as.numeric(format(round(df_smt$value, 2), nsmall = 2))
colnames(df_smt) = c("x","y","Indice","Year","Treatment","Valor","Dist","Dist2")

eqm = c("orange", "red", "blue") #My color palette

smtplot = ggplot(df_smt, aes(x=Year, y=Valor, color = Treatment))+
  geom_smooth(method='gam',aes(group=Treatment), alpha = 0.5, size = 1, level = 0.999, se = T)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  #stat_summary(geom="line", fun.data="mean_cl_boot", size = 0.5, linetype = "dashed", aes(group=Tratamento))+
 # stat_summary(geom="point", fun.data="mean_cl_boot",
  #             size = 2, alpha = 0.6, aes(group=Treatment, shape = Treatment))+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.6, aes(group=Treatment, shape = Treatment))+
  #stat_summary(geom="pointrange", fun.data="mean_cl_boot", alpha = 0.5, aes(group=Tratamento))+
  stat_summary(fun.y="mean", geom = "errorbar", aes(group=Treatment))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 90))+
  labs(y = NULL)+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14)); smtplot

#3ggsave(filename = "Smooth_Landsat.png", plot = smtplot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", 
#       width = 20, height = 17, units =  "cm", dpi = 300)

#Mean time series =================================================
df_m = df %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

df_m2 = df_m #To not have problem in diff...I dont know why!
colnames(df_m2) = c("Year", "Treatment", "Indice", "Valor")

ggplot(df_m2, aes(x=Year, y=Valor, color = Treatment))+
  geom_rect(aes(xmin = 2004, xmax = 2011, ymin = -Inf, ymax = Inf),
            fill = "black", color = NA, alpha = 0.002)+
  geom_line(aes(group=Treatment), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Treatment))+
  #geom_vline(xintercept = "2004", linetype = "dashed")+
  #geom_vline(xintercept = "2011", linetype = "dashed")+
  #stat_summary(geom="line", fun.y="mean", size = 1.5, aes(group=Tratamento))+
  #stat_summary(geom="point", fun.y="mean", size = 2, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Diference by sbtraction ===========================================
#Calculate difference in relation of control
df_crt = filter(df_m, treat == "Control")
df_b3yr = filter(df_m, treat == "B3yr")
df_b1yr = filter(df_m, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Year", "Treatment", "Indice","Valor")

difplot = ggplot(df_diff, aes(x=Year, y=Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Treatment))+
  labs(y = "Burned - Control (% of difference)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14)); difplot

#ggsave(filename = "Landsat_1985-2019_diff.png", plot = difplot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", 
#     width = 20, height = 10, units =  "cm", dpi = 300)


# Confidence intervals ----------------------------------------------------
grnd_diff = df_diff %>% 
  filter(Indice == "GRND") %>% 
  filter(Treatment == "B1yr")

m_value = mean(grnd_diff$Valor) #Mean
n = length(grnd_diff$Valor)    #Number of data
sdev = sd(grnd_diff$Valor)     #Standard Deviation
s_error = sdev/sqrt(n)         #Standard Error


alpha = 0.05
d_free = n - 1 #Degrees of freedom
t_score = qt(p=alpha/2, df=d_free,lower.tail=F)
m_error <- t_score * s_error #Margin error


#Calculating lower bound and upper bound
lower_bound <- m_value - m_error
upper_bound <- m_value + m_error

#Print the confidence interval
print(c(lower_bound,upper_bound))







#Edge and Interior facet ===============================================
#Mean time series ======================================================
df_edge = df %>% 
  filter(dist2 == "Borda") %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

df_edge2 = df_edge #To not have problem in diff...I dont know why!
colnames(df_edge2) = c("Ano", "Tratamento", "Indice", "Valor")

df_core = df %>% 
  filter(dist2 == "Interior") %>% 
  group_by(year, treat, index) %>% 
  summarise(value = mean(value))

df_core2 = df_core #To not have problem in diff...I dont know why!
colnames(df_core2) = c("Ano", "Tratamento", "Indice", "Valor")


ggplot(df_edge2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  ggtitle("Edge")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

ggplot(df_core2, aes(x=Ano, y=Valor, color = Tratamento))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group=Tratamento), size = 1.5, alpha = 0.7)+
  stat_summary(geom="point", fun.y="mean", size = 2, alpha = 0.5, aes(group=Tratamento))+
  facet_grid(rows = vars(Indice), scales = "free")+
  theme_bw()+
  ggtitle("Core")+
  scale_color_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Difference from Control ===========================================
#Calculate difference in relation of control
df_crt = filter(df_edge, treat == "Control")
df_b3yr = filter(df_edge, treat == "B3yr")
df_b1yr = filter(df_edge, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Year", "Tratamento", "Indice","Valor")
df_dif_ed = df_diff
#df_dif_ed$Valor = abs(df_dif_ed$Valor)

df_crt = filter(df_core, treat == "Control")
df_b3yr = filter(df_core, treat == "B3yr")
df_b1yr = filter(df_core, treat == "B1yr")

df_b3yr$value = 100 - ((df_b3yr$value*100)/df_crt$value)
df_b1yr$value = 100 - ((df_b1yr$value*100)/df_crt$value)
df_diff = rbind(df_b3yr, df_b1yr)
colnames(df_diff) = c("Year", "Tratamento", "Indice","Valor")
df_dif_co = df_diff
#df_dif_co$Valor = abs(df_dif_co$Valor)

a = ggplot(df_dif_ed, aes(x=Year, y=Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento))+
  labs(y = "Burned - Control (% of difference)")+
  theme_bw()+
  ggtitle("Edge")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14)); a


b = ggplot(df_dif_co, aes(x=Year, y=Valor, color = Indice))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Indice), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento))+
  labs(y = "Burned - Control (% of difference)")+
  theme_bw()+
  ggtitle("Core")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#377eb8','#1b9e77','#e41a1c','darkred','#ff7f00','#4daf4a'))+
  theme(text = element_text(family = "Times New Roman", size = 14)); b


#ggsave(filename = "Diff_Edge.png", plot = a,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", width = 20, height = 10, units = "cm", dpi = 300)

#ggsave(filename = "Diff_Core.png", plot = b,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Landsat Time Series", width = 20, height = 10, units = "cm", dpi = 300)


















