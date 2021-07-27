# Linear Model hypespectral VIs

# Eduardo Q Marques 21-07-2021

library(tidyverse)
library(reshape2)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.factor(df$treat)

x11()

#Linear Model with date as factor
df$treat = as.character(df$treat)
df$treat[df$treat == "control"] = c("a")
df$treat = as.factor(df$treat)

lmx2 = function(z){
  w = df %>% filter(index == z)
  w$year = as.factor(w$year)
  w = lm(value ~ treat + year + year:treat, data = w)
  print(summary(w))
  layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))
  plot(w)
  hist(w$residuals, main="Histogram of Residuals")
}

lmx2("evi2")
lmx2("ndvi")
lmx2("ndii")
lmx2("vig")
lmx2("vari")
lmx2("nirv")
lmx2("lwvi2")
lmx2("msi")
lmx2("ndwi")
lmx2("pssr")
lmx2("psri")
lmx2("sipi")
lmx2("wbi")
lmx2("pri")
lmx2("rendvi")
lmx2("nbr")
lmx2("nbr2")

#Generalized Linear Model with date as factor
#df$treat = as.character(df$treat)
#df$treat[df$treat == "control"] = c("a")
#df$treat = as.factor(df$treat)

glmx = function(z){
  w = df %>% filter(index == z)
  w$year = as.factor(w$year)
  w = glm(value ~ treat + year + year:treat, data = w)
  print(summary(w))
  layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))
  plot(w)
  hist(w$residuals, main="Histogram of Residuals")
}

glmx("evi2")
glmx("ndvi")
glmx("ndii")
glmx("vig")
glmx("vari")
glmx("nirv")
glmx("lwvi2")
glmx("msi")
glmx("ndwi")
glmx("pssr")
glmx("psri")
glmx("sipi")
glmx("wbi")
glmx("pri")
glmx("rendvi")
glmx("nbr")
glmx("nbr2")




#Trying posthoc and plot results
k = df %>% filter(index == "ndvi")
k$year = as.factor(k$year)
k$value2 = log(k$value)
model = glm(value ~ treat + year + year:treat, data = k)
summary(model)

#Posthoc
library(emmeans)
model2 = lsmeans(model, pairwise~treat + year + year:treat, adjust="tukey")
model2
plot(model2, comparisons = TRUE)


#Convert lsmeans results to data frames
contra = as.data.frame(model2$contrasts)
means = as.data.frame(model2$lsmeans)

#Means plot
eqm = c("blue", "orange", "red") #My color palette

ggplot(means, aes(x=year, y=lsmean, col = treat))+
  geom_line(aes(group = treat), size = 1.5, alpha = 0.5)+
  geom_point(size = 1.5)+
  scale_color_manual(values = eqm)+
  theme_bw()


#Contrasts
ggplot(contra, aes(x=contrast, y=p.value))+
  geom_point(size = 1.5)

contra$treat = substr(contra$contrast, 10, 13)
contra$year = substr(contra$contrast, 3, 6)

contra2 = contra %>% 
  filter(contrast %in% c("a 2004 - b1yr 2004", "a 2004 - b3yr 2004", "a 2005 - b1yr 2005", "a 2005 - b3yr 2005", "a 2006 - b1yr 2006", "a 2006 - b3yr 2006", "a 2008 - b1yr 2008", "a 2008 - b3yr 2008", "a 2010 - b1yr 2010", "a 2010 - b3yr 2010", "a 2011 - b1yr 2011", "a 2011 - b3yr 2011", "a 2012 - b1yr 2012", "a 2012 - b3yr 2012"))

ggplot(contra2, aes(x=year, y=p.value, shape = treat))+
  geom_point(size = 10, alpha = 0.5)+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  #scale_color_manual(values = eqm)+
  theme_bw()



library(jtools)
plot_summs(model, scale = T, plot.distributions = T, inner_ci_level = .9)





