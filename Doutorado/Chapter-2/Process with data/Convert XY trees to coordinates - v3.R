#Convert XY trees to coordinates
#Blowdown Data (AREA-1)
#Eduardo Q Marques 10-06-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(viridis)
library(fmsb)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')
df = read.csv("blowdown_full_update_2021_all_C.csv", sep = ",")

#Transformation in meters from Point A0 =======================================================
#Part 1 - Make Y coordenates
#Transect to meter
df$trasc_m[df$transecto == "A"] <- 0
df$trasc_m[df$transecto == "AA"] <- 10
df$trasc_m[df$transecto == "AB"] <- 20
df$trasc_m[df$transecto == "B"] <- 50
df$trasc_m[df$transecto == "C"] <- 100
df$trasc_m[df$transecto == "D"] <- 150
df$trasc_m[df$transecto == "E"] <- 200
df$trasc_m[df$transecto == "F"] <- 250
df$trasc_m[df$transecto == "G"] <- 300
df$trasc_m[df$transecto == "H"] <- 350
df$trasc_m[df$transecto == "I"] <- 400
df$trasc_m[df$transecto == "J"] <- 450
df$trasc_m[df$transecto == "K"] <- 500
df$trasc_m[df$transecto == "L"] <- 550
df$trasc_m[df$transecto == "M"] <- 600
df$trasc_m[df$transecto == "N"] <- 650
df$trasc_m[df$transecto == "O"] <- 700
df$trasc_m[df$transecto == "P"] <- 750
df$trasc_m[df$transecto == "Q"] <- 800
df$trasc_m[df$transecto == "R"] <- 850
df$trasc_m[df$transecto == "S"] <- 900
df$trasc_m[df$transecto == "T"] <- 950
df$trasc_m[df$transecto == "U"] <- 1000

df$trasc_m <- replace(df$trasc_m,is.na(df$trasc_m),0)
df$nsdist <- replace(df$nsdist,is.na(df$nsdist),0)

df$trasc_m = as.numeric(df$trasc_m)
df$y = (df$trasc_m+(df$nsdist)) #Include trees thats only have nsdist

#Part 2 - Make X coordenates
df$x = df$metragem #Just not work for trees bigger 40cm


#Convert lines in meters
lmeter = seq(0,1500, by = 50)#Sequence and for to convert lines in meters
for (x in 1:31) {
  df$linha_m[df$linha_bl == x] <- lmeter[[x]]
}

#Convert X=NA to line distance in meters
for (z in 1:length(df$x)) {
  df$x[[z]] <- replace(df$x[[z]],is.na(df$x[[z]]),df$linha_m[[z]])
}

#Adjust with lodist values
for (z in 1:length(df$x)) {
  df$x[[z]][df$lo[[z]] == "O"] <- (df$x[[z]] + df$lodist[[z]]) #West difference
  df$x[[z]][df$lo[[z]] == "L"] <- (df$x[[z]] - df$lodist[[z]]) #East difference
}

#Transformation in UTM from Point A0 =======================================================
#One meter unit in UTM = 0.000009
#Coordenates of Area-1 shapefile vertice  = -52.37688, -13.07417
df$x = -((df$x*0.000009)+52.37688)
df$y = -((df$y*0.000009)+13.07417)


#Tests results =============================================================================
#x11()
eqm = c("red","orange", "blue","green")
ggplot(df, aes(x=x, y=y))+
  geom_point(aes(col = tipo_de_dano), size = 3, alpha = 0.7)+
  scale_color_manual(values = eqm)+
  theme_light()+
  coord_fixed()

#Export data ================================================================================
df2 = df[,c(1:30, 32, 33)] #Remove no important calumns created in the process

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento")

#write.table(df2, file = "blowdown_full_update_2021_XY.csv", sep = ",", row.names = F)








