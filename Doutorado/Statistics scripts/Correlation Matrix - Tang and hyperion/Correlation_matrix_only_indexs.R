#######################################################
# Correlaion - Vegetation Indexs                      #
# Eduardo Q Marques  04/11/2019                       #
#######################################################

library(ggplot2)
library(GGally)

setwd('C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\Doutorado\\Deposito\\Banco de Dados Tanguro\\Dados para analise cap1')

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)
#hy = hy[,-1]; hy = hy[,-19]

#All Indexs
ggpairs(hy[,-1])


#Struture
struc = hy[,c(2, 4, 10, 12, 18, 19)]
ggpairs(struc)

#Biochemistry
bioc = hy[,c(2,3,5,6,9,11,14,15,17,20)]
ggpairs(bioc)

#Physiology
phy = hy[,c(2,13,16)]
ggpairs(phy)

#Fire
fir = hy[,c(2,7,8)]
ggpairs(fir)






