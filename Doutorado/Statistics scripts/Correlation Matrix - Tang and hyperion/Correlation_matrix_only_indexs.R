#######################################################
# Correlaion - Vegetation Indexs                      #
# Eduardo Q Marques  04/11/2019                       #
#######################################################

library(ggplot2)
library(GGally)



setwd('C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\Doutorado\\Deposito\\Banco de Dados Tanguro\\Dados para analise cap1')

df = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)

ggpairs(arv[,-1])



#Correlation bettewen all data
ggcorr(df, nbreaks = 10, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))













