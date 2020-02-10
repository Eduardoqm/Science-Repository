#Plots to comparation Insdex data and Area1 data

#Eduardo Q Marques 06-02-2020
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Data bank
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1')

landsat = read.csv("Landast_indexs_median by plot.csv")


hyper = read.csv("Hyperion_indexs_median by plot_2.csv")


satellite = full_join(landsat, hyper, by = "parcela")
