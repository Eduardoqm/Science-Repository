#######################################################
# Path Analysis Vegetation Index and plot data        #
# About analysis http://www.rpubs.com/tbihansk/302732 #
# Eduardo Q Marques  02/10/2019                       #
#######################################################

library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)

#Data
#Fuel quantity
#Obs: All indices versus litter data separately for each parcel
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Master_Area1_ by plot.csv", sep = ",", header = TRUE)

#Specify the model
str_model <- 'liteira ~ evi + ndvi + vari + vig + nirv'
bioc_model <- 'liteira ~ ari + lwvi2 + msi + ndii + ndwi + pssr + psri + sipi + wbi'
phy_model <- 'liteira ~ pri + rendvi'
fire_model <- 'liteira ~ nbri + nbri2'

#Fit the model
fit1 <- cfa(str_model, data = df)
fit2 <- cfa(bioc_model, data = df)
fit3 <- cfa(phy_model, data = df)
fit4 <- cfa(fire_model, data = df)

#View results
summary(fit1, fit.measures = TRUE, standardized=T,rsquare=T)
summary(fit2, fit.measures = TRUE, standardized=T,rsquare=T)
summary(fit3, fit.measures = TRUE, standardized=T,rsquare=T)
summary(fit4, fit.measures = TRUE, standardized=T,rsquare=T)

#Building a Structural Equation Model (SEM)
#semPaths(fit, 'std', layout = 'circle')
semPaths(fit1,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)
semPaths(fit2,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)
semPaths(fit3,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)
semPaths(fit4,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

#Correlation bettewen all data
ggcorr(df, nbreaks = 10, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))













