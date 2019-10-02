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
mtcars

#Specify the model
model <-'
mpg ~ hp + gear + cyl + disp + carb + am + wt
hp ~ cyl + disp + carb
'
#Fit the model
fit <- cfa(model, data = mtcars)

#View results
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

#Building a Structural Equation Model (SEM)
semPaths(fit, 'std', layout = 'circle')

semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

ggcorr(mtcars[-c(5, 7, 8)], nbreaks = 6, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))















