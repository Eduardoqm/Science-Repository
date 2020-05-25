#Path Analysis EXEMPLE
#Test from the http://www.rpubs.com/tbihansk/302732
#Eduardo Q Marques  02/10/2019

library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)

# Organizing package information for table
packages <- c("tidyverse", "knitr", "kableExtra", "lavaan", "semPlot", "OpenMx", "GGally")
display <- c("Package","Title", "Maintainer", "Version", "URL")
table <- matrix(NA, 1, NROW(display), dimnames = list(1, display))
for(i in 1:NROW(packages)){
  list <- packageDescription(packages[i])
  table <- rbind(table, matrix(unlist(list[c(display)]), 1, NROW(display), byrow = T))
}
table[,NROW(display)] <- stringr::str_extract(table[,NROW(display)], ".+,")

# Table of packages
kable(table[-1,], format = "html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


#Data
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
