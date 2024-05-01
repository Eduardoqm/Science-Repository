library(rgdal)
library(raster)
library(rasterVis)
library(caret)
library(snow)

#Abrir a imagem do drone
img <- raster("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\Doutorado\\Disciplinas\\INPE - Detec e Aná de Padrões de Mudanças de Uso e Cobert da Terra\\campo_ser-415\\horta2.tif")

r <- raster(nrow=50, ncol=25)

levelplot(img, margin = FALSE, main = "Horta")

#execute the kMeans function on the image values (indicated by the squared bracket) 
kMeansResult <- kmeans(img[], centers=6)

#create a dummy raster using the first layer of our image 
result <- raster(img)
result <- setValues(result, kMeansResult$cluster)

#plot the result
levelplot(result, margin = FALSE, main = "Horta")
plot(result)
#levelplot(result, margin = FALSE, par.settings = RdBuTheme, main = "Horta")

plot(result, col=c("red", "orange", "black", "yellow", "darkgreen"))

df <- as.data.frame(result)
colnames(df) <- c("classes")

summary(df2$)
