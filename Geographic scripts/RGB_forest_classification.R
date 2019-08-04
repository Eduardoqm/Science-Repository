library(rgdal)
library(raster)
library(caret)
library(snow)

#Abrir a imagem do drone
#rgb=raster("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Drone_IPAM/Area1/RGB/rgb-201902[25-26-27].tif")
img <- brick("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Drone_IPAM/Area1/RGB/rgb-201902[25-26-27].tif")
names(img) <- paste0("B", c(1:5, 7)) 
#Abrir o vetor com coluna class
trainData <- shapefile("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Drone_IPAM/Area1/RGB/classpoly.shp")
responseCol <- "class"

#Extrair os valores dos pixels pelo vetor
dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))   
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}

#Filtrar numero de samples (gerar random samples)
nsamples <- 1000
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]

#Metodo de classificacao
modFit_rf <- train(as.factor(class) ~ B3 + B4 + B5, method = "rf", data = sdfAll)

#Classificacao
beginCluster()
preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
endCluster()



