library(raster)
library(rasterVis)
library(gridExtra)

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion")
a <- stack("Reflectance_27_July_2005")

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion")
b <- stack("Reg_27_July_2005.tif")


for (x in 1:152) {
  i = max(values(a[[x]]))
  z = max(values(b[[x]]))
  print(a[[x]]@data@names)
  print(i-z)
}









for (x in 1:152) {
  m <- rbind(c(1, 2))
  layout(m)
  plot(a[[x]], main=a[[x]]@data@names)
  plot(b[[x]])
}


m <- rbind(c(1, 2))
layout(m)
plot(a[[44]], main=a[[44]]@data@names)
plot(b[[44]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[24]], main=a[[24]]@data@names)
plot(b[[24]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[5]], main=a[[5]]@data@names)
plot(b[[5]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[7]], main=a[[7]]@data@names)
plot(b[[7]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[14]], main=a[[14]]@data@names)
plot(b[[14]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[1]], main=a[[1]]@data@names)
plot(b[[1]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[30]], main=a[[30]]@data@names)
plot(b[[30]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[63]], main=a[[63]]@data@names)
plot(b[[63]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[74]], main=a[[74]]@data@names)
plot(b[[74]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[100]], main=a[[100]]@data@names)
plot(b[[100]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[40]], main=a[[40]]@data@names)
plot(b[[40]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[105]], main=a[[105]]@data@names)
plot(b[[105]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[43]], main=a[[43]]@data@names)
plot(b[[43]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[78]], main=a[[78]]@data@names)
plot(b[[78]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[38]], main=a[[38]]@data@names)
plot(b[[38]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[25]], main=a[[25]]@data@names)
plot(b[[25]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[26]], main=a[[26]]@data@names)
plot(b[[26]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[8]], main=a[[8]]@data@names)
plot(b[[8]])

m <- rbind(c(1, 2))
layout(m)
plot(a[[74]], main=a[[74]]@data@names)
plot(b[[74]])