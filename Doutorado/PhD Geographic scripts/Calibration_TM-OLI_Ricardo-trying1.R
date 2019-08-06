setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5')
 
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

b7 = stack(list7)

b7
plot(b7[[1]])

new_ext = drawExtent()
b7 = crop(b7, new_ext)

b7_median = b7
for (i in 1:nlayers(b7)) {
  b7_median[[i]] = focal(b7[[i]], w = matrix(1,nrow=55,ncol=55), fun = function(x) median(x, na.rm=T), pad=T)
}
plot(b7_median)

b7_norm = b7 - b7_median
plot(b7_norm)
click(b7_norm)


#Abrir os shapes para amostrar pixels (100 pontos por shape)
library(rgdal)
crt <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Controle_points")
b1yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B1y_pontos")
b3yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B3y_pontos")

crt = spTransform(crt, crs(b7))
b1yr = spTransform(b1yr, crs(b7))
b3yr = spTransform(b3yr, crs(b7))

#Extrair valor dos pixels
crtdf <- extract(b7_norm, crt)#[1,])
b1yrdf <- extract(b7_norm, b1yr)#[1,])
b3yrdf <- extract(b7_norm, b3yr)#[1,])

plot(1:30, crtdf, type="l")
plot(1:30, b1yrdf, type="l")
plot(1:30, b3yrdf, type="l")

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda7 <- banda[,c(2, 3, 6, 9)]
banda7$data = substr(banda7$data, 18, 25)

plot(banda7$controle~banda7$data)

#Infravermelho medio 2
gg <- melt(banda7, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_boxplot()+
  labs(title="BANDA (IR MÃ©dio 2)", fill= "Plot",x="Data",y="ReflectÃ¢ncia")+
  annotate("rect", xmin = 10, xmax = 29, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  theme(axis.text.x = element_text(angle = 90))

irmedio2_md <- banda7
irmedio2_md$data = substr(irmedio2_md$data, 1, 4)


irmedio2_md = irmedio2_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle, na.rm = TRUE),
            b3yr = median(b3yr, na.rm = TRUE),
            b1yr = median(b1yr, na.rm = TRUE))

gg <- melt(irmedio2_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="Mediana BANDA (IR MÃ©dio 2)", fill= "Plot",x="Data",y="ReflectÃ¢ncia")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0, ymax = Inf, alpha = .2, fill = "red")+
  geom_vline(xintercept = 13, color = "red", linetype = "dashed")+
  annotate("text", x = 8, y = 25, label = "Experimento de fogo")+
  annotate("text", x = 12.5, y = 25, label = "TM")+
  annotate("text", x = 13.5, y = 25, label = "OLI")+
  theme(axis.text.x = element_text(angle = 90))
