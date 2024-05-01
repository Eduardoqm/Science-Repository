setwd("/mnt/data/dados_publicos/Documents/data_geo/Base_raster/Dados Spera/")
dir()
spera_tang=read.csv("Spera_Tanguro.csv")
head(spera_tang)
ggplot(spera_tang, aes(ano)) +                    # basic graphical object
geom_line(aes(y=soja), colour="red") +  # first layer
geom_line(aes(y=milho), colour="green")+  # second layer
geom_line(aes(y=soja.milho), colour="blue")+ 
ylab("Área Plantada (ha)")+xlab("Anos")+
  scale_x_continuous( breaks=c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2015) )+
  theme(text = element_text(size=13),
        axis.text.x = element_text(angle=30, vjust=1))

soja=data.frame(ano=spera_tang$ano,Area=spera_tang$soja,var="Soja")
sojamilho=data.frame(ano=spera_tang$ano,Area=spera_tang$soja.milho,var="Soja-milho")
milho=data.frame(ano=spera_tang$ano,Area=spera_tang$milho,var="Milho")
dff=rbind(soja,sojamilho,milho)

ggplot(dff, aes(ano,Area, fill=var)) + geom_bar(stat="identity") +
ylab("Área Plantada (ha)")+xlab("Anos")+
  scale_x_continuous( breaks=c(2001,2003,2005,2007,2009,2011,2013,2015) )+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, vjust=1))+
  theme_bw()
