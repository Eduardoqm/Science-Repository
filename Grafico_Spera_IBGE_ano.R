setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/Comparação Spera IBGE")
dir()
total=read.csv("Total de area Spera_IBGE(km).csv")
head(total)


soja_ibge=data.frame(ano=total$ano,Area=total$area.ibge,var="Soja IBGE")
soja_spera=data.frame(ano=total$ano,Area=total$area.spera,var="Soja Spera")
dff2=rbind(soja_spera,soja_ibge)
dff2=na.exclude(dff2)

ggplot(dff2, aes(id_merge)) +                    # basic graphical object
geom_line(aes(y=soja_spera), colour="red") +  # first layer
geom_line(aes(y=soja_ibge), colour="green")+  # second layer
ylab("Área Plantada (ha)")+xlab("Municípios do Matopiba (ID)")

#Grafico de linha
ggplot(dff2, aes(ano,Area, fill=var)) + geom_line(stat="identity") +
  ylab("Área Plantada (ha)")+xlab("Anos")+
  scale_x_continuous( breaks=c(2001,2003,2005,2007,2009,2011,2013,2015) )+
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=0, vjust=1))+
  theme_bw()

#Grafico de barra
ggplot(dff2, aes(x=factor(ano), y=Area, fill=var)) + 
  geom_bar(stat="identity", position="dodge")+
ylab("Área Plantada (km²)")+xlab("Anos")+
theme(text = element_text(size=40),
     axis.text.x = element_text(angle=0, vjust=1))+
  theme_bw()

