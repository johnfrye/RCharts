pdf_datei<-"karten_europa_choropleth_laender.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=13,height=11)

par(omi=c(0,0,0,0),mai=c(0,0,0,0),family="Lato Light",las=1)  
library(maptools)
library(RColorBrewer)
library(sp)
library(rgdal) # für spTransform

# Daten einlesen und Grafik vorbereiten

daten<-read.csv("daten/prop.table EVS cntr.csv",sep=";",dec=",")
daten[is.na(daten$ATHE),"ATHE"]<--9
data(wrld_simpl) 
w=wrld_simpl[!(wrld_simpl@data[,"ISO2"] %in% daten$Country),] 
w=w[w@data[,"NAME"] != "Antarctica",] 
m=spTransform(w,CRS=CRS("+proj=merc"))

# Grafik erstellen

plot(m,xlim=c(-2000000,5000000),ylim=c(4000000,10000000),
	col=rgb(160,160,160,100,maxColorValue=255),border=F)

x<-readShapeSpatial("daten/NUTS-2010/NUTS_RG_60M_2010.shp",
	proj4string=CRS("+proj=longlat"))
y<-x[x$NUTS_ID %in% daten$Country,]
m=spTransform(y,CRS=CRS("+proj=merc")) 

klassen<-c(-10,0,8,16,24,32,48)
farbpalette<-c("cornflowerblue",brewer.pal(5,"Reds"))

id<-m$NUTS_ID
n<-length(id)
position<-vector()
for (i in 1:n){
	position[i]<-match(m$NUTS_ID[i], daten$Country)
}
farb_nr<-cut(daten$ATHE[position],klassen)
levels(farb_nr)<-c("k. Angabe"," 0 bis 8","> 8 bis 16",">16 bis 24",">24 bis 32",">32 bis 48")
plot(m,col=farbpalette[farb_nr],border="white",add=T)

legend("bottomleft",levels(farb_nr),cex=1.45,border=F,bty="n",
	fill= farbpalette,text.col="black")

# Betitelung

mtext("Atheismus in Europa",at=-1300000,cex=2,adj=0,line=-3)
mtext("Quelle: European Values Study 2008",1,at=3300000,cex=1.7,adj=0,line=-2.3,font=3)
dev.off()

