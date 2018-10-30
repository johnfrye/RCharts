pdf_datei<-"balkendiagramme_einfach.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=9,height=6.5)

par(omi=c(0.65,0.25,0.75,0.75),mai=c(0.3,2,0.35,0),mgp=c(3,3,0),
	family="Lato Light", las=1)  

# Daten einlesen und Grafik vorbereiten

ipsos<-read.xls("daten/ipsos.xlsx")
sort.ipsos<-ipsos[order(ipsos$Wert) ,]
attach(sort.ipsos)

# Grafik erstellen

x<-barplot(Wert,names.arg=F,horiz=T,border=NA,xlim=c(0,100),
	col="grey", cex.names=0.85,axes=F)

# Grafik beschriften

for (i in 1:length(Land))
{
if (Land[i] %in% c("Deutschland","Brasilien")) 
	{schrift<-"Lato Black"} else {schrift<-"Lato Light"}
text(-8,x[i],Land[i],xpd=T,adj=1,cex=0.85,family=schrift)
text(-3.5,x[i],Wert[i],xpd=T,adj=1,cex=0.85,family=schrift)
}

# weitere Elemente

rect(0,-0.5,20,28,col=rgb(191,239,255,80,maxColorValue=255),border=NA)
rect(20,-0.5,40,28,col=rgb(191,239,255,120,maxColorValue=255),border=NA)
rect(40,-0.5,60,28,col=rgb(191,239,255,80,maxColorValue=255),border=NA)
rect(60,-0.5,80,28,col=rgb(191,239,255,120,maxColorValue=255),border=NA)
rect(80,-0.5,100,28,col=rgb(191,239,255,80,maxColorValue=255),border=NA)

wert2<-c(0,0,0,0,27,0,0,0,0,0,0,0,0,84,0,0)
farbe2<-rgb(255,0,210,maxColorValue=255)
x2<-barplot(wert2,names.arg=F,horiz=T,border=NA,xlim=c(0,100),
	col=farbe2,cex.names=0.85,axes=F,add=T)

arrows(45,-0.5,45,20.5,lwd=1.5,length=0,xpd=T,col="skyblue3") 
arrows(45,-0.5,45,-0.75,lwd=3,length=0,xpd=T)
arrows(45,20.5,45,20.75,lwd=3,length=0,xpd=T)
text(41,20.5,"Durchschnitt",adj=1,xpd=T,cex=0.65,font=3)
text(44,20.5,"45",adj=1,xpd=T,cex=0.65,family="Lato",font=4)
text(100,20.5,"Alle Angaben in Prozent",adj=1,xpd=T,cex=0.65,font=3)
mtext(c(0,20,40,60,80,100),at=c(0,20,40,60,80,100),1,line=0,cex=0.80)

# Betitelung

mtext("'Ich glaube fest an Gott oder ein höheres Wesen'",3,line=1.3,adj=0,cex=1.2,family="Lato Black",outer=T)
mtext("...sagten 2010 in:",3,line=-0.4,adj=0,cex=0.9,outer=T)
mtext("Quelle: www.ipsos-na.com, Design: Stefan Fichtel, ixtract",1,line=1,adj=1.0,cex=0.65,outer=T,font=3)
dev.off()

