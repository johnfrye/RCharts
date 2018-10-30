pdf_datei<-"zeitreihen_prognoseintervall_1x5_index.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=11,height=7)

par(mfcol=c(1,5),omi=c(1.0,0.25,1.45,0.25),mai=c(0,0.75,0.25,0),
	family="Lato Light",las=1)

# Daten einlesen und Grafik vorbereiten

UNPop<-read.csv("daten/UNPop.csv")

auswahl<-c("World","Northern America","China","Africa","Germany")
ymin<-rep(0, 5)
ymax<-rep(500, 5)
titel<-c("Welt","USA","China","Afrika","Deutschland")

for (i in 1:length(auswahl)) {
source("skripte/inc_prognoseintervall_05_index.r")
mtext(titel[i],side=3,adj=0,line=1,cex=1.1,font=3)

if (i==1)
{
legend(1900,-70,c("obere Prognose","mittlere Prognose","untere Prognose"),
	fill=c("grey","grey","grey"),border=F,pch=15,xpd=NA,
	col=c("black","white","orange"),bty="n",cex=1.6,ncol=3)
}
}

# Betitelung

mtext("Bevölkerungsprognosen der UN",3,line=7,adj=0,cex=2.25,family="Lato Black",outer=T)
mtext("2010=100, Fünfjahreswerte",3,line=3.5,adj=0,cex=1.75,font=3,outer=T)
mtext("Quelle: UN World Population Prospects: The 2010 Revision",1,line=5,adj=1.0,cex=0.95,font=3,outer=T)
dev.off()
