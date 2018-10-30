pdf_datei<-"zeitreihen_quartal_saeulen.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=14,height=7)

library(gplots)
par(omi=c(0.65,0.75,0.95,0.75),mai=c(0.9,0,0.25,0.02),
	fg="cornsilk",bg="cornsilk",family="Lato Light",las=1)  

# Daten einlesen und Grafik vorbereiten

bip<-read.xls("daten/bip_deutschland_quartal.xlsx",sheet=2)
x<-rev(bip$preisbereinigt)
t<-unique(bip$jahr)

# Grafik erstellen und weitere Elemente

par(mfcol=c(1,length(t)))
for (i in length(t):1)
{
xt<-subset(bip$preisbereinigt,bip$jahr == t[i])
farben<-rep("blue4",length(xt))
for (j in 1:length(xt)) if(xt[j]<0) farben[j]<-"coral4"
barplot2(rev(xt),border=NA,bty="n",col=rev(farben),ylim=c(-4,2),
	axes=F,prcol="bisque1")
if (i==length(t)) axis(2,col="cornsilk",cex.axis=1.25,at=c(-4:2),
	labels=c("-4%","-3%","-2%","-1%","0%","1%","2%"))
mtext(t[i],1,line=2,col=rgb(64,64,64,maxColorValue=255),cex=1.25)
}

# Betitelung

mtext("Bruttoinandsprodukt von Deutschland 2000–2011",3,line=2.5,adj=0,cex=2,family="Lato Black",col="Black",outer=T)
mtext("Veränderungsraten zum Vorquartal in Prozent, preisbereinigt, Kettenindex, Quartalswerte",3,line=-0.5,adj=0,cex=1.5,font=3,col="Black",outer=T)
mtext("Quelle: destatis, Konjunkturindikatoren",1,line=1,adj=1,cex=1.25,font=3,col="Black",outer=T)
mtext("saison- und kalenderbereinigte Werte nach Census X-12-ARIMA",1,line=1,adj=0,cex=1.25,font=3,col="Black",outer=T)
dev.off()

