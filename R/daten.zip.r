pdf_datei<-"kreisdiagramme_sitzverteilung.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=10,height=3.75)

par(omi=c(0.5,0.5,1,0.5),mai=c(0,0,0,0),xpd=T,mfcol=c(1,2),
	family="Lato Light")
library(plotrix)

# Grafik definieren

plot(1:5,type="n",axes=F,xlab="",ylab="",xlim=c(1,5),ylim=c(1,10))
sitze<-c(51,54,61,222,226)
bez<-c(sitze,""); segmente<-50*sitze/sum(sitze)
werte<-c(segmente,50); scheibe<-100
sfarbe<-c("white", "white", "black", "white", "white")

# Grafik erstellen

halbkreis<-floating.pie(3,1,werte,border="white",radius=1.9,
	xpd=F,col=c("green","pink","yellow","red","black",par("bg")))
pie.labels(3,1,halbkreis,bez,bg=NA,border=NA,radius=1.5,cex=2,col=sfarbe)
floating.pie(3,1,scheibe,border="white",col=par("bg"),radius=0.7,xpd=F)
mtext("16. Deutscher Bundestag",3,line=0,adj=0.5,font=3,cex=1.3)

par(xpd=T)
legend(1,0.5,c("CDU/CSU","SPD","FDP","Die Linke","Bündnis 90/Die Grünen"),
	border=F,pch=15,col=c("black","red","yellow","pink","green"),
	bty="n",cex=0.8,xpd=NA,ncol=3)
par(xpd=F)

# Grafik definieren

plot(1:5,type="n",axes=F,xlab="",ylab="",xlim=c(1,5),ylim=c(1,10))
sitze<-c(68,76,93,146,237)
bez<-c(sitze,""); segmente<-50*sitze/sum(sitze)
werte<-c(segmente,50); scheibe<-100

# Grafik erstellen

halbkreis<-floating.pie(3,1,werte,border="white",radius=1.9,xpd=F,
	col=c("green","pink","yellow","red","black",par("bg")))
pie.labels(3,1,halbkreis,bez,bg=NA,border=NA,radius=1.5,cex=2,col=sfarbe)
floating.pie(3,1,scheibe,border="white",col=par("bg"),radius=0.7,xpd=F)
mtext("17. Deutscher Bundestag",3,line=0,adj=0.5,font=3,cex=1.3)

# Betitelung

mtext("Sitzverteilungen im Bundestag",3,line=3,adj=0,family="Lato Black",outer=T,cex=1.8)
mtext("Quelle: www.bundestag.de",1,line=1,adj=1.0,cex=0.8,font=3,outer=T)
dev.off()

