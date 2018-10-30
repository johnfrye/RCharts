pdf_datei<-"streudiagramme_quadranten.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=11.69,height=9)

par(mar=c(4,4,0.5,2),omi=c(0.5,0.5,1,0),family="Lato Light",las=1)
library(RColorBrewer)

# Daten einlesen und Grafik vorbereiten

daten<-read.csv(file="daten/BetterLifeIndex_Data_2011V6.csv",head=F,
	sep=";",dec=",",skip=6)
daten<-daten[2:36,]
attach(daten)

x<-as.numeric(V16)
y<-as.numeric(V15)
xbez<-"Self-Reported Health (Scale from 0 to 100)"
ybez<-"Life Expectancy"

# Grafik definieren und weitere Elemente

plot(type="n",xlab=xbez,ylab=ybez,x, y,xlim=c(30,90),ylim=c(72,83),axes=F)
axis(1,col=par("bg"),col.ticks="grey81",lwd.ticks=0.5,tck=-0.025)
axis(2,col=par("bg"),col.ticks="grey81",lwd.ticks=0.5,tck=-0.025)

f1<-brewer.pal(5,"PiYG")[5]
f2<-brewer.pal(5,"PiYG")[4]
f3<-brewer.pal(5,"PiYG")[1]
f4<-brewer.pal(5,"PiYG")[2]

p1<-subset(daten[c("V2","V16","V15")],x > mean(x) & y > mean(y))
p2<-subset(daten[c("V2","V16","V15")],x < mean(x) & y > mean(y))
p3<-subset(daten[c("V2","V16","V15")],x < mean(x) & y < mean(y))
p4<-subset(daten[c("V2","V16","V15")],x > mean(x) & y < mean(y))

n1<-nrow(p1)
n2<-nrow(p2)
n3<-nrow(p3)
n4<-nrow(p4)

symbols(p1[,2:3],bg=f1,circles=rep(1,n1),inches=0.3,add=T,xpd=T,fg="white")
symbols(p2[,2:3],bg=f2,circles=rep(1,n2),inches=0.3,add=T,xpd=T,fg="white")
symbols(p3[,2:3],bg=f3,circles=rep(1,n3),inches=0.3,add=T,xpd=T,fg="white")
symbols(p4[,2:3],bg=f4,circles=rep(1,n4),inches=0.3,add=T,xpd=T,fg="white")

text(p2[,2:3],as.matrix(p2$V2),cex=0.9,pos=1,offset=1.75)
text(p4[,2:3],as.matrix(p4$V2),cex=0.9,pos=1,offset=1.75)

abline(v=mean(x,na.rm=T),col="black",lty=3)
abline(h=mean(y,na.rm=T),col="black",lty=3)

text(min(V16),mean(V15)+0.005*mean(V15),"high",family="Lato Black",adj=0)
text(min(V16),mean(V15)-0.005*mean(V15),"low",family="Lato Black",adj=0)
text(mean(V16)-0.001*mean(V16),72,"high",family="Lato Black",pos=4)
text(mean(V16)+0.001*mean(V16),72,"low",family="Lato Black",pos=2)

# Betitelung

mtext("Life Expectancy and Self-Reported Health (OECD)",3,adj=0,line=2.5,cex=2.0,family="Lato Black")
mtext("",3,adj=0,line=0,cex=1.0,font=3)
mtext("Source: oecdbetterlifeindex.org",1,line=4,adj=1,cex=0.95,font=3)
dev.off()


