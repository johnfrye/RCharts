pdf_datei<-"kreisdiagramme_spiechart.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=15,height=11)

par(omi=c(0.5,0.5,0.75,0.5),mai=c(0.1,0.1,0.1,0.1),
	family="Lato Light",las=1)
library(RColorBrewer)

# Daten einlesen und Grafik vorbereiten

x<-read.xlsx("daten/Healthcare_costs.xlsx",1)
attach(x)
n<-nrow(x)
faktor<-max(sqrt(Acosts60))/0.8

# Grafik definieren und weitere Elemente

plot.new()
f0<-rep(NA,n)
farben<-brewer.pal(n,"Set3")
for (i in 1:n)
{
par(new=T)
r<-col2rgb(farben[i])[1]
g<-col2rgb(farben[i])[2]
b<-col2rgb(farben[i])[3]
f0[i]<-rgb(r,g,b,190,maxColorValue=255)
wert<-format(Total60/1000000,digits=1)
komplett<-paste(Disease,": ",wert," Mio. $",sep="")
if (Acosts60[i] == max(Acosts60)) {bez<-komplett} else {bez<-NA}

# Segmente erstellen

pie(Patients60,border=NA,radius=sqrt(Acosts60[i])/faktor,col=f0,
	labels=bez,cex=1.8)
par(new=T)
r<-col2rgb(farben[i])[1]
g<-col2rgb(farben[i])[2]
b<-col2rgb(farben[i])[3]
f0[i]<-rgb(r,g,b,maxColorValue=255)
pie(Patients60,border=NA,radius=sqrt(Pcosts60[i])/faktor,col=f0,labels=NA)
f0<-rep(NA,n)
}

# Betitelung

mtext("The Cost of Getting Sick",3,line=-1,adj=0,cex=3.5,family="Lato Black",outer=T)
mtext("The Medical Expenditure Panel Survey.  Age: 60, Total Costs:  41.4 Mio. US $",3,line=-3.6,adj=0,cex=1.75,outer=T)
mtext("Inside: Personal Costs.  Outside: Insurer Costs.",1,line=0,adj=0,cex=1.75,outer=T,font=3)
mtext("visualization.geblogs.com/visualization/health_costs/",1,line=0,adj=1.0,cex=1.75,outer=T,font=3)
dev.off()

