# inc_prognoseintervall_05_index.r
Land<-subset(UNPop,UNPop$Country==auswahl[i] & UNPop$Variant=="Medium variant")

Prognosen<-subset(UNPop,UNPop$Country == auswahl[i] & Year >= 2010)
Prognose_L<-subset(Prognosen,Prognosen$Variant=="Low variant")$Value/1000
Prognose_M<-subset(Prognosen,Prognosen$Variant=="Medium variant")$Value/1000
Prognose_H<-subset(Prognosen,Prognosen$Variant=="High variant")$Value/1000
Jahre<-seq(2010,2100,by=5)
attach(Land)
basis<-(Value[13]/1000)

plot(axes=F,type="n",xlab="",ylab="",Year,Value/1000,ylim=c(ymin[i],ymax[i]))
py<-c(0,100,200,300,400,500)
abline(h=py[2:6], col="lightgray",lty="dotted")
axis(1,tck=-0.01,col="grey",at=c(1950,2010,2100),cex.axis=1.2) 
py<-c(0,100,200,300,400,500)
if (auswahl[i]=="World")
{
axis(2,tck=-0.01,col="grey",at=py,labels=format(py,big.mark="."),
	cex.axis=1.2) 
}
xx<-c(Jahre,rev(Jahre))
yy<-c(100*Prognose_H/basis,rev(100*Prognose_L/basis))

polygon(xx,yy,col=rgb(192,192,192,maxColorValue=255),border=F)

lines(Year,100*(Value/1000)/basis,col="grey",lwd=2)
lines(Jahre,100*Prognose_H/basis,col="black",lwd=2)
lines(Jahre,100*Prognose_L/basis,col="orange",lwd=2)
lines(Jahre,100*Prognose_M/basis,col="white",lwd=2)


