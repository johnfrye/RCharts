pdf_datei<-"karten_staedte_1x2.pdf"
cairo_pdf(bg="grey98", pdf_datei,width=14,height=7)

par(omi=c(0,0,0.5,0),mai=c(0.5,0,0.5,0),mfcol=c(1,2),family="Lato Light",
	las=1)
library(maptools) # laedt auch sp fuer over
library(rgdal) # fuer spTransform

# Grafik vorbereiten und Daten einlesen

flib<-rgb(0,139,0,120,maxColorValue=255)
fpol<-rgb(139,0,0,120,maxColorValue=255)

nach_proj<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +over +no_defs"

von_proj<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=375,-111,431,0,0,0,0 +units=m +no_defs"
shapedatei<-"daten/london/greater_london_const_region.shp"
lon_verw<-readShapeSpatial(shapedatei,proj4string=CRS(von_proj))
lon_verw=spTransform(lon_verw,CRS=CRS(nach_proj))

# Grafik erstellen und weitere Elemente

plot(lon_verw,col=rgb(139,139,139,60,maxColorValue=255),border="white")
lon_osm<-readShapeSpatial("daten/london/london.osm-amenities.shp")
proj4string(lon_osm)<-proj4string(lon_verw)
inside1<-!is.na(over(lon_osm,lon_verw)) & lon_osm@data$type == "library"
inside2<-!is.na(over(lon_osm,lon_verw)) & lon_osm@data$type == "police"
points(lon_osm[inside1[ ,1],],col=flib,pch=15,cex=1.25,lwd=0)
points(lon_osm[inside2[ ,1],],col=fpol,pch=19,cex=1.25,lwd=0)

legend("bottomright",c("library","police"),col=c(flib,fpol),
	pch=c(15,19),bty="n",pt.cex=1.5,cex=1.5)
mtext(side=3,"In London ...",cex=1.5,col=rgb(64,64,64,maxColorValue=255))
mtext(side=1,"OSM-Daten: metro.teczno.com/",adj=1,cex=0.85)
mtext(side=1,"adm. Grenzen: parlvid.mysociety.org:81/os/",adj=0.1,cex=0.85)

# Daten einlesen und Grafik vorbereiten

von_proj<-"+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
shapedatei<-"daten/newyork/nybb.shp"
ny_verw<-readShapeSpatial(shapedatei,proj4string=CRS(von_proj))
ny_verw=spTransform(ny_verw,CRS=CRS(nach_proj))

# Grafik erstellen

plot(ny_verw,col=rgb(139,139,139,60,maxColorValue=255),border="white")

# Daten einlesen und weitere Elemente

ny_osm<-readShapeSpatial("daten/newyork/new-york.osm-amenities.shp")
proj4string(ny_osm)<-proj4string(ny_verw)
inside1<-!is.na(over(ny_osm,ny_verw)) & ny_osm@data$type == "library"
inside2<-!is.na(over(ny_osm,ny_verw)) & ny_osm@data$type == "police"
points(ny_osm[inside1[ ,1],],col=flib,pch=15,cex=1.25,lwd=0)
points(ny_osm[inside2[ ,1],],col=fpol,pch=19,cex=1.25,lwd=0)

# Betitelung

mtext(side=3,"... und in New York",cex=1.5,col=rgb(64,64,64,maxColorValue=255))
mtext(side=3,"'Amenities' in Open Street Map",outer=T,cex=2,family="Lato Black")
mtext(side=1,"adm. Grenzen: www.nyc.gov/html/dcp/html/bytes/dwndistricts.shtml",adj=0.9,cex=0.85)
dev.off()


