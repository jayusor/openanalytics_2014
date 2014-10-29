#########################################################
#
# Jorge Ayuso Rejas
# Open Analytics 2014
# https://github.com/jayusor/openanalytics_2014
#
#########################################################

rm(list = ls());gc()
library(leafletR)
source("2.analizar_vinos.R")

ddoo<-ddoo[,c("OBJECTID","CAL_DS_NOM","N","Puntos","Precio")]

# ddoo<-ddoo[sample(1:80,4),]

ddoo$Precio2<-ddoo$Precio
ddoo$Precio<-paste0(round(ddoo$Precio,2),"€")
ddoo$Puntos<-round(ddoo$Puntos,2)

q.style <- styleGrad(prop="Precio2", breaks=seq(0,50,by = 5), 
                     style.val=rev(heat.colors(10)), leg="Precio", 
                     fill.alpha=0.7)

qfile<-"/home/jorge/Dropbox/OpenAnalytics2014/leaftR"
q.dat <- toGeoJSON(data=ddoo, dest=qfile, name="ddoo")


folder<-"/home/jorge/Dropbox/OpenAnalytics2014/leaftR"
file.create(folder,showWarnings = F)


q.map <-leaflet(data = q.dat,dest = folder,base.map = "toner",incl.data = TRUE,
                style = q.style,                
                popup=c("CAL_DS_NOM","Precio","Puntos","N"))

q.map
