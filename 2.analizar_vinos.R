#########################################################
#
# Jorge Ayuso Rejas
# Open Analytics 2014
# https://github.com/jayusor/openanalytics_2014
#
#########################################################
rm(list = ls());gc()

library(data.table)
library(ggplot2)
library(rgdal)
library(stringdist)
library(maps)
library(ggmap)


load("datos/2014-10-26_vinos.Rdata")

todo$Puntos<-as.numeric(gsub(",","\\.",todo$Puntos))
todo<-todo[todo$Precio!="-",]
todo$Precio<-ordered(todo$Precio,c("0-5","5-10","10-20","20-30","30-50","50-100","100+"))
todo$Precio2<-sapply(strsplit(as.character(todo$Precio),"-"),function(x) mean(as.numeric(gsub("\\+","",x),na.rm=T)))

todo$Añada<-as.numeric(todo$Añada)
table(todo$Añada)
todo$Añada[!(todo$Añada>=1800 & todo$Añada<=2014)]<-NA
table(todo$Añada)

# Borro los vinos que no me interesan
todo$DO<-gsub("España\\s+?-?\\s+?","",todo$DO)
todo<-todo[-grep("^(sin|otra)",tolower(todo$DO)),]
todo<-todo[-grep("^v\\.t\\.|vino(s)? de la tierra|sin d\\.o\\.|varias d\\.o\\.|^igp",tolower(todo$DO)),]

pairs(todo[,c("Añada","Puntos","Precio"),with=F])

ggplot(todo[!is.na(todo$Precio),],aes(x=Precio,y=Puntos))+geom_boxplot(fill="deepskyblue2")+theme_bw()

ggplot(todo,aes(x=Añada,y=Puntos))+geom_point()+geom_smooth(se=FALSE,lwd=1,col="orange")+theme_bw()

########

ddoo<-readOGR("CalidadDiferenciada_tcm7-212106",layer = "CalidadDiferenciada")
summary(ddoo)

ddoo<-ddoo[ddoo$FAM_DS_NOM=="Vinos",]

## Simplificar la geometría para hacerla menos pesada
library(rgeos)

# plot(ddoo)

todo$DO_id<-tolower(iconv(gsub("(\\w\\.)+(\\s+)?","",todo$DO),to = "ASCII//TRANSLIT"))
todo$DO_id<-gsub("\\W"," ",todo$DO_id)

ddoo$CAL_DS_NOM_id<-tolower(iconv(gsub("(\\w\\.)+(\\s+)?","",ddoo$CAL_DS_NOM),to = "ASCII//TRANSLIT"))
ddoo$CAL_DS_NOM_id<-gsub("\\W"," ",ddoo$CAL_DS_NOM_id)

ddoo$CAL_DS_NOM_id<-gsub("\\s?chacoli de.*$","",ddoo$CAL_DS_NOM_id)
todo$DO_id[grep("navarra",todo$DO_id)]<-"vino navarra"
todo$DO_id[grep("rioja",todo$DO_id)]<-"cvino de rioja"

unique(todo$DO_id[grep("rioja",todo$DO_id)])
unique(ddoo$CAL_DS_NOM_id[grep("rioja",ddoo$CAL_DS_NOM_id)])

aux<-merge(ddoo@data[,c("OBJECTID","CAL_DS_NOM_id")],
                 todo[,.N,by=DO_id],
                 by.x = "CAL_DS_NOM_id",
                 by.y = "DO_id",all.x = T)

todo[!todo$DO_id %in% aux$CAL_DS_NOM_id[!is.na(aux$N)] ,]

(cuales<-sapply(unique(todo$DO_id[!todo$DO_id %in% aux$CAL_DS_NOM_id[!is.na(aux$N)] ]),function(x){
  amatch(x,ddoo$CAL_DS_NOM_id,maxDist = Inf)
}))

prueba<-data.frame(
           ID=unique(todo$DO_id[!todo$DO_id %in% aux$CAL_DS_NOM_id[!is.na(aux$N)] ]),
           Mach=ddoo$CAL_DS_NOM_id[cuales]
           )
prueba

bien<-c(1,4,5,6,7,8,10,12,13)
prueba[-bien,]
prueba<-prueba[bien,]

rownames(prueba)<-prueba$ID

todo$DO_id[todo$DO_id %in% prueba$ID]<-as.character(prueba[todo$DO_id[todo$DO_id %in% prueba$ID],"Mach"])

aux<-merge(ddoo@data[,c("OBJECTID","CAL_DS_NOM_id")],
           todo[,.N,by=DO_id],
           by.x = "CAL_DS_NOM_id",
           by.y = "DO_id",all.x = T)

aux[which(is.na(aux$N)),]


ddoo<-ddoo[order(ddoo$CAL_DS_NOM_id),]

ddoo@data<-merge(ddoo@data,
      todo[,list(.N,Puntos=mean(Puntos,na.rm = T),Precio=mean(Precio2)),by=DO_id],
      by.x = "CAL_DS_NOM_id",
      by.y = "DO_id",all.x = T)

ddoo<-ddoo[-which(is.na(ddoo@data$N)|is.na(ddoo@data$Puntos)|is.na(ddoo@data$Precio)),]



##################################3
## MAPAS
# plot(ddoo)

# map(regions = "spain",col="red")
# plot(ddoo[grep("Duero",ddoo$CAL_DS_NOM),],add=T)

map <- get_map(matrix(c(36, -10, 43, 3),ncol=2)[2:1,],maptype = "roadmap")
# map <- get_map("spain",zoom = 6,maptype = "roadmap")
ggmap(map,extent = "device")


ddoo_df<-fortify(ddoo,region ="OBJECTID" )
ddoo_df$id<-as.integer(ddoo_df$id)
ddoo_df<-merge(ddoo_df,ddoo@data,by.x = "id",by.y = "OBJECTID",all.x = T,sort=FALSE)

ggmap(map,extent = "device")+
  geom_polygon(aes(x=long,y=lat,group=group,fill=Puntos),data=ddoo_df,col="white",alpha = .4, size = .2)


ggmap(map,extent = "device")+
  geom_polygon(aes(x=long,y=lat,group=group,fill=Precio),data=ddoo_df,col="white",alpha = 1, size = .2)+ 
  scale_fill_gradient(low = "antiquewhite3", high = "red")




