#########################################################
#
# Jorge Ayuso Rejas
# Open Analytics 2014
# https://github.com/jayusor/openanalytics_2014
#
#########################################################

rm(list = ls());gc()
library(RCurl)
library(RSelenium)
library(XML)
library(data.table)
library(parallel)
url<-"http://www.verema.com/vinos/portada"

checkForServer()
Sys.sleep(.5)
startServer()
Sys.sleep(2)

parsear_tablas<-function(j){
  
  doc<-getURI(j)
  error<-1
  intentos<-0

  while(error==1 & intentos<10){
    
    tabla<-readHTMLTable(doc ,stringsAsFactors = FALSE, encoding="UTF-8",header = TRUE)
    if(!inherits(try(tabla[[1]]), "try-error") ) error<-0
    intentos<-intentos+1
    
  }

  if(inherits(try(tabla[[1]]), "try-error") ) return()     

  if(is.null(tabla)|length(tabla)==0|is.null(tabla[[1]])) return()
  tabla<-try(tabla[[1]])
  if(length(tabla)<2) return(NULL)
  
  tabla$Vino<-gsub("\\n\\(function.*$","",tabla$Vino)
  cual<-grep("Anterior.*Siguiente",tabla$Vino)
  if(length(cual)>0) tabla<-tabla[-cual,]
  return(as.data.table(tabla))
}


leer<-function(i){
  firefox$navigate(url)
  Sys.sleep(2)
  firefox$findElement("xpath",paste0('//*[@id="producto_ddoo_id"]/option[.=\'',i,'\']'))$clickElement()
  Sys.sleep(1)
  firefox$findElement("xpath",'//*[@id="leftflex-cell-col300"]/div/div[1]/div/form/input[4]')$clickElement()
  
  url_ddoo<-firefox$getCurrentUrl()[[1]]
  
  vinos_total<-firefox$findElement("xpath",'//*[@id="leftflex-cell-col300"]/h1/span')
  vinos_total<-as.numeric(gsub("^\\W*([0-9]+).*$","\\1",vinos_total$getElementText()[[1]]))
  
  if(inherits(try(vinos_total==0), "try-error")|is.na(vinos_total) ) browser()      
  
  if(vinos_total==0) return()
  
  if(vinos_total>50){
    if(vinos_total>500){
      url_ddoo<-paste0(url_ddoo,apply(expand.grid(1:7,1:10),1,function(x) paste0("&rango_precio=",x[1],'&page=',x[2])))
    }else{
      url_ddoo<-paste0(url_ddoo,paste0("&page=",1:ceiling(vinos_total/50)))  
    }    
  }
  
  url_ddoo<-gsub("&page=1$","",url_ddoo)
  
  zoom<-mclapply(url_ddoo,parsear_tablas,mc.cores = 6)   #mclapply solo funciona en linux/unix poner lapply
  zoom<-rbindlist(zoom)
  
  if(length(zoom)==0) return()
  zoom$DO<-i
  return(zoom)
  }

firefox <- remoteDriver()
# firefox <- remoteDriver(browserName = "phantomjs")

firefox$open()
firefox$navigate(url)

ddoo<-firefox$findElements("xpath",'//*[@id="producto_ddoo_id"]/option')
Sys.sleep(3)

nombres_ddoo<-sapply(ddoo,function(x) x$getElementText())
nombres_ddoo<-do.call(c,nombres_ddoo)
nombres_ddoo<-nombres_ddoo[grep("^España",nombres_ddoo)]


todo<-lapply(nombres_ddoo,leer)


firefox$close()
firefox$closeServer()

todo<-rbindlist(todo)

save(todo,file=paste0(Sys.Date(),"_vinos.Rdata"))

