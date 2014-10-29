#########################################################
#
# Jorge Ayuso Rejas
# Open Analytics 2014
# Basado en Modelos predictivos con el paquete caret (Jesús Herranz Valera)
# http://r-es.org/Programa+de+las+VI+Jornadas
#
#########################################################

rm(list = ls());gc()
library(ggplot2)
library(reshape2)
library(caret)
library(corrplot)
library(pROC) 
library(doParallel)


registerDoParallel(cores = detectCores())

tinto<-read.csv("datos/winequality-red.csv",sep = ";")
tinto$tipo<-"Tinto"

blanco<-read.csv("datos/winequality-white.csv",sep = ";")
blanco$tipo<-"Blanco"

xx.all<-rbind(tinto,blanco)
rm(tinto,blanco)

xx.all$tipo<-NULL

## Posición de Y en el dataframe (columna)

xx.all$quality<-ordered(xx.all$quality,as.character(3:9))

indY = which ( "quality" == names(xx.all) )

#####################

corrplot(cor(xx.all[,-indY]),order="hclust")

## Muestra 80% training
itrain<- createDataPartition(xx.all$quality, p = .8)

xx.train<-xx.all[ itrain$Resample1, ]
xx.test <-xx.all[-itrain$Resample1, ]

nrow(xx.train)/nrow(xx.all)

### ESCALO
w.trans = preProcess (  xx.train [ , -indY ] , method = c ("center", "scale") )
w.trans

xx.train[ , -indY]  = predict( w.trans , xx.train[ , -indY]  )
xx.test [ , -indY]  = predict( w.trans , xx.test[ , -indY]  )

################################################################################
################################################################################
## Suppor Vector Machine
################################################################################
################################################################################

cv.ctrl = trainControl ( method = "cv", number = 5)


## Parámetros a explorar en SVM                             
svmGrid     = expand.grid ( .C = c ( 1, 5, 10, 50,100) ,
                            .sigma = c (1.8,2,2.2) )      


## Construcción del Modelo Predictivo
svm.fit = train ( xx.train[ , -indY ] , xx.train$quality , 
                  method = "svmRadial", 
                  trControl = cv.ctrl,
                  tuneGrid = svmGrid
                  )

svm.fit
names(svm.fit)

#################################################################
## Parámetros óptimos

svm.fit$bestTune

## Resultados
head(svm.fit$results)

## Gráfico del AUC respecto a los 2 parámetros

plot(svm.fit) 

plot( svm.fit, plotType="level" )


#################################################################
## Modelo Final, construido con los parámetros óptimos

svm.fit$finalModel 

class(svm.fit)
class(svm.fit$finalModel)   


#################################################################
## Predicciones del Modelo Final. Evaluación del Modelo

## Clases predichas
pred.train.class = predict ( svm.fit$finalModel, newdata = xx.train [ , -indY] )
pred.test.class  = predict ( svm.fit$finalModel, newdata = xx.test [ , -indY] )
head(pred.test.class)

confusionMatrix ( pred.test.class , xx.test$quality )
confusionMatrix ( pred.train.class , xx.train$quality )

svm.imp = varImp ( svm.fit , scale = F )
svm.imp

## Importancia de todas las variables
head(svm.imp$importance)
dim(svm.imp$importance)

# save.image("post_svm.Rdata")

