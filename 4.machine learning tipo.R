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
library(kernlab)
registerDoParallel(cores = 4)

tinto<-read.csv("datos/winequality-red.csv",sep = ";")
tinto$tipo<-"Tinto"

blanco<-read.csv("datos/winequality-white.csv",sep = ";")
blanco$tipo<-"Blanco"

xx.all<-rbind(tinto,blanco)
rm(tinto,blanco)

xx.all$tipo<-as.factor(xx.all$tipo)

## Posición de Y en el dataframe (columna)
indY = which ( "tipo" == names(xx.all) )

#####################

ggplot(melt(xx.all,id.vars = "tipo"),aes(x=value,col=tipo))+
  geom_density()+facet_wrap(~variable,scales = "free")+theme_bw()+
  theme(legend.position="bottom",legend.title=element_blank())

#####################


## Muestra 80% training
itrain<- createDataPartition(xx.all$tipo, p = .8)

xx.train<-xx.all[ itrain$Resample1, ]
xx.test <-xx.all[-itrain$Resample1, ]

nrow(xx.train)/nrow(xx.all)
prop.table(table(xx.all$tipo))
prop.table(table(xx.train$tipo))
prop.table(table(xx.test$tipo))

### ESCALO
w.trans = preProcess (  xx.train [ , -indY ] , method = c ("center", "scale") )
w.trans

xx.train[ , -indY]  = predict( w.trans , xx.train[ , -indY]  )
xx.test [ , -indY]  = predict( w.trans , xx.test[ , -indY]  )

## Comprobando la estandarización
mean.st = apply ( xx.train[ , -indY] , 2 , mean, na.rm=T )  
range ( mean.st )
sd.st   = apply ( xx.train[ , -indY] , 2 , sd, na.rm=T )
range ( sd.st )

mean.st = apply ( xx.test[ , -indY] , 2 , mean, na.rm=T )  
range ( mean.st )
sd.st   = apply ( xx.test[ , -indY] , 2 , sd, na.rm=T )
range ( sd.st )

################################################################################
################################################################################
## RandomForest
################################################################################
################################################################################


## Control de la Técnica de Remuestreo
fiveStats = function(...) c (twoClassSummary(...), defaultSummary(...))
# Para tener los dos tipos de summary


cv.ctrl = trainControl ( method = "repeatedcv", number = 5 , repeats = 5, 
                         classProbs = TRUE,
                         summaryFunction = fiveStats )



## Construcción del Modelo Predictivo
rf.fit = train ( xx.train[ , -indY ] , xx.train$tipo , 
                  method = "rf", 
                  trControl = cv.ctrl,
                  metric = "ROC",
                  prob.model = TRUE 
)

rf.fit
names(rf.fit)

#################################################################
## Parámetros óptimos

rf.fit$bestTune

## Resultados
head(rf.fit$results)

## Gráfico del AUC respecto a los 2 parámetros
# 
plot( rf.fit )
# 
plot( rf.fit, metric= "Kappa" )


#################################################################
## Modelo Final, construido con los parámetros óptimos

rf.fit$finalModel 

class(rf.fit)
class(rf.fit$finalModel)   


#################################################################
## Predicciones del Modelo Final. Evaluación del Modelo

## Clases predichas
pred.train.class = predict ( rf.fit$finalModel, newdata = xx.train [ , -indY] )
pred.test.class  = predict ( rf.fit$finalModel, newdata = xx.test [ , -indY] )
head(pred.test.class)

## Probabilidades predichas
pred.test.prob  = predict( rf.fit$finalModel , newdata = xx.test [ , -indY] , 
                           type = "prob" )
head(pred.test.prob)

## Evaluación del Modelo. Matriz de Coonfusión
prop.table ( table(xx.test$tipo) )   ## No Information Rate
confusionMatrix ( pred.test.class , xx.test$tipo )
confusionMatrix ( pred.train.class , xx.train$tipo )

## Curva ROC
# 
roc ( xx.test$tipo, pred.test.prob [ , 2] , plot=T )


#################################################################
## Importancia de las Variables

rf.imp = varImp ( rf.fit , scale = F )
rf.imp

## Importancia de todas las variables
head(rf.imp$importance)
dim(rf.imp$importance)

## Gráfico

plot(rf.imp, top=5)

save.image("post_rf.Rdata")

