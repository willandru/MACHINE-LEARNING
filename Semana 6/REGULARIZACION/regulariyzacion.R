install.packages("glmnet")
install.packages("caret")


library(glmnet)
library(caret)
library(tidyverse)


carros<-read.csv("C:/Users/willi/GITHUB/MACHINE-LEARNING/Semana 6/REGULARIZACION/carros2011imputado.csv", header=TRUE, sep=";", dec=",")
str(carros)
glimpse(carros)


#guardar los modelos de los carros para interpretación
carmodelos<-carros$modelo
#remover el ID, el modelo y dos de los precios
carros<-within(carros,rm(ID,modelo,precio_basico,precio_equipado))
#crear variables dummy (binarias)
set.seed(1)
carroswin<-dummyVars("~.",data=carros)
carrosfin<-as.data.frame(predict(carroswin,newdata=carros))

glimpse(carrosfin2)
str(carrosfin)



carrosfin2<-within(carrosfin,rm(fabricanteLexus,tipoSporty,traccionTrasera,transmision_manualSi,
                                `numero_de_airbagsNo tiene`,
                                `hecho_o_no_en_USAHecho fuera de USA`))
#veamos la dimension final de la base de datos
dim(carrosfin2)



#retiro la variable a predecir
predictores<-cbind(as.matrix(carrosfin2[,1:27]),as.matrix(carrosfin2[,29:58]))


#MODELING

##entrenamiento y validación 80-20
set.seed(49584)
sample <- sample.int(nrow(carrosfin2), floor(.8*nrow(carrosfin2)))
carros.train <- carrosfin2[sample, ]
carros.test <- carrosfin2[-sample, ]

#haciendo el modelo
modeloaug<-lm(precio_promedio~.,data=carros.train)
modelocarstep<-step(modeloaug,direction="both",trace=0)
summary(modelocarstep)

predic.train<-cbind(as.matrix(carros.train[,1:27]),as.matrix(carros.train[,29:58]))
precio.train<-as.matrix(carros.train[,28])

predic.test<-cbind(as.matrix(carros.test[,1:27]),as.matrix(carros.test[,29:58]))
precio.test<-as.matrix(carros.test[,28])

fitlasso<-glmnet(predic.train,precio.train,alpha = 1)
fitridge<-glmnet(predic.train,precio.train,alpha = 0)

#ver la variacion de los coeficientes con el lambda
plot(fitlasso, xvar="lambda")
