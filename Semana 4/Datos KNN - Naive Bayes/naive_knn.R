if (!require('readxl')) install.packages('readxl')
if (!require('caret')) install.packages('caret')
if (!require('MLmetrics')) install.packages('MLmetrics')
if (!require('e1071')) install.packages('e1071')

library(readxl)
library(caret)
library(MLmetrics)
library(ROCR)
library(e1071)

cartera<-read_excel("carteraguia2017.xlsx")

str(cartera)  

cartera$TIPOips<-as.factor(cartera$TIPO_ips)


##Crea un set completo de variables dummy
carterawin<-dummyVars("~.",data=cartera)
carterafin<-as.data.frame(predict(carterawin,newdata=cartera))
carterafin2<-within(carterafin,rm(TIPOips.5,TIPO_ips))
carterafin2$retrasos<-as.factor(carterafin2$retrasos)
carterafin2$retrasos<-factor(carterafin2$retrasos,ordered=TRUE, levels=c("0","1"))


# estandarización de las variables escalares
carterafin2[1:7]<-scale(carterafin2[1:7])

head(carterafin2)


# creamos base de entrenamiento y de prueba
set.seed(1)
#aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 75% de los casos
sample <- sample.int(nrow(carterafin2), floor(.75*nrow(carterafin2)))
cartera.train <- carterafin2[sample, ]
cartera.test <- carterafin2[-sample, ]

cartera.train1 <- cartera.train
cartera.test1 <- cartera.test

# creo parámetros de validación cruzada
set.seed(1)
cross<-trainControl(method="cv",number=10)
modeloknn1<-train(retrasos~.,method="knn",
                  tuneGrid=expand.grid(k=1:30),
                  trControl=cross,
                  metric="Accuracy",
                  data=cartera.train)
modeloknn1
plot(modeloknn1)


levels(cartera.train$retrasos) <- make.names(levels(factor(cartera.train$retrasos)))
# creo parámetros de validación cruzada
cross<-trainControl(method="cv",number=5,
                    classProbs = TRUE,
                    summaryFunction =prSummary)
set.seed(1)
modeloknn2<-train(retrasos~.,method="knn",
                  tuneGrid=expand.grid(k=1:30),
                  trControl=cross,
                  metric="AUC",
                  data=cartera.train)
modeloknn2
plot(modeloknn2)

# el modelo sintonizado
predmod1<-predict(modeloknn2,cartera.test,type="prob")
pronknn1<-ifelse(predmod1$X1 > 0.5, 1, 0)
confknn1<-confusionMatrix(as.factor(pronknn1),
                          cartera.test$retrasos, positive = "1")
confknn1$table
confknn1$byClass

# el modelo sintonizado
predmod1<-predict(modeloknn2,cartera.test,type="prob")
pronknn1<-ifelse(predmod1$X1 > 0.3, 1, 0)
confknn1<-confusionMatrix(as.factor(pronknn1),
                          cartera.test$retrasos, positive = "1")
confknn1$table
confknn1$byClass


#crear objeto de predicciones
pr<-prediction(pronknn1,cartera.test$retrasos)
#creacion del objeto de la curva
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
#grafico de la curva
plot(curvaROC)


#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
#ver el AUC
auc


#USO DE LA FUNCION KNN3 DE CARET


# MODELO CON K = 5
set.seed(1)
test_pred = knn3(retrasos~., data=cartera.train1, k = 5)

# Desempeño en entrenamiento.  K = 5
predmod <- predict(test_pred, cartera.train1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.3 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         cartera.train1$retrasos, positive = "1")
confknn$table
confknn$overall
confknn$byClass
      #Nos va a interesar mirar el accuracy, la sensibidad, la epsecificdad-

# Desempeño en prueba.  K = 5
predmod <- predict(test_pred, cartera.test1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.3 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         cartera.test1$retrasos, positive = "1")
confknn$table
confknn$overall
confknn$byClass


# CURVA ROC y AUC.  K = 5

#crear objeto de predicciones
pr<-prediction(pronknn,cartera.test$retrasos)
#creacion del objeto de la curva
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
#grafico de la curva
plot(curvaROC)

#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
#ver el AUC
auc


# MODELO CON K = 17
set.seed(1)
test_pred = knn3(retrasos~., data=cartera.train1, k = 17)

# Desempeño en entranamiento  K = 17
predmod <- predict(test_pred, cartera.train1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.3 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         cartera.train1$retrasos, positive = "1")
confknn$table
confknn$overall
confknn$byClass

# Desempeño en prueba  K = 17
predmod <- predict(test_pred, cartera.test1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.3 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         cartera.test1$retrasos, positive = "1")
confknn$table
confknn$overall
confknn$byClass

# CURVA ROC y AUC.  K = 17

#crear objeto de predicciones
pr<-prediction(pronknn,cartera.test$retrasos)
#creacion del objeto de la curva
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
#grafico de la curva
plot(curvaROC)

#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
#ver el AUC
auc

# MODELO CON K = 30
set.seed(1)
test_pred = knn3(retrasos~., data=cartera.train1, k = 30)

# Desempeño en entrenamiento
predmod <- predict(test_pred, cartera.train1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.3 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         cartera.train1$retrasos, positive = "1")
confknn$table
confknn$overall
confknn$byClass

# Desempeño en prueba
predmod <- predict(test_pred, cartera.test1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.3 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         cartera.test1$retrasos, positive = "1")
confknn$table
confknn$overall
confknn$byClass

# CURVA ROC y AUC.  K = 30

#crear objeto de predicciones
pr<-prediction(pronknn,cartera.test$retrasos)
#creacion del objeto de la curva
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
#grafico de la curva
plot(curvaROC)

#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
#ver el AUC
auc


## BALANCEO DE DATASET DE ENTRENAMIENTO CON SMOTE
install.packages("remotes")
remotes::install_github("cran/DMwR")
library(DMwR)


#utilizar SMOTE para crear un nuevo conjunto de datos más equilibrado
cartera.train2 <- SMOTE(retrasos~., as.data.frame(cartera.train1), perc.over = 200, perc.under = 150)
#ver la distribución de la variable de respuesta en el nuevo conjunto de datos
table(cartera.train2$retrasos)

# MODELO SMOTE CON K = 5
set.seed(1)
test_pred = knn3(retrasos~., data=cartera.train2, k = 5)
