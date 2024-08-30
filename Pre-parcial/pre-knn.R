
library(readxl)
library(caret)
library(MLmetrics)
library(ROCR)
library(e1071)
library(tidyverse)

data <- read.csv("C:/Users/willi/GITHUB/MACHINE-LEARNING/Pre-parcial/df_revisado_pequeno_pruebasSaber11.csv")
glimpse(data)


# Convertir columnas a factores
data$ESTU_GENERO <- as.factor(data$ESTU_GENERO)
data$ESTU_DEPTO_RESIDE <- as.factor(data$ESTU_DEPTO_RESIDE)
data$ESTU_MCPIO_RESIDE <- as.factor(data$ESTU_MCPIO_RESIDE)
data$FAMI_ESTRATOVIVIENDA <- as.factor(data$FAMI_ESTRATOVIVIENDA)
data$COLE_BILINGUE <- as.factor(data$COLE_BILINGUE)
data$COLE_CARACTER <- as.factor(data$COLE_CARACTER)
data$DESEMP_INGLES <- as.factor(data$DESEMP_INGLES)

#Fechas
reference_date <- Sys.Date() # or use a specific date
data$ESTU_EDAD <- as.numeric(difftime(reference_date, data$ESTU_FECHANACIMIENTO, units = "weeks")) / 52.25

glimpse(data)


df <- data %>% select(-ESTU_FECHANACIMIENTO)
glimpse(df)



##Crea un set completo de variables dummy
prowin<-dummyVars("~.",data=df)
profin<-as.data.frame(predict(prowin,newdata=df))

colnames(profin) <- make.names(colnames(profin))

profin$ESTU_GENERO.F <- NULL
profin$ESTU_DEPTO_RESIDE. <- NULL
profin$ESTU_MCPIO_RESIDE. <- NULL
profin$COLE_BILINGUE.N <- NULL
profin$ESTU_GENERO.F <- NULL

glimpse(profin)


clean_data <- na.omit(profin)
glimpse(profin)
#Dataset de entrenamiento y prueba
set.seed(1)
#aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 75% de los casos
sample <- sample.int(nrow(clean_data), floor(.75*nrow(clean_data)))
sb11.train <- clean_data[sample, ]
sb11.test <- clean_data[-sample, ]

sb11.train1 <- sb11.train
sb11.test1 <- sb11.test

# Volvemos la columna que queremos predecir de tipo 'Factor'



#VALIDACION CRUZADA
# creo parámetros de validación cruzada
set.seed(1)
cross<-trainControl(method="cv",number=10)
modeloknn1<-train(PUNT_GLOBAL~.,method="knn",
                  tuneGrid=expand.grid(k=1:10),
                  trControl=cross,
                  metric="RMSE",
                  data=sb11.train)


modeloknn1

plot(modeloknn1)



# MODELO CON K = f
set.seed(1)
test_pred = knn3(PUNT_GLOBAL~., data=sb11.train1, k = )

# Desempeño en entrenamiento.  K = 5
predmod <- predict(test_pred, sb11.train1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.5 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         sb11.train1$PUNT_GLOBAL, positive = "1")
confknn$table

confknn$overall

confknn$byClass

# Desempeño en prueba.  K = 5
predmod <- predict(test_pred, cereal.test1, type = "prob")
pronknn<-ifelse(predmod[,2] > 0.5 ,1, 0)
confknn<-confusionMatrix(as.factor(pronknn),
                         cereal.test1$PUNT_GLOBAL, positive = "1")
confknn$table

confknn$overall

confknn$byClass

# CURVA ROC y AUC.  K = 5

#crear objeto de predicciones
pr<-prediction(pronknn,cereal.test$desayunoAvena)
#creacion del objeto de la curva
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
#grafico de la curva
plot(curvaROC)

#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
#ver el AUC
auc