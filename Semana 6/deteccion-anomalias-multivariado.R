#DETECCION DE ANOMALIAS -- PUNTO DE VISTA MULTIVARIADO

regtecnica <- read.csv2("regtecnica.csv")

set.seed(920203) #se deja alguna semilla para que el muestreo sea replicable
#aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 80% de los casos
sample <- sample.int(nrow(regtecnica), floor(.8*nrow(regtecnica)))
regtecnica.train <- regtecnica[sample, ]
regtecnica.test <- regtecnica[-sample, ]

library('MVN')

result<-mvn(regtecnica, mvnTest = "mardia", multivariateOutlierMethod ="quan" )
atipicos<-c("3","25","54")