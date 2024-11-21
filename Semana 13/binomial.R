# Parámetros
n <- 25  # número total de personas
p <- 0.35  # probabilidad de que cada persona acierte
k <- 13  # queremos que al menos 13 personas acierten

# Calcular la probabilidad acumulada
probabilidad_mayoria <- 1 - pbinom(k-1, n, p)

# Mostrar el resultado
probabilidad_mayoria
