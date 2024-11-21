import scipy.stats as stats

# Parámetros
n = 25  # número total de personas
 # probabilidad de que cada persona acierte
k = 13  # queremos que al menos 13 personas acierten

p = 0.35

# Recalcular la probabilidad acumulada
probabilidad_mayoria_035 = 1 - stats.binom.cdf(k-1, n, p)

print(probabilidad_mayoria_035)