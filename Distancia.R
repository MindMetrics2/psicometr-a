# Paso 1: Calcular la distancia de Mahalanobis
# Supongamos que tus datos están en un data frame llamado `datos` con las variables TMMS24 y BULLYING

# Selecciona las variables relevantes
variables <- datos[, c("TMMS24", "BULLYING")]

# Calcula la media y la matriz de covarianza
media <- colMeans(da)
matriz_cov <- cov(da)

# Calcula la distancia de Mahalanobis
dist_mahalanobis <- mahalanobis(da, center = media, cov = matriz_cov)

# Paso 2: Determinar el umbral crítico
# Número de variables independientes
k <- ncol(da)

# Nivel de significancia deseado (ej. 0.05)
alpha <- 0.05

# Valor crítico chi-cuadrado
umbral_critico <- qchisq(1 - alpha, df = k)

# Paso 3: Identificar los casos que deben ser eliminados
casos_a_eliminar <- which(dist_mahalanobis > umbral_critico)

# Imprimir los casos
casos_a_eliminar

# Eliminar los casos identificados
datos_limpios <- da[-casos_a_eliminar, ]
