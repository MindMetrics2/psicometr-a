
trainData <- de[c(TRUE,FALSE),]
testData <- de[c(FALSE,TRUE),]

items<- paste0("Q",1:24)
# Form saturated model:
GGM_train <- ggm(trainData, estimator = "FIML")

# Perform model selection steps:
GGM_train <- GGM_train %>% runmodel %>% 
  prune(alpha = 0.01, recursive = FALSE) %>% 
  modelsearch(verbose = FALSE)

# Obtain the network:
net_train <- getmatrix(GGM_train, "omega")
qgraph(net_train, theme = "colorblind", layout = "spring")

# Obtain structure:
structure <- 1*(net_train != 0)

# Form test model:
GGM_test <- ggm(trainData, estimator = "FIML",
                omega = structure)
# Fit model:
GGM_test <- GGM_test %>% runmodel

# Obtain the network:
net_test <- getmatrix(GGM_test, "omega")
qgraph(net_test, theme = "colorblind", layout = "spring")

GGM_test %>% fit

#Modelo de red residual
#Para controlar un posible factor de autoestima común, 
#podemos estimar un modelo de red residual (RNM). 
#Primero, comenzaremos con un modelo CFA saturado:

# Factor loadings matrix (three factor):
Lambda <- matrix(0, 24, 3)
Lambda[1:8,1] <- 1
Lambda[c(9:16),2] <- 1
Lambda[c(17:24),3] <- 1
print(Lambda)

# CFA model:
CFA_train <- rnm(trainData, estimator = "FIML",
                 lambda = Lambda) %>% runmodel


CFA_test <- rnm(testData, estimator = "FIML",
                lambda = Lambda) %>% runmodel


rbind(
  train = CFA_train@fitmeasures,
  test = CFA_test@fitmeasures)[,c("rmsea","cfi","tli","rni")]



#A continuación, podemos entrenar un modelo RNM utilizando la selección de modelos 

#paso a paso mediante índices de modificación, seguido de modelsearchuna búsqueda más precisa:
# Estimate RNM:
RNM_train <- CFA_train %>% 
  stepup(criterion = "bic",verbose = FALSE) %>% 
  modelsearch(verbose = FALSE)

# Obtain residual network:
residnet_train <- getmatrix(RNM_train, "omega_epsilon")

#Finalmente, podemos probar este RNM en los datos de prueba:
  
  structure <- 1*(residnet_train!=0)

# Form RNM on test data:
RNM_test <- rnm(testData, estimator = "FIML",
                lambda = Lambda, omega_epsilon = structure)

# Run model:
RNM_test <- RNM_test %>% runmodel

RNM_test %>% fit


#Podemos graficar las cargas factoriales y la red residual de la siguiente manera:
# Obtain residual network:
residnet_test <- getmatrix(RNM_test, "omega_epsilon")

# Obtain factor loadings:
factorloadings <- getmatrix(RNM_test, "lambda")

# Plot:
layout(t(1:2))
qgraph(residnet_test, theme = "colorblind", layout = "spring", edge.labels=TRUE,
       title = "Residual network", vsize = 8)
qgraph.loadings(factorloadings, theme = "colorblind", model = "reflective", edge.labels=TRUE,
                title = "Factor loadings", vsize = c(8,13), asize = 5)

# Comparison in training data:
compare(
  GGM = GGM_train,
  RNM = RNM_train
)

# Comparison in test data:
compare(
  GGM = GGM_test,
  RNM = RNM_test
)

# Factor loadings matrix (bifactor):
Lambda4 <- matrix(0, 24, 4)
Lambda4[,1] <- 1
Lambda4[c(1:8),2] <- 1
Lambda4[c(9:16),3] <- 1
Lambda4[c(17:24),4] <- 1
print(Lambda4)

# Fit bifactor:
bi_train <- rnm(trainData, estimator = "FIML",
                lambda = Lambda4, sigma_zeta = "empty") %>% runmodel

bi_test <- rnm(testData, estimator = "FIML",
               lambda = Lambda4, sigma_zeta = "empty") %>% runmodel

# Fit bifactor RNM to train data:
RNM_bi_train <- bi_train %>% 
  stepup(criterion = "bic",verbose = FALSE) %>% 
  modelsearch(verbose = FALSE)


# Fit bifactor RNM to test data:
structure <- 1*(getmatrix(RNM_bi_train,"omega_epsilon")!=0)
RNM_bi_test <- rnm(testData, estimator = "FIML",
                   lambda = Lambda4, omega_epsilon = structure) %>% runmodel


#Ahora podemos comparar todos estos modelos:
  
  comparison <- compare(
    GGM = GGM_test,
    RNM = RNM_test,
    bifactor = bi_test,
    bifactor_RNM = RNM_bi_test
  )  
print(comparison)


# To posterior model probability:
BICs <- mpfr(comparison$BIC, 100)
BICtrans <- exp(-0.5 * BICs)
sumBICtrans <- sum(BICtrans)
comparison$modelProbability <- as.numeric(BICtrans / sumBICtrans)

# Make a plot:
g <- ggplot(comparison, aes(x=model, y = modelProbability)) + 
  geom_bar(stat="identity") + xlab("") + 
  ylab("Posterior model probability") + theme_bw() + 
  ylim(0,1)
print(g)
