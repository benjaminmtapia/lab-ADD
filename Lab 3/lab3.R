library(ggplot2)
library(ggpubr)
library(arulesViz)
#Se obtiene la base de datos y se crea el dataframe
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"
data <- read.csv(url, header = FALSE, fill = TRUE,na.strings = c("?"))
colnames(data) <- c("classname","date","plant-stand","precip","temp","hail","crop-hist","area-damaged","severity","seed-tmt","germination","plant-growth","leaves","leafspots-halo","leafspots-marg","leafspot-size","leaf-shread","leaf-malf","leaf-mild","stem","lodging","stem-cankers","canker-lesion","fruiting-bodies","external decay","mycelium","int-discolor","sclerotia","fruit-pods","fruit spots","seed","mold-growth","seed-discolor","seed-size","shriveling","roots")
data <- data.frame(data)

#Se realiza una limpieza de los datos
data <- na.omit(data)
data <- dplyr::filter(data,
                      classname != '2-4-d-injury',
                      classname != 'herbicide-injury',
                      classname != 'cyst-nematode',
                      classname != 'diaporthe-pod-&-stem-blight')
#Se eliminan columnas que contienen informacion detallada sobre otra variable
data[13:19] <- NULL

### Implementacion algoritmo apriori ###

#Se define el soporte
support <- 0.2

#Se procede a utilizar apriori para generar las reglas
rules <- apriori(data,
                 parameter = list(support = support, minlen = 2, maxlen = 3, target = "rules"),
)
#No se agrega el print porque este muestra el resultado, se asigna a una variable y luego se muestra
result_allRules <- sort(x=rules, decreasing = TRUE, by = "lift")
result_allRulesData <- inspect(result_allRules)

#10 mejores reglas segun el lift
result_liftRanking <- inspect(head(result_allRules, n = 10, by = "lift"))

#Top 20 itemsets de mayor a menor soporte
result_suppRanking <- sort(result_allRules, by = "support", decreasing = TRUE)[1:20]
inspect(result_suppRanking) #Muestra casi todas con valores 1 de soporte, confianza, etc