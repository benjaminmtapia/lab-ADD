library(ggplot2)
library(ggpubr)
library("C50")
library(caret)
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
data$classname = as.factor(data$classname)
training.index <- createDataPartition(data$classname, p=0.6)$Resample1
training.set = data[training.index, ]
test.set = data[-training.index, ]
tree = C5.0(classname ~ ., training.set)
tree.rules = C5.0(x = training.set[, -8], y = training.set$classname, rules = T)