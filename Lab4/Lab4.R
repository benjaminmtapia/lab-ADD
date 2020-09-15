library(ggplot2)
library(ggpubr)
library(rpart)
library(rpart.plot)
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
data[3] <- NULL
data[5] <- NULL
data[8:27] <- NULL
data[7]<- NULL
#data[5:6] <- NULL

#Arbol N°1
data1 <- data[1:46,]
data1$classname = as.factor(data1$classname)
training.index <- createDataPartition(data1$classname, p=0.7)$Resample1
training.set = data1[training.index, ]
test.set = data1[-training.index, ]
tree = C5.0(classname ~ ., training.set)
tree.rules = C5.0(x = training.set[, -5], y = training.set$classname, rules = T)
plot(tree)
summary(tree)
summary(tree.rules)

#Arbol N°2
data2 <- data[47:126,]
data2$classname = as.factor(data2$classname)
training.index2 <- createDataPartition(data2$classname, p=0.7)$Resample1
training.set2 = data2[training.index2, ]
test.set2 = data2[-training.index2, ]
tree2 = C5.0(classname ~ ., training.set2)
tree.rules2 = C5.0(x = training.set2[, -5], y = training.set2$classname, rules = T)
plot(tree2)
summary(tree2)
summary(tree.rules2)

#Arbol N°3
data3 <- data[127:176,]
data3$classname = as.factor(data3$classname)
training.index3 <- createDataPartition(data3$classname, p=0.7)$Resample1
training.set3 = data3[training.index3, ]
test.set3 = data3[-training.index3, ]
tree3 = C5.0(classname ~ ., training.set3)
tree.rules3 = C5.0(x = training.set3[, -5], y = training.set3$classname, rules = T)
plot(tree3)
summary(tree3)
summary(tree.rules3)

#Arbol N°4
data4 <- data[177:266,]
data4$classname = as.factor(data4$classname)
training.index4 <- createDataPartition(data4$classname, p=0.7)$Resample1
training.set4 = data4[training.index4, ]
test.set4 = data4[-training.index4, ]
tree4 = C5.0(classname ~ ., training.set4)
tree.rules4 = C5.0(x = training.set4[, -5], y = training.set4$classname, rules = T)
plot(tree4)
summary(tree4)
summary(tree.rules4)