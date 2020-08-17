library(ggplot2)
library(ggpubr)
library(arulesViz)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"
data <- read.csv(url, header = FALSE, fill = TRUE,na.strings = c("?"))
colnames(data) <- c("classname","date","plant-stand","precip","temp","hail","crop-hist","area-damaged","severity","seed-tmt","germination","plant-growth","leaves","leafspots-halo","leafspots-marg","leafspot-size","leaf-shread","leaf-malf","leaf-mild","stem","lodging","stem-cankers","canker-lesion","fruiting-bodies","external decay","mycelium","int-discolor","sclerotia","fruit-pods","fruit spots","seed","mold-growth","seed-discolor","seed-size","shriveling","roots")



#Contruimos las tablas de y para los valores de las columnas
dates = as.data.frame(table(data$date))
names(dates)[1] = 'date'
#limpieza de datos
data<- na.omit(data)
#se eliminan enfermedades con menores ocurrencias
data<-dplyr::filter(data,classname != '2-4-d-injury', 
                    classname!= 'herbicide-injury',
                    classname != 'cyst-nematode', 
                    classname != 'diaporthe-pod-&-stem-blight')
#se eliminan columnas relacionadas con las hojas
data[13:19]<- NULL

#graficos
#grafico para las 3 enfermedades
boxplot1 <- ggboxplot(data = data, x="classname", y ="area-damaged",color="classname")
  
#algoritmo apriori
#aplicar algoritmo para: todo el dataset, enfermedades 1 2 y 3

rules = apriori(data, parameter = list(support = 0.2,minlen=2,maxlen=2,target="rules"))

inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))