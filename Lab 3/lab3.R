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

#Apriori para todo el dataset
rulesAllData = apriori(data,
                       parameter = list(support = 0.2,minlen = 2, maxlen= 2, target="rules")) #Ver si se puede agregar lo de la clase
#Como inspect es una especie de print, si se pone aqui en el ranking no se muestra
result_rulesAllData <- sort(x = rulesAllData, decreasing = TRUE, by  = "confidence")
inspect(result_rulesAllData)

#Apriori de ranking 10 reglas by lift
result_liftRanking <- inspect(head(result_rulesAllData, n = 10, by = "lift"))

#Top 20 itemsets de mayor a menor soporte
top20a <- sort(result_rulesAllData, by = "support", decreasing = TRUE)[1:20]
inspect(top20a) #Muestra los que tienen soporte de 1
#Grafico: (NO SE MUESTRA)
graph_20a <- as(top20a, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items,support), y = support)) + 
  geom_col() + 
  coord_flip() + 
  labs(title = "Itemsets mas frecuentes \ Mayor a menor soporte", x = "itemsets") +
  theme_bw()

#Top 20 itemsets de menor a mayor soporte
top20b <- sort(result_rulesAllData, by = "support", decreasing = FALSE)[1:20]
inspect(top20b) #Muestra los que tienen soporte de 1

#Apriori phythoptora-rot
#dataPhytho = data[31:46,]
#rulesPhytho = apriori(dataPhytho,
#                       parameter = list(support = 0.2,minlen = 1, maxlen= 2, target="rules")) #Ver si se puede agregar lo de la clase
#Como inspect es una especie de print, si se pone aqui en el ranking no se muestra
#result_rulesPhytho <- sort(x = rulesPhytho, decreasing = TRUE, by  = "confidence")
#inspect(result_rulesPhytho)

#Apriori brown-spot
