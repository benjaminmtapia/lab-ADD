library(cluster)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"
data <- read.csv(url, header = FALSE, fill = TRUE)
colnames(data) <- c("classname","date","plant-stand","precip","temp","hail","crop-hist","area-damaged","severity","seed-tmt","germination","plant-growth","leaves","leafspots-halo","leafspots-marg","leafspot-size","leaf-shread","leaf-malf","leaf-mild","stem","lodging","stem-cankers","canker-lesion","fruiting-bodies","external decay","mycelium","int-discolor","sclerotia","fruit-pods","fruit spots","seed","mold-growth","seed-discolor","seed-size","shriveling","roots")

#Contruimos las tablas de y para los valores de las columnas
dates = as.data.frame(table(data$date))
names(dates)[1] = 'date'