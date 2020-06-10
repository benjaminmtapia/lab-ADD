library(cluster)
library(dplyr)
library(ggplot2)
library(factoextra)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"
data <- read.csv(url, header = FALSE, fill = TRUE,na.strings = c("?"))
colnames(data) <- c("classname","date","plant-stand","precip","temp","hail","crop-hist","area-damaged","severity","seed-tmt","germination","plant-growth","leaves","leafspots-halo","leafspots-marg","leafspot-size","leaf-shread","leaf-malf","leaf-mild","stem","lodging","stem-cankers","canker-lesion","fruiting-bodies","external decay","mycelium","int-discolor","sclerotia","fruit-pods","fruit spots","seed","mold-growth","seed-discolor","seed-size","shriveling","roots")



#Contruimos las tablas de y para los valores de las columnas
dates = as.data.frame(table(data$date))
names(dates)[1] = 'date'
#limpieza de datos
data<- na.omit(data)
#se eliminan enfermedades con menores ocurrencias
data1<-dplyr::filter(data,classname != '2-4-d-injury', 
                     classname!= 'herbicide-injury',
                     classname != 'cyst-nematode', 
                     classname != 'diaporthe-pod-&-stem-blight')
#se eliminan columnas relacionadas con las hojas
data1[13:19]<- NULL

#calculamos cuantos clusters se deben usar con elbow method de factoextra 
nclusters <- select(data1,-classname)
nclusters <- fviz_nbclust(data.ncluster, pam, method = "wss")

#se ve que son 4 clusters los necesarios, se procede a crear cluster con PAM
matriz.diferencias <- daisy(data1[,-1],metric="gower")
cluster<- pam(data1,k=4,metric = c("manhattan"))
fviz_cluster(cluster,data1,ellipse.type = "t")+theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

cluster2<-pam(matriz.diferencias,k=4,diss=TRUE)
clusplot(cluster2)
data1["grupo"]<-cluster2$clustering
