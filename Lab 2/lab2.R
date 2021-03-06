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
data<-dplyr::filter(data,classname != '2-4-d-injury', 
                    classname!= 'herbicide-injury',
                    classname != 'cyst-nematode', 
                    classname != 'diaporthe-pod-&-stem-blight')
#se eliminan columnas relacionadas con las hojas
data[13:19]<- NULL


#primer cluster
#calculamos cuantos clusters se deben usar con elbow method de factoextra 
data1 <- select(data,date, precip, temp, 'area-damaged', severity)
data1 <- select(data1,-classname)

nclusters1 <- fviz_nbclust(data1, pam, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
#se ve que son 4 clusters los necesarios, se procede a crear cluster con PAM
matriz.diferencias1 <- daisy(data1[,-1],metric="gower")

cluster1<-pam(matriz.diferencias1,k=4,diss=TRUE)
clusplot(cluster1.2)
data1["grupo"]<-cluster2$clustering


#Segundo cluster: Brown spot con crop-hist

#Se obtienen las observaciones requeridas para el cluster
data2 <- select(data[87:126,] ,date, precip, temp, 'area-damaged', severity, 'crop-hist')

#Se obtiene el numero de clusters necesarios 
nclusters2 <- fviz_nbclust(data2, pam, method = "wss")+ geom_vline(xintercept = 4, linetype = 2)

matriz.diferencias2 <- daisy(data2[,-1],metric="gower")

#Se obtienen el cluster

cluster2<-pam(matriz.diferencias2,k=4,diss=TRUE)
clusplot(cluster2)

#Tercer cluster: Phytophthora-rot con precip 

#Se obtienen las observaciones requeridas para el cluster
data3 <- select(data[31:46,] ,date, precip, temp, severity)

#Se obtiene el numero de clusters necesarios 
nclusters3 <- fviz_nbclust(data3, pam, method = "wss") + geom_vline(xintercept = 3, linetype = 2)
matriz.diferencias3 <- daisy(data1[,-1],metric="gower")

#Se obtienen el cluster

cluster3<-pam(matriz.diferencias3,k=3,diss=TRUE)
clusplot(cluster3)
