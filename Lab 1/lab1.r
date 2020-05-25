library(ggpubr)
library(ggplot2)
#Soybean dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"
data <- read.csv(url, header = FALSE, fill = TRUE)
colnames(data) <- c("classname","date","plant-stand","precip","temp","hail","crop-hist","area-damaged","severity","seed-tmt","germination","plant-growth","leaves","leafspots-halo","leafspots-marg","leafspot-size","leaf-shread","leaf-malf","leaf-mild","stem","lodging","stem-cankers","canker-lesion","fruiting-bodies","external decay","mycelium","int-discolor","sclerotia","fruit-pods","fruit spots","seed","mold-growth","seed-discolor","seed-size","shriveling","roots")

#Contruimos las tablas de y para los valores de las columnas
dates = as.data.frame(table(data$date))
names(dates)[1] = 'date'

#Obtenemos el numero total de entradas de la tabla original (siempre son 307)
numberRows <- nrow(data)

#Creamos el dataframe con los valores de los porcentajes para los date
df_dates <- data.frame()

#Se iterara para obtener los porcentajes, en este caso tenemos 8 (?,0,1,2,3,4,5,6)
for (i in 1:nrow(dates)){
  #cat("a") la puse para verificar cuantas veces iteraba
  #para obtener el dato especifico de un dataframe se usa: as.vector(dataframe$field[i])
  dateActual <- as.vector(dates$date[i])
  freqDateActual <- as.vector(dates$Freq[i])
  porcentajeActual <- round(freqDateActual/numberRows,4)*100
  nueva_fila <- data.frame(Mes=c(dateActual),Porcentaje=c(porcentajeActual))
  df_dates <- rbind(df_dates,nueva_fila)
}

#data frames para graficos
df_classname<- as.data.frame(table(data$classname))
df_date<- as.data.frame(table(data$date))
df_plant_stand<- as.data.frame(table(data$'plant-stand'))
df_precip<- as.data.frame(table(data$precip))
df_temp<- as.data.frame(table(data$temp))
df_crop_hist<- as.data.frame(table(data$`crop-hist`))
df_area_damaged<- as.data.frame(table(data$'area-damaged'))
df_leaves<- as.data.frame(table(data$leaves))
df_seed<- as.data.frame(table(data$seed))
df_seed_size<- as.data.frame(table(data$`seed-size`))

#tablas de contingencia para las clases y cada caracteristica
contingency.date <- table(data$classname,data$date)
contingency.precip <- table(data$classname,data$precip)
contingency.temp <- table(data$classname,data$temp)
contingency.areadamaged <- table(data$classname,data$`area-damaged`)

#Tabla phytophthora-rot / precip
pprecip<- as.data.frame(data[31:70,4])
pprecip.table <- table(pprecip)
chisq.test(pprecip.table,simulate.p.value = FALSE)

#Tabla brown spot / crop hist
bcrop <- as.data.frame(data[111:150,7])
bcrop.table <- table(bcrop)
chisq.test(bcrop.table,simulate.p.value = FALSE)
#

#graficos
plot.barchart<-function(
  data,
  x,
  y,
  xlabel,
  ylabel,
  title
){
  ggplot(data, aes(x=x, y=y, fill=x,label=y))+ 
    scale_fill_brewer(palette="Set3") + 
    ggtitle(title)+
    labs(x=xlabel,y=ylabel)+
    geom_bar(stat="identity")+
    theme(panel.border = element_blank(),panel.background= element_blank())
}

#Graficos
#Enfermedades
p0<-  plot.barchart(df_classname,df_classname$Var1,df_classname$Freq,"Enfermedad","Frecuencia","Clases")


#Date
p1<-  plot.barchart(df_date,df_date$Var1,df_date$Freq,"Mes","Frecuencia","Date")

#Temp
p2<-  plot.barchart(df_temp,df_temp$Var1,df_temp$Freq,"Tipo de Temperatura","Frecuencia","Temp")

#Area damaged
p3<-  plot.barchart(df_area_damaged,df_area_damaged$Var1,df_area_damaged$Freq,"Tipo de daño","Frecuencia","Area Dañada")

#leaves
p4<-  plot.barchart(df_leaves,df_leaves$Var1,df_leaves$Freq,"Tipo de Hoja","Frecuencia","Leaves")

#Seed size
p5<-  plot.barchart(df_seed_size,df_seed_size$Var1,df_seed_size$Freq,"Tamaño","Frecuencia","Tipo de tamaño")

#Contingencia fecha y enfermedad
p6 = ggplot(data, aes(x = date, fill = classname)) + geom_bar() + labs(x="Fecha",y="Frecuencia") 
+ggtitle("Contingencia Enfermedad y Fecha")

  #Contingencia phytophthora-rot y precip 
p7<- barplot(pprecip.table,xlab="Precip",ylab="Casos phytophthora-rot")

#Contingencia Brown-spot y crop hist
p8<- barplot(bcrop.table,xlab="Crop Hist",ylab="Casos Brown-spot")

#Contingencia Temperatura y enfermedad
p9<-ggplot(data,aes(x=classname,fill=temp)) + geom_bar()

#Prueba de independencia
#H0: independencia
#H1: dependencia

date.chi<-chisq.test(contingency.date)
temp.chi<- chisq.test(contingency.temp,simulate.p.value = FALSE)
areadamaged.chi<- chisq.test(contingency.areadamaged, simulate.p.value = TRUE)
pprecip.chi<-chisq.test(pprecip.table,simulate.p.value = FALSE)
bcrop.chi<-chisq.test(bcrop.table,simulate.p.value = FALSE)
