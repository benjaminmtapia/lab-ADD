library(ggpubr)
library(ggplot2)
setwd("C:/Users/Maximiliano Ar�valo/OneDrive/Escritorio/AnalisisDatos")
#Soybean dataset
data <- read.csv("soybean-large.data", header = FALSE, fill = TRUE)
colnames(data) <- c("classname","date","plant-stand","precip","temp","hail","crop-hist","area-damaged","severity","seed-tmt","germination","plant-growth","leaves","leafspots-halo","leafspots-marg","leafspot-size","leaf-shread","leaf-malf","leaf-mild","stem","lodging","stem-cankers","canker-lesion","fruiting-bodies","external decay","mycelium","int-discolor","sclerotia","fruit-pods","fruit spots","seed","mold-growth","seed-discolor","seed-size","shriveling","roots")

#Se limpiaran los datos para eliminar los datos "?"
#cleanData<-data[!(data$date=="?"),]
#mean(cleanData$date, na.rm =TRUE)


#Haremos los pasos requeridos para un caso particular (Date), luego debemos generalizarlo

#Contruimos las tablas de frecuencia para los valores de las columnas
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

head(df_dates)

plot.piechart<-function(
  data,
  x,
  y,
  xlabel,
  ylabel,
  title
){
  ggplot(data, aes(x=x, y=y, fill=x,label=y))+ 
    geom_bar(width = 1, stat = "identity") +

    scale_fill_brewer(palette="Set3") + 
    ggtitle(title)+

    labs(x=xlabel,y=ylabel)+
    theme(panel.border = element_blank(),panel.background= element_blank())
}


plot.piechart(df_dates,df_dates$Mes,df_dates$Porcentaje,"x","Porcentaje","Grafico de Torta")

ggarrange(
  plot.piechart(df_dates,df_dates$Mes,df_dates$Porcentaje,"Mes","Porcentaje","Grafico 1"),
  plot.piechart(df_dates,df_dates$Mes,df_dates$Porcentaje,"Mes","Porcentaje","Grafico 2"),
  plot.piechart(df_dates,df_dates$Mes,df_dates$Porcentaje,"Mes","Porcentaje","Grafico 3")
)
