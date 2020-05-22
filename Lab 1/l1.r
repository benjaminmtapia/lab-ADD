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



df_date<- as.data.frame(table(data$date))
df_plant_stand<- as.data.frame(table(data$'plant-stand'))
df_precip<- as.data.frame(table(data$precip))
df_temp<- as.data.frame(table(data$temp))
df_hail<- as.data.frame(table(data$hail))
df_crop_hist<- as.data.frame(table(data$`crop-hist`))
df_area_damaged<- as.data.frame(table(data$'area-damaged'))
df_severity<- as.data.frame(table(data$severity))
df_seed_tmt<- as.data.frame(table(data$'seed-tmt'))
df_germination<- as.data.frame(table(data$germination))
df_plant_growth<- as.data.frame(table(data$'plant-growth'))

df_leaves<- as.data.frame(table(data$leaves))
df_leafspot_size<- as.data.frame(table(data$`leafspot-size`))
df_leafspots_halo<- as.data.frame(table(data$`leafspots-halo`))
df_leafspots_marg<- as.data.frame(table(data$`leafspots-marg`))
df_leaf_shread<- as.data.frame(table(data$`leaf-shread`))
df_leaf_malf<- as.data.frame(table(data$`leaf-malf`))
df_leaf_mild<- as.data.frame(table(data$`leaf-mild`))

df_stem<- as.data.frame(table(data$stem))
df_lodging<- as.data.frame(table(data$lodging))
df_stem_cankers<- as.data.frame(table(data$`stem-cankers`))
df_canker_lesion<- as.data.frame(table(data$`canker-lesion`))
df_fruiting_bodies<- as.data.frame(table(data$`fruiting-bodies`))
df_external_decay<- as.data.frame(table(data$`external decay`))
df_mycelium<- as.data.frame(table(data$mycelium))

df_int_discolor<- as.data.frame(table(data$`int-discolor`))
df_sclerotia<- as.data.frame(table(data$sclerotia))
df_fruit_pods<- as.data.frame(table(data$`fruit-pods`))
df_fruit_spots<- as.data.frame(table(data$`fruit spots`))
df_seed<- as.data.frame(table(data$seed))
df_mold_growth<- as.data.frame(table(data$`mold-growth`))
df_seed_discolor<- as.data.frame(table(data$`seed-discolor`))
df_seed_size<- as.data.frame(table(data$`seed-size`))
df_shriveling<- as.data.frame(table(data$shriveling))
df_roots<- as.data.frame(table(data$roots))



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

#tablas de contingencia para las clases y cada caracteristica
contingency.date <- table(data$classname,data$date)
contingency.precip <- table(data$classname,data$precip)
contingency.temp <- table(data$classname,data$temp)
contingency.areadamaged <- table(data$classname,data$`area-damaged`)

temp.chi<- chisq.test(contingency.temp,simulate.p.value = TRUE)
areadamaged.chi<- chisq.test(contingency.areadamaged, simulate.p.value = TRUE)

temp.chi.residuals <- temp.chi$residuals
areadamaged.chi.residuals <- areadamaged.chi$residuals