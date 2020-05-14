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


barplot<-ggarrange(

p1<-  plot.barchart(df_date,df_date$Var1,df_date$Freq,"Mes","y","Date"),
p2<-  plot.barchart(df_plant_stand,df_plant_stand$Var1,df_plant_stand$Freq,"Plant stand","y","Plant stand"),
p3<-  plot.barchart(df_area_damaged,df_area_damaged$Var1,df_area_damaged$Freq,"x","y","Area damaged"),
p4<-  plot.barchart(df_canker_lesion,df_canker_lesion$Var1,df_canker_lesion$Freq,"x","y","Canker lesion"),
p5<-  plot.barchart(df_precip,df_precip$Var1,df_precip$Freq,"x","y","Precip"),
p6<-  plot.barchart(df_temp,df_temp$Var1,df_temp$Freq,"x","y","Temp"),
p7<-  plot.barchart(df_hail,df_hail$Var1,df_hail$Freq,"x","y","Hail"),
p8<-  plot.barchart(df_crop_hist,df_crop_hist$Var1,df_crop_hist$Freq,"x","y","Crop Hist"),
p9<-  plot.barchart(df_severity,df_severity$Var1,df_severity$Freq,"x","y","Severity"),
p10<-  plot.barchart(df_seed_tmt,df_seed_tmt$Var1,df_seed_tmt$Freq,"Mes","y","Seed tmt"),

p11<-  plot.barchart(df_germination,df_germination$Var1,df_germination$Freq,"x","y","Germination"),
p12<-  plot.barchart(df_plant_growth,df_plant_growth$Var1,df_plant_growth$Freq,"x","y","Area damaged"),
p13<-  plot.barchart(df_leaves,df_leaves$Var1,df_leaves$Freq,"x","y","Leaves"),
p14<-  plot.barchart(df_leafspots_halo,df_leafspots_halo$Var1,df_leafspots_halo$Freq,"x","y","Leafspots Halo"),
p15<-  plot.barchart(df_leafspots_marg,df_leafspots_marg$Var1,df_leafspots_marg$Freq,"x","y","Leafspost Marg"),
p16<-  plot.barchart(df_leafspot_size,df_leafspot_size$Var1,df_leafspot_size$Freq,"x","y","Leafspot Size"),
p17<-  plot.barchart(df_leaf_shread,df_leaf_shread$Var1,df_leaf_shread$Freq,"x","y","Leaf Shread"),
p18<-  plot.barchart(df_leaf_malf,df_leaf_malf$Var1,df_leaf_malf$Freq,"x","y","Leaf Malf"),

p19<-  plot.barchart(df_leaf_mild,df_leaf_mild$Var1,df_leaf_mild$Freq,"x","y","Leaf Mild"),
p20<-  plot.barchart(df_stem,df_stem$Var1,df_stem$Freq,"x","y","Stem"),
p21<-  plot.barchart(df_lodging,df_lodging$Var1,df_lodging$Freq,"x","y","Lodging"),
p22<-  plot.barchart(df_stem_cankers,df_stem_cankers$Var1,df_stem_cankers$Freq,"x","y","Stem Cankers"),
p23<-  plot.barchart(df_fruiting_bodies,df_fruiting_bodies$Var1,df_fruiting_bodies$Freq,"x","y","Fruiting Bodies"),
p24<-  plot.barchart(df_external_decay,df_external_decay$Var1,df_external_decay$Freq,"x","y","External Bodies"),
p25<-  plot.barchart(df_mycelium,df_mycelium$Var1,df_mycelium$Freq,"x","y","Mycelium"),
p26<-  plot.barchart(df_int_discolor,df_int_discolor$Var1,df_int_discolor$Freq,"x","y","Int Discolor"),
p27<-  plot.barchart(df_fruit_pods,df_fruit_pods$Var1,df_fruit_pods$Freq,"x","y","Fruit Pods"),

p28<-  plot.barchart(df_fruit_spots,df_fruit_spots$Var1,df_fruit_spots$Freq,"x","y","Fruit Spots"),
p29<-  plot.barchart(df_seed,df_seed$Var1,df_seed$Freq,"x","y","Seed"),
p30<-  plot.barchart(df_mold_growth,df_mold_growth$Var1,df_mold_growth$Freq,"x","y","Mold Growth"),
p31<-  plot.barchart(df_seed_discolor,df_seed_discolor$Var1,df_seed_discolor$Freq,"x","y","Seed Discolor"),
p32<-  plot.barchart(df_seed_size,df_seed_size$Var1,df_seed_size$Freq,"x","y","Seed Size"),
p33<-  plot.barchart(df_shriveling,df_shriveling$Var1,df_shriveling$Freq,"x","y","Shriveling"),
p34<-  plot.barchart(df_roots,df_roots$Var1,df_roots$Freq,"x","y","Roots"),

nrow=4, ncol=4

)
