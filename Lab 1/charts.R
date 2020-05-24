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

save.barchart<-function()
{
  jpeg("img/class.jpg")
  p0<-  plot.barchart(df_classname,df_classname$Var1,df_classname$Freq,"Enfermedad","Frecuencia","Clases")
  dev.off()
  jpeg("img/date.jpg")
  p1<-  plot.barchart(df_date,df_date$Var1,df_date$Freq,"Mes","Frecuencia","Date")
  dev.off()
  jpeg("img/temp.jpg")
  p2<-  plot.barchart(df_temp,df_temp$Var1,df_temp$Freq,"Tipo de Temperatura","Frecuencia","Temp")
  dev.off()
  jpeg("img/area_damaged.jpg")
  p3<-  plot.barchart(df_area_damaged,df_area_damaged$Var1,df_area_damaged$Freq,"Tipo de daño","Frecuencia","Area Dañada")
  dev.off()
  jpeg("img/leaves.jpg")
  p4<-  plot.barchart(df_leaves,df_leaves$Var1,df_leaves$Freq,"Tipo de Hoja","Frecuencia","Leaves")
  dev.off()
  jpeg("img/seed_size.jpg")
  p5<-  plot.barchart(df_seed_size,df_seed_size$Var1,df_seed_size$Freq,"Tamaño","Frecuencia","Tipo de tamaño")
  dev.off()
}