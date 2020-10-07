
ggplot_spatfd=function(data,variable=1,main="Functional data",ylab="Value",xlab="Time"){
     
     data=data[[variable]]$fdata
     eval=eval.fd(1:length(data$fdnames$time),data)
     melt_s=melt(eval);class(melt_s)= "data.frame"
     as.factor(melt_s$Var2)
     names(melt_s)=c("tiempo","estaciones","precipitacion")
     
     graf=ggplot(melt_s,aes(x=tiempo,y=precipitacion,col=estaciones)) +
          geom_line()+labs(title=main) +
          labs(x = xlab,y =ylab)+
          theme_minimal()
     
     return(graf)
}

graf=ggplot_spatfd(data)+scale_colour_manual(values=wes_palette(n=nrow(data[[variable]]$coords)*1.5, name="GrandBudapest1",type="continuous"))

ggplotly(graf)

nrow(data[[1]]$coords)

ggplot_spatfd1=function(data,variable=1,estacion="XAL",main="Functional data",ylab="Value",xlab="Time"){
     data1=data.frame(Time=1:length(data[[variable]]$fdata$fdnames$time),Estacion=data[[variable]]$data[,estacion])
     data=data[[variable]]$fdata
     eval=eval.fd(1:length(data$fdnames$time),data)
     melt_s=melt(eval);class(melt_s)= "data.frame"
     as.factor(melt_s$Var2)
     names(melt_s)=c("tiempo","estaciones","precipitacion")
     melt_s=melt_s[melt_s$estaciones==estacion,]
     graf=ggplot() +
          geom_line(melt_s,mapping=aes(x=tiempo,y=precipitacion),col="red")+
          geom_point(data1,mapping=aes(x=Time,y=Estacion),col="red") +
          labs(title=main)+
          labs(x = xlab,y =ylab)+
          theme_minimal()
     return(graf)
}

graf=ggplot_spatfd1(ds)
graf