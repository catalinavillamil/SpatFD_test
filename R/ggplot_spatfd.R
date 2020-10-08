ggplot_spatfd=function(data,variable=1,main="Functional data",ylab=NULL,xlab="Time"){
     ylab=ifelse(is.null(ylab),deparse(substitute(data)),ylab)
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