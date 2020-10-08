scores=function(SFD){

     puntaje=list()
     puntajes=list()
     for(j in 1:length(SFD)){


     puntaje[[j]]=SFD[[j]]$fpca$scores
     rownames(puntaje[[j]])=SFD[[j]]$coordsnames
     puntajes[[j]]=as.data.frame(puntaje[[j]])
     }
     return(puntajes)
}
