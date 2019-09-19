OSFCOK=function(X,modelo,fixcoords,movcoords, S,...){
        
       return(optim(movcoords,.sumvar,method="SANN",X=X,modelo=modelo,fixcoords=fixcoords,S=S,control = list(...)))
        
}