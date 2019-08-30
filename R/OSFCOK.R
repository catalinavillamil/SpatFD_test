OSFCOK=function(X,modelo, S,...){
        
       return(optim(S,.sumvar,method="SANN",X=X,modelo=modelo,control = list(...)))
        
}