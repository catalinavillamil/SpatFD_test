OSFCOK=function(SFD,modelo,fixcoords,movcoords, S,xbound,ybound,...){
     N=nrow(movcoords)
     lower=c(rep(xbound[1],N),rep(ybound[1],N))
     upper=c(rep(xbound[2],N),rep(ybound[2],N))
     movcoords=c(as.matrix(movcoords))
     return(GenSA(movcoords,.sumvar,lower,upper,SFD=SFD,modelo=modelo,fixcoords=fixcoords,S=S,byrow=TRUE,control = list(...)))
}