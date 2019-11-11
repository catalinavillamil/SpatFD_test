.sumvar=function(SFD,modelo,fixcoords,movcoords,S,byrow=TRUE){
     s=0
     movcoords=matrix(movcoords,,2,byrow=byrow)
     for(i in seq(1,length(S),2)){
        s=.varfcok(SFD,modelo,fixcoords,movcoords,s0=c(S[i],S[i+1]))  
     }
     return(s)
}