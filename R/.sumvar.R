.sumvar=function(X,modelo,fixcoords,movcoords,S){
     s=0
     for(i in seq(1,length(S),2)){
        s=.varfcok(X,modelo,fixcoords,movcoords,s0=c(S[i],S[i+1]))  
     }
     return(s)
}