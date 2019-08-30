.sumvar=function(X,modelo,S){
     s=0
     for(i in seq(1,length(S),2)){
        s=.varfcok(X,modelo,s0=c(S[i],S[i+1]))  
     }
     return(s)
}