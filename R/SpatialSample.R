SpatialSample=function(x,n,type,r,SFD,modelo,fixcoords,S,...){
     
     s=spsample(x,n,type,...)@coords
     c=combinations(nrow(s),r)
     sam=s[c[,1],]
     if(r>1){
          for(i in 1:(r-1)){
               sam=cbind(sam,s[c[,i+1],])
          }
     }
     m=which.min(apply(X=sam,MARGIN=1,FUN=.sumvar,SFD=SFD,modelo=modelo,fixcoords=fixcoords,S=S))
     loc=matrix(sam[m,],,2,byrow=TRUE)
     colnames(loc)=c("X","Y")
     return(loc)
}