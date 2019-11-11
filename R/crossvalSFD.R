crossvalSFD=function(data,coords,basis="Bsplines",nbasis=4,lambda=0,nharm=NULL,vp=NULL,add=NULL,fn="FKSK",model,j=1,fill.all=T,...){
     Predictions=list()
     all=SpatFD(data,coords,basis="Bsplines",nbasis,lambda,nharm,vp,add,...)$fpred
     
     for(i in 1:ncol(data)){
          SFD=SpatFD(data[,-i],coords[-i,],basis="Bsplines",nbasis,lambda,nharm,vp,add,...)
          if(fn=="FKSK"){
               Predictions[i]=FKSK(SFD,coords[i,],model,j,fill.all)$fpred
          }else if(fn=="FKCK"){
               Predictions[i]=FKCK(SFD,coords[i,],model,j,fill.all)$fpred
          }else{
               stop("Error: fn is not a valid function")
               }
     }
    
     return(all,Predictions)
     
}