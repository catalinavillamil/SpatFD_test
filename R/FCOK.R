FCOK=function(SFD,newcoords,model,j=1,fill.all=T){
     #
     #agregar sugerencia de vp> 0.5 por componente
     puntaje=list()
     puntajes=list()
     for(k in 1:length(SFD)){
          puntaje[[k]]=SFD[[k]]$fpca$scores
          rownames(puntaje[[k]])=SFD[[k]]$cn
          puntajes[[k]]=as.data.frame(puntaje[[k]])
          coordinates(puntajes[[k]])=SFD[[k]]$coords
     }
     #Estimador de la silla: varianza de cada componente (valor propio del comp principal)
     #todos son estacionarios de segundo orden y tienen silla y es exactamente igual al vp de cada comp principal
     #para estimar la silla del semivariog se tiene metodos de geoestadistica o tomando como silla vaprop
     colnames(newcoords)=c('x','y')
     coordinates(newcoords)=~x+y
     aa=rep(1:length(SFD),lapply(puntajes,ncol))
     bb=unlist(lapply(lapply(puntajes,ncol),seq))
     #model=vgm(1000,'Gau',11000)
     cc=paste0(
             c("g=gstat(,paste(colnames(SFD[[",rep("g=gstat(g,paste(colnames(SFD[[",(length(aa)-1))),
             aa,
             rep("]][[\"fpca\"]][[\"harmonics\"]][[\"coefs\"]])[",length(aa)),
             bb,
             rep("],",length(aa)),
             aa,
             rep(",sep=\".\"),puntajes[[",length(aa)),
             aa,
             rep("]][[",length(aa)),
             bb,
             rep("]]~1,puntajes[[",length(aa)),
             aa,
             rep("]])",length(aa))
                 )
     
     eval(parse(text=cc))
     

     g <- gstat(g, model=model, fill.all=fill.all)

     vg <- variogram(g)
     mcl = fit.lmc(vg, g, fit.method=2)

     plot(vg, model = mcl)

     z = predict(mcl, newdata = newcoords)
     #CV

     #cv <- krige.cv(puntajes$V1~1, model= fv, nmax = 40)
     p=0
     for( i in 1:length(SFD)){
          p=p+ncol(puntajes[[i]])
     }
     ##para retornar
     for (i in 1:p){
          grid.arrange(spplot(z[2*i-1], main = "ordinary kriging predictions"),spplot(z[2*i], main = "ordinary kriging variance"))  }
     #falta meter los datos predichos
     pred=z[1][[1]]
     vari=z[2][[1]]
     if(p>1){
          for (k in 2:p){
               pred=cbind(pred,z[2*k-1][[1]])
               vari=cbind(vari,z[2*k][[1]])
          }
     }


     # plot(x=c(0,1000),y=c(-50,140))
     # for (i in 1:nrow(pred)){
     #       lines(SFD[[j]][["fpca"]][["meanfd"]]+sum((pred[i,]*SFD[[j]][["fpca"]][["harmonics"]])),col=i)
     # }
     if(j==1){
          fpred=list()
          fvari=list()
          for( i in 1:nrow(pred)){
               fpred[[i]]=SFD[[j]][["fpca"]][["meanfd"]]+sum((pred[i,1:ncol(puntajes[[j]])]*SFD[[j]][["fpca"]][["harmonics"]]))
               fvari[[i]]=SFD[[j]][["fpca"]][["meanfd"]]+sum((vari[i,1:ncol(puntajes[[j]])]*SFD[[j]][["fpca"]][["harmonics"]]))

          }
     }else{r=0
     for( i in 1:(j-1)){
          r=r+ncol(puntajes[[i]])
     }
     fpred=list()
     for( i in 1:nrow(pred)){
          fpred[[i]]=SFD[[j]][["fpca"]][["meanfd"]]+sum((pred[i,r:r+ncol(puntajes[[j]])]*SFD[[j]][["fpca"]][["harmonics"]]))
          fvari[[i]]=SFD[[j]][["fpca"]][["meanfd"]]+sum((vari[i,r:r+ncol(puntajes[[j]])]*SFD[[j]][["fpca"]][["harmonics"]]))

     }

     }


     # plot(x=c(0,1000),y=c(-5000,80000))
     # for (i in 1:nrow(vari)){
     #       lines(SFD[[j]][["fpca"]][["meanfd"]]+sum((vari[i,]*SFD[[j]][["fpca"]][["harmonics"]])),col=i)
     # }


     ret=list(SFD=SFD,model=mcl,fpred=fpred,fvar=fvari)
     # hacer cv o no? agregar krigin(K) o no?
     class(ret)='FCOK'
     return(ret)
}
