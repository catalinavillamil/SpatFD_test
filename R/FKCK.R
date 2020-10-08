FKCK=function(SFD,newcoords,model,vari=1,fill.all=T){
        #----------------------------------------------------------------------------
        #           VALIDANDO ARGUMENTOS *
        #----------------------------------------------------------------------------
        #all
        if(missing(SFD)){
                stop("Missing SFD")
        }
        if (missing(newcoords)){
                stop("Missing new coords")
        }
        if(missing(model)){
                stop("Missing model")
        }
        #SFD
        if(!inherits(SFD,"SpatFD")){
                stop("SFD must be an object SpatFD")
        }
        
        #newcoords
        if(!(is.matrix(newcoords) || is.data.frame(newcoords))){
                stop("Wrong class of newcoords object")
        }else if(!all(apply(newcoords, c(1,2), is.numeric))){
                stop("Newcoords must be numeric data")
        }else if(any(is.na(newcoords))){
                stop("There is some NA value in newcoords")
        }
        
        # messages default values
        if(missing(vari)){
                message("Using first variable by default")
        }
        if(missing(fill.all)){
                message("Using fill.all = TRUE by default")
        }
        
        #vari
        
        if(is.null(vari)){
                vari=1
        } else if ((is.character(vari)&& length(vari)==1)){
                if (length(which(names(SFD)==vari))==1){
                        vari=which(names(SFD)==vari)
                }else if (length(which(names(SFD)==vari))==0){
                        stop(paste(vari,"doesn't not exists. Change vari for an existing variable name."))
                }else if (length(which(names(SFD)==vari))==0){
                        stop("There are more than one variable with the same name")
                }
        }
        if ((is.null(vari)  || !(is.numeric(vari)&& length(vari)==1))){
                stop("Wrong class of vari object")
        }
        
        #fill.all
        if ( !( ( isTRUE(fill.all) || isFALSE(fill.all) ) && length(fill.all)==1 ) ){
                stop("Wrong class of fill.all object")
        }
        
        
        #model
        
        if(!(inherits(model,"variogramModel") || inherits(model,"list"))){
                stop("Wrong class of model, model should be of class variogramModel or a list of them (use vgm of gstat package) ")
        }else if(inherits(model,"list") && !all(lapply(model,inherits,"variogramModel"))){
                stop("Wrong class of model, each element of list should be of class variogramModel (use vgm of gstat package)")
        }#else if(inherits(model,"list") && (length(model)!=ncol(as.data.frame(SFD[[vari]]$fpca$scores)))){
         #       stop("length of list of models must be equal to number of harmonics of the choosen variable ")
        #}else if(inherits(model,"variogramModel") && !(fill.all || (ncol(as.data.frame(SFD[[vari]]$fpca$scores))==1))){
         #       stop("If model is not a list and there are more than one nharm of that variable, then fill.all must be TRUE or you can create a list of models with the same number of harmonics")
        #}
        
        ### agregar validaciones por length de nharm combinations
        
     #
     #agregar sugerencia de vp> 0.5 por componente
     puntaje=SFD[[vari]]$fpca$scores
     rownames(puntaje)=SFD[[vari]]$coordsnames
     puntajes=as.data.frame(puntaje)
     coordinates(puntajes)=SFD[[vari]]$coords

     #Estimador de la silla: varianza de cada componente (valor propio del comp principal)
     #todos son estacionarios de segundo orden y tienen silla y es exactamente igual al vp de cada comp principal
     #para estimar la silla del semivariog se tiene metodos de geoestadistica o tomando como silla vaprop
     colnames(newcoords)=c('x','y')
     coordinates(newcoords)=~x+y
     #model=vgm(1000,'Gau',11000)
     #  for (i in 2:ncol(puntajes)){
     #    g=gstat(g,colnames(SFD[[vari]][["fpca"]][["harmonics"]][["coefs"]])[i],puntajes[[i]]~1,puntajes)
     #  }
     
     ii=1:ncol(puntajes)
     cc=paste0(c("g=gstat(,colnames(SFD[[vari]][[\"fpca\"]][[\"harmonics\"]][[\"coefs\"]])[",rep("g=gstat(g,colnames(SFD[[vari]][[\"fpca\"]][[\"harmonics\"]][[\"coefs\"]])[",ncol(puntajes)-1)),ii,rep("],puntajes[[",ncol(puntajes)),ii,rep("]]~1,puntajes)",ncol(puntajes)))
     eval(parse(text=cc))

     g <- gstat(g, model=model, fill.all=fill.all)

     vg <- variogram(g)
     mcl = fit.lmc(vg, g)

     plot(vg, model = mcl)

     z = predict(mcl, newdata = newcoords)
     #CV

     #cv <- krige.cv(puntajes$V1~1, model= fv, nmax = 40)

     ##para retornar
     for (i in 1:ncol(puntajes)){
          grid.arrange(spplot(z[2*i-1], main = "ordinary kriging predictions"),spplot(z[2*i], main = "ordinary kriging variance"))  }
     ##falta meter los datos predichos
     pred=z[1][[1]]
     if(ncol(puntajes)>1){
          for (i in 2:ncol(puntajes)){
               pred=cbind(pred,z[2*i-1][[1]])
          }
     }


     # plot(x=c(0,1000),y=c(-50,140))
     # for (i in 1:nrow(pred)){
     #       lines(SFD[[vari]][["fpca"]][["meanfd"]]+sum((pred[i,]*SFD[[vari]][["fpca"]][["harmonics"]])),col=i)
     # }

     fpred=list()
     for( i in 1:nrow(pred)){
          fpred[[i]]=SFD[[vari]][["fpca"]][["meanfd"]]+sum((pred[i,]*SFD[[vari]][["fpca"]][["harmonics"]]))
     }
     # vari=z[2][[1]]
     # if(ncol(puntajes)>1){
     #      for (i in 2:ncol(puntajes)){
     #           vari=cbind(vari,z[2*i][[1]])
     #      }
     # }
     # plot(x=c(0,1000),y=c(-5000,80000))
     # for (i in 1:nrow(vari)){
     #       lines(SFD[[vari]][["fpca"]][["meanfd"]]+sum((vari[i,]*SFD[[vari]][["fpca"]][["harmonics"]])),col=i)
     # }


     # fvari=list()
     # for( i in 1:nrow(vari)){
     #      fvari[[i]]=SFD[[vari]][["fpca"]][["meanfd"]]+sum((vari[i,]*SFD[[vari]][["fpca"]][["harmonics"]]))
     # }

     ret=list(SFD=SFD,model=mcl,fpred=fpred)
     # hacer cv o no? agregar krigin(K) o no?
     class(ret)='FKCK'
     return(ret)
}
