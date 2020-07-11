FKSK=function(SFD, newcoords,model,j=1,fill.all=NULL){
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

        #model

        if(!(inherits(model,"variogramModel") || is.list(model))){
                stop("Wrong class of model, model should be of class variogramModel or a list of them (use vgm) ")
        }
        if(is.list(model) && !all(lapply(model,inherits,"variogramModel"))){
                stop("Wrong class of model, each element of list should be of class variogramModel")
        }
        if(inherits(model,"variogramModel")){}
        if(inherits(model,"list")){}

        #j
        if ((is.null(j)  || !(is.numeric(j)&& length(j)==1))){
                stop("Wrong class of j object")
        }

        #fill.all
        if ( !( ( isTRUE(fill.all) || isFALSE(fill.all) ) && length(fill.all)==1 ) ){
                stop("Wrong class of fill.all object")
        }

        # messages default values
        if(missing(j)){
                message("Using first variable by default")
        }
        if(missing(fill.all)){
                message("Using fill.all = TRUE by default")
        }

        puntaje=SFD[[j]]$fpca$scores
        rownames(puntaje)=SFD[[j]]$cn
        puntajes=as.data.frame(puntaje)
        coordinates(puntajes)=SFD[[j]]$coords
        if(fill.all==T){
          v=list()
          fv=list()
          for (i in 1:ncol(puntajes)){

               v[[i]]=variogram(puntajes[[i]]~1,puntajes)
               fv[[i]]=fit.variogram(v[[i]],model)
          }
        }else{
          v=list()
          fv=list()
          for (i in 1:ncol(puntajes)){

               v[[i]]=variogram(puntajes[[i]]~1,puntajes)
               fv[[i]]=fit.variogram(v[[i]],model[[i]])
          }
        }
        colnames(newcoords)=c('x','y')
        coordinates(newcoords)=~x+y
        #kriging:
        K=list()
        for (i in 1:ncol(puntajes)){
          K[[i]] <- krige(puntajes[[i]]~1,puntajes,newcoords, model = fv[[i]])
        }
        #### falta tener en cuenta para mas dimensiones, si es mas de uno, cokriging entre scores

        #CV

        #cv <- krige.cv(puntajes$V1~1, model= fv, nmax = 40)

        ##para retornar
        for (i in 1:ncol(puntajes)){
          grid.arrange(spplot(K[[i]]['var1.pred'], main = "ordinary kriging predictions"),spplot(K[[i]]['var1.var'], main = "ordinary kriging variance"))
        }
        ##falta meter los datos predichos
        pred=K[[1]]$var1.pred
        if(ncol(puntajes)>1){
          for (i in 2:ncol(puntajes)){
               pred=cbind(pred,K[[i]]$var1.pred)
          }
        }


        # plot(x=c(0,1000),y=c(-50,140))
        # for (i in 1:nrow(pred)){
        #       lines(SFD[[j]][["fpca"]][["meanfd"]]+sum((pred[i,]*SFD[[j]][["fpca"]][["harmonics"]])),col=i)
        # }

        fpred=list()
        for( i in 1:nrow(pred)){
          fpred[[i]]=SFD[[j]][["fpca"]][["meanfd"]]+sum((pred[i,]*SFD[[j]][["fpca"]][["harmonics"]]))
        }
        vari=K[[1]]$var1.var
        if(ncol(puntajes)>1){
          for (i in 2:ncol(puntajes)){
               vari=cbind(vari,K[[i]]$var1.vari)
          }
        }
        # plot(x=c(0,1000),y=c(-5000,80000))
        # for (i in 1:nrow(vari)){
        #       lines(SFD[[j]][["fpca"]][["meanfd"]]+sum((vari[i,]*SFD[[j]][["fpca"]][["harmonics"]])),col=i)
        # }


        fvari=list()
        for( i in 1:nrow(vari)){
          fvari[[i]]=SFD[[j]][["fpca"]][["meanfd"]]+sum((vari[i,]*SFD[[j]][["fpca"]][["harmonics"]]))
        }

        ret=list(SFD=SFD,model=fv,fpred=fpred,fvar=fvari)# hacer cv o no? agregar krigin(K) o no?
        class(ret)='FKSK'
        return(ret)

}
