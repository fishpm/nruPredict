#' @title xxx
#' @description Short description of the function
#' @name fx_modelPerf
#' 
#' @param modelOutput aa
#' @param dthresh bb
#' @param perm If TRUE, .... Otherwise ...
#' @param many cc
#'
#' @return Describe output of the function
#'
#' @examples
#' # R code showing how to use the function
#' 1+1
#'
#' @export

fx_modelPerf <- function(modelOutput, dthresh = 0.5, many = T, perm = F){
    
    if (!many) {
        
    } else {
        
        n.models <- length(modelOutput)
        params.set <- !(names(modelOutput[[1]]$parameters)=='train.rows'|
                            names(modelOutput[[1]]$parameters)=='test.rows')
        parameters <- modelOutput[[1]][['parameters']][params.set]
        class.levels <- parameters$class.levels
        
        nrows.df <- length(modelOutput[[1]]$parameters$train.rows)+
            length(modelOutput[[1]]$parameters$test.rows)
        
        regmodels <- c('regression','rf.regression')
        classmodels <- c('svm','rf','logistic')
        
        if(modelOutput[[1]]$parameters$model.type%in%regmodels){
            
            df.allfolds <- do.call(rbind, lapply(
                lapply(seq(n.models), function(j){
                    return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
                                fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
                                pred.values.covar = modelOutput[[j]]$pred.covar$pred.values,
                                pred.values.full = modelOutput[[j]]$pred.full$pred.values,
                                actual.values = modelOutput[[j]]$pred.full$actual.values))
                }),
                data.frame))
            
        } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
            
            df.allfolds <- do.call(rbind, lapply(
                lapply(seq(n.models), function(j){
                    return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
                                fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
                                pred.prob.covar = modelOutput[[j]]$pred.covar$pred.prob,
                                pred.prob.full = modelOutput[[j]]$pred.full$pred.prob,
                                pred.class.covar = modelOutput[[j]]$pred.covar$pred.class,
                                pred.class.full = modelOutput[[j]]$pred.full$pred.class,
                                actual.class = modelOutput[[j]]$pred.full$actual.class))
                }),
                data.frame))
            
            pred.class.covar.not.defined <- all(is.na(df.allfolds$pred.class.covar))
            if(pred.class.covar.not.defined){
                
                df.allfolds$pred.class.covar <- class.levels[as.numeric(df.allfolds$pred.prob.covar>dthresh)+1]
                df.allfolds$pred.class.full <- class.levels[as.numeric(df.allfolds$pred.prob.full>dthresh)+1]
                
            }
            
            parameters$cmat.descrip <- 'Rows: prediction, Columns: actual'
            parameters$negative.class <- parameters$class.levels[1]
            parameters$positive.class <- parameters$class.levels[2]
            parameters$dthresh <- dthresh
            
        } else {
            
            stop(paste0('Not sure how to handle: ', modelOutput[[1]]$parameters$model.type))
            
        }
        
    }
    
    foldPerf <- lapply(c('across',unique(df.allfolds$fold)), function(i){
        
        if(i=='across'){
            
            firstrows <- sapply(unique(df.allfolds$orig.df.row), function(j){
                return(which(df.allfolds$orig.df.row==j)[1])
            })
            
            df.tmp <- df.allfolds[firstrows,]
            
        } else {
            
            df.tmp <- df.allfolds[df.allfolds$fold==i,]
        }
        
        if(parameters$model.type%in%regmodels){
            
            foldrsq.full <- sapply(seq(n.models), function(j){modelOutput[[j]]$pred.full$rsq[1]})
            rmse.full <- sqrt(mean((df.tmp$pred.values.full-df.tmp$actual.values)**2))
            
            if(!is.null(parameters$covar)){
                
                foldrsq.covar <- sapply(seq(n.models),function(j){modelOutput[[j]]$pred.covar$rsq[1]})
                rmse.covar <- sqrt(mean((df.tmp$pred.values.covar-df.tmp$actual.values)**2))
                
            } else {
                
                foldrsq.covar <- rep(NA,n.models)
                rmse.covar <- NA
                
            }
            
            if(i=='across'){
                
                rsq.full <- mean(foldrsq.full)
                if(!is.null(parameters$covar)){
                    rsq.covar <- mean(foldrsq.covar)
                } else {
                    rsq.covar <- NA
                }
                
            } else {
                
                rsq.full <- foldrsq.full[as.numeric(i)]
                if(!is.null(parameters$covar)){
                    rsq.covar <- foldrsq.covar[as.numeric(i)]
                } else {
                    rsq.covar <- NA
                }
            }
            
            perfMetrics <- data.frame(rmse.covar = rmse.covar,
                                      rmse.full = rmse.full,
                                      rsq.covar = rsq.covar,
                                      rsq.full = rsq.full)
            
            return(list(perfMetrics = perfMetrics, fold = i))
            
        } else if(parameters$model.type%in%classmodels){
            
            cmat.full <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
            cmat.full[class.levels[1],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[1])
            cmat.full[class.levels[2],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[1])
            cmat.full[class.levels[1],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[2])
            cmat.full[class.levels[2],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[2])
            
            if(!is.null(parameters$covar)){
                cmat.covar <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
                cmat.covar[class.levels[1],class.levels[1]] <-
                    sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[1])
                cmat.covar[class.levels[2],class.levels[1]] <-
                    sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[1])
                cmat.covar[class.levels[1],class.levels[2]] <-
                    sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[2])
                cmat.covar[class.levels[2],class.levels[2]] <-
                    sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[2])
            } else {
                cmat.covar <- matrix(NA,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
            }
            
            perfMetrics <- data.frame(TP.covar = cmat.covar[parameters$positive.class, parameters$positive.class],
                                      FP.covar = cmat.covar[parameters$positive.class, parameters$negative.class],
                                      TN.covar = cmat.covar[parameters$negative.class, parameters$negative.class],
                                      FN.covar = cmat.covar[parameters$negative.class, parameters$positive.class])
            perfMetrics$sens.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FN.covar)
            perfMetrics$spec.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FP.covar)
            perfMetrics$ppv.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FP.covar)
            perfMetrics$npv.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FN.covar)
            perfMetrics$acc.covar <- (perfMetrics$TP.covar + perfMetrics$TN.covar)/sum(cmat.covar)
            
            if(!is.null(parameters$covar)){
                
                rocCompute.covar <- fx_rocCompute(pred.prob = df.tmp$pred.prob.covar,
                                                  actual.class = df.tmp$actual.class,
                                                  class.levels = class.levels)
                perfMetrics$auc.ROC.covar <- rocCompute.covar$roc.auc
                perfMetrics$optThresh.covar <- rocCompute.covar$optimal.threshold
                
            } else {
                
                perfMetrics$auc.ROC.covar <- perfMetrics$optThresh.covar <- NA
                
            }
            
            if(!all(is.na(df.tmp$pred.prob.full))){
                
                rocCompute.full <- fx_rocCompute(pred.prob = df.tmp$pred.prob.full,
                                                 actual.class = df.tmp$actual.class,
                                                 class.levels = class.levels)
                
                perfMetrics$auc.ROC.full <- rocCompute.full$roc.auc
                perfMetrics$optThresh.full <- rocCompute.full$optimal.threshold
                
            } else {
                
                perfMetrics$auc.ROC.full <- perfMetrics$optThresh.full <- NA
                
            }
            
            perfMetrics$TP.full <- cmat.full[parameters$positive.class, parameters$positive.class]
            perfMetrics$FP.full <- cmat.full[parameters$positive.class, parameters$negative.class]
            perfMetrics$TN.full <- cmat.full[parameters$negative.class, parameters$negative.class]
            perfMetrics$FN.full <- cmat.full[parameters$negative.class, parameters$positive.class]
            perfMetrics$sens.full <- perfMetrics$TP.full/(perfMetrics$TP.full + perfMetrics$FN.full)
            perfMetrics$spec.full <- perfMetrics$TN.full/(perfMetrics$TN.full + perfMetrics$FP.full)
            perfMetrics$ppv.full <- perfMetrics$TP.full/(perfMetrics$TP.full + perfMetrics$FP.full)
            perfMetrics$npv.full <- perfMetrics$TN.full/(perfMetrics$TN.full + perfMetrics$FN.full)
            perfMetrics$acc.full <- (perfMetrics$TP.full + perfMetrics$TN.full)/sum(cmat.full)
            
            return(list(cmat.covar = cmat.covar, cmat.full = cmat.full, perfMetrics = perfMetrics, fold = i))
            
        }
    })
    
    if(modelOutput[[1]]$parameters$model.type%in%regmodels){
        
        perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
        perfMetrics <- as.data.frame(cbind(perfMetrics,do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
        perfMetrics$fold <- as.character(perfMetrics$fold)
        perfMetrics[nrow(perfMetrics)+1,'fold'] <- 'within'
        perfMetrics[nrow(perfMetrics),colnames(perfMetrics)[colnames(perfMetrics)!='fold']] <- 
            unlist(lapply(colnames(perfMetrics)[colnames(perfMetrics)!='fold'], 
                          function(i){mean(perfMetrics[perfMetrics$fold%in%unique(df.allfolds$fold),i],na.rm=T)}))
        cmat.covar <- NULL
        cmat.full <- NULL
        
    } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
        
        perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
        perfMetrics <- as.data.frame(cbind(perfMetrics, do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
        perfMetrics$fold <- as.character(perfMetrics$fold)
        perfMetrics[nrow(perfMetrics)+1,'fold'] <- 'within'
        perfMetrics[nrow(perfMetrics),colnames(perfMetrics)[colnames(perfMetrics)!='fold']] <- 
            unlist(lapply(colnames(perfMetrics)[colnames(perfMetrics)!='fold'], 
                          function(i){mean(perfMetrics[perfMetrics$fold%in%unique(df.allfolds$fold),i], na.rm=T)}))
        cmat.covar <- foldPerf[[1]]$cmat.covar
        cmat.full <- foldPerf[[1]]$cmat.full
        
    }
    
    return(list(perfMetrics = perfMetrics, 
                parameters = parameters, 
                cmat.covar = cmat.covar, 
                cmat.full = cmat.full, 
                df.allfolds = df.allfolds))
    
}