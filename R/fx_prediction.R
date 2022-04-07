## * fx_cmatrix
##' @export
fx_cmatrix <- function(actual.class, pred.class, pred.prob = NULL, parameters = NULL){
    
    class.levels <- parameters$class.levels
    
    cMatrix <- matrix(0,nrow=2,ncol=2,
                      dimnames=list(class.levels,
                                    class.levels))
    
    cMatrix[class.levels[1],class.levels[1]] <- 
        sum(pred.class==class.levels[1] & actual.class==class.levels[1])
    cMatrix[class.levels[2],class.levels[1]] <- 
        sum(pred.class==class.levels[2] & actual.class==class.levels[1])
    cMatrix[class.levels[1],class.levels[2]] <- 
        sum(pred.class==class.levels[1] & actual.class==class.levels[2])
    cMatrix[class.levels[2],class.levels[2]] <- 
        sum(pred.class==class.levels[2] & actual.class==class.levels[2])
    
    if (!is.null(pred.prob)){
        rocObj <- roc(actual.class, pred.prob)
    }
    
    perf <- list()
    perf$actual.class <- actual.class
    perf$pred.class <- pred.class
    perf$pred.prob <- pred.prob
    perf$decisionThreshold <- decisionThreshold
    perf$cMatrix <- cMatrix
    perf$descrip <- 'Rows: prediction, Columns: actual'
    perf$negative <- class.levels[1]
    perf$positive <- class.levels[2]
    
    # true positive
    perf$TP <- cMatrix[perf$positive, perf$positive]
    # false positive
    perf$FP <- cMatrix[perf$positive, perf$negative]
    # true negative
    perf$TN <- cMatrix[perf$negative, perf$negative]
    # false negative
    perf$FN <- cMatrix[perf$negative, perf$positive]
    # sensitivity
    perf$sensitivity <- perf$TP/(perf$TP + perf$FN)
    # specificity
    perf$specificity <- perf$TN/(perf$TN + perf$FP)
    # positive predictive value
    perf$ppv <- perf$TP/(perf$TP + perf$FP)
    # negative predictive value
    perf$npv <- perf$TN/(perf$TN + perf$FN)
    # accuracy
    perf$accuracy <- (perf$TP + perf$TN)/sum(cMatrix)
    
    if(is.null(pred.prob)){
        perf$auc.ROC <- NA
        perf$auc.95CI.delong <- NA
        perf$auc.95CI.boot <- NA
    } else {
        perf$auc.ROC <- auc(rocObj)
        perf$auc.95CI.delong <- suppressWarnings(ci(rocObj, of = 'auc')) 
        if(auc.boot){
            perf$auc.95CI.boot <- suppressWarnings(ci(rocObj, of = 'auc', method = 'boot'))
        } else {
            perf$auc.95CI.boot <- NA
        }
    }
    
    perf$parameters <- parameters
    return(perf)
} # deprecated

## * fx_modelVar
##' @export
fx_modelVar <- function(modelPerfObj, modelObj, nresample = 50, n.cores = 20){
    
    modelVarPerfObj <- list()
    updateMarks <- seq(from = 0, to = nresample, length.out = 11)
    
    parameters <- modelObj[[1]]$parameters
    df.resample <- get(modelObj[[1]]$parameters$data.frame)
    
    writeLines('Generating resample results...')
    
    for(j in seq(nresample)){
        
        if (j%in%updateMarks){
            writeLines(paste0('\tResample: ', j, ' (', (j/nresample)*100, '% complete)'))
        }
        
        resample.partitionList <- 
            fx_partition(df.resample, type = modelObj[[1]]$parameters$sample.type)
        resample.modelObj <- parallel::mclapply(seq(length(resample.partitionList)), function(i){
            fx_model(fx_sample(df.resample, 
                               resample.partitionList[[i]]), 
                     covar = covar, 
                     voi = voi, 
                     outcome = y, 
                     model.type = modelObj[[1]]$parameters$model.type)}, 
            mc.cores = 20)
        resample.modelPerfObj <- 
            fx_modelPerf(resample.modelObj, dthresh = modelPerfObj$parameters$decisionThreshold)
    
    resample.modelPerfObj$df.allfolds <- NULL
    resample.modelPerfObj$parameters <- NULL
    modelVarPerfObj[[j]] <- resample.modelPerfObj
    }
    
    writeLines('Resample complete!')
    return(modelVarPerfObj)
}

## * fx_modelVarPerf
##' @export
fx_modelVarPerf <- function(modelPerfObj, modelVarObj, measures = NULL, compute.perf = 'within', df.iter.out = T, qrange = c(0.025, 0.975)){
    
    parameters <- modelPerfObj$parameters
    parameters$nresample <- length(modelVarObj)
    parameters$nkfcv <- F
    parameters$compute.perf <- compute.perf
    
    regmodels <- c('regression')
    classmodels <- c('svm','rf','logistic')
    
    if(parameters$model.type%in%regmodels){
        measuresSet <- c('rmse', 'rsq')
    } else if(parameters$model.type%in%classmodels) {
        measuresSet <- c('acc', 'auc.ROC', 'sens', 'spec', 'ppv', 'npv', 'optThresh')
    }
    
    if(is.null(measures)){
        measures <- measuresSet
    } else {
        if(any(!measures %in% measuresSet)){
            stop(paste0('Unknown outcome measures: ', paste(measures[which(!measures%in%measuresSet)], collapse = ',')))
        }
    }
    
    df.iter <- as.data.frame(do.call(rbind,lapply(seq(parameters$nresample), function(i){
        modelVarObj[[i]]$perfMetrics[modelVarObj[[i]]$perfMetrics$fold==compute.perf,]
    })))
    
    avg <- colMeans(df.iter[,c(paste0(measures,'.covar'), paste0(measures,'.full'))])
    stdev <- apply(df.iter[,c(paste0(measures,'.covar'), paste0(measures,'.full'))],2,sd)
    ci <- sapply(names(avg), function(i){c(quantile(df.iter[,i], probs = qrange, na.rm = T))})
    df.pval <- as.data.frame(rbind(obs,stdev,ci))
    rownames(df.pval) <- c('avg','stdev',paste0(qrange*100,'%'))
    
    if(df.iter.out){
        return(list(df.iter=df.iter,
                    df.pval=df.pval,
                    parameters = parameters))
    } else {
        return(list(df.pval=df.pval,
                    parameters = parameters))
    }
    
    
}

## * fx_boot
##' @title Bootstrap Confidence Intervals
##' @description Bootstrap confidence intervals
##' @export
fx_boot <- function(df0, modelPerfObj, modelObj, partitionList, nboot = 10, n.cores = 20){

    bootPerfObj <- list()
    updateMarks <- seq(from = 0, to = nboot, length.out = 11)
    writeLines('Deriving bootstrap results...')

    parameters <- modelObj[[1]]$parameters
    if(is.numeric(parameters$sample.type)){
        parameters$nresample <- partitionList[[1]]$nresample
        parameters$balance.col <- partitionList[[1]]$balance.col
    }
    decisionThreshold <- modelPerfObj$parameters$decisionThreshold

    for (i in seq(nboot)){
        if (i%in%updateMarks){
            writeLines(paste0('\tBootstrap: ', i, ' (', (i/nboot)*100, '% complete)'))
        }
        
        regmodels <- c('regression')
        classmodels <- c('svm','rf','logistic')
        if (parameters$model.type%in%regmodels){
            outcome.class <- F
        } else if (parameters$model.type%in%classmodels){
            outcome.class <- T
        } else {stop(paste0('Unregcognized model.type (', parameters$model.type, ')'))}
        df.boot <- fx_scramble(df0, parameters$outcome,
                               outcome.class=outcome.class)

        modelObjPerm <- parallel::mclapply(seq(length(partitionList)), function(j){
            fx_model(fx_sample(df.boot,partitionList[[j]]),
                     covar = parameters$covar,
                     voi = parameters$voi,
                     outcome = parameters$outcome,
                     model.type = parameters$model.type)},
            mc.cores = n.cores)
        modelPerfObjPerm <- fx_modelPerf(modelObjPerm,
                                         dthresh = decisionThreshold)

        modelPerfObjPerm$df.allfolds <- NULL
        modelPerfObjPerm$parameters <- NULL
        bootPerfObj[[i]] <- modelPerfObjPerm
    }

    writeLines('Bootstrap complete!')
    return(bootPerfObj)
}

## * fx_bootPerf
##' @title Estimate Bootstrap Distributions
##' @description Estimate CIs and p-values from bootstrap distributions
##' @export
fx_bootPerf <- function(modelPerfObj, bootObj, measures = NULL, compute.perf = 'within', df.iter.out = T, qrange = c(0.025, 0.975)){
    
    parameters <- modelPerfObj$parameters
    parameters$nboot <- length(bootObj)
    parameters$nkfcv <- F
    parameters$compute.perf <- compute.perf
    
    regmodels <- c('regression')
    classmodels <- c('svm','rf','logistic')
    
    if(parameters$model.type%in%regmodels){
        measuresSet <- c('rmse', 'rsq')
    } else if(parameters$model.type%in%classmodels) {
        measuresSet <- c('acc', 'auc.ROC', 'sens', 'spec', 'ppv', 'npv', 'optThresh')
    }
    
    if(is.null(measures)){
        measures <- measuresSet
    } else {
        if(any(!measures %in% measuresSet)){
            stop(paste0('Unknown outcome measures: ', paste(measures[which(!measures%in%measuresSet)], collapse = ',')))
        }
    }
    
    df.iter <- as.data.frame(do.call(rbind,lapply(seq(parameters$nboot), function(i){
        bootObj[[i]]$perfMetrics[bootObj[[i]]$perfMetrics$fold==compute.perf,]
    })))
    
    obs <- modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,
                                    c(paste0(measures,'.covar'), paste0(measures,'.full'))]
    ci <- sapply(names(obs), function(i){c(quantile(df.iter[,i], probs = qrange, na.rm = T))})
    df.pval <- as.data.frame(rbind(obs,ci))
    rownames(df.pval)[1] <- 'obs'
    
    if(df.iter.out){
        return(list(df.iter=df.iter,
                    df.pval=df.pval,
                    parameters = parameters))
    } else {
        return(list(df.pval=df.pval,
                    parameters = parameters))
    }
    
    
}









