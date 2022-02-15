### Summarizaes bootstrap performance


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
