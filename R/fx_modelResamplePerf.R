## * fx_modelResamplePerf (documentation)
##' @description Estimates effects across model variants

## * fx_modelResamplePerf (code)
##' @export
fx_modelResamplePerf <- function(modelResampleObj, measures = NULL, compute.perf = 'within', df.iter.out = T, qrange = c(0.025, 0.975)){
    
    parameters <- modelResampleObj$parameters
    
    # "within" doesn't make sense for loocv
    if(parameters$sample.type=='loocv'){
        writeLines('LOOCV - resetting compute.perf to across...')
        compute.perf <- 'across'
    }
    
    parameters$compute.perf <- compute.perf
    
    regmodels <- c('regression','rf.regression')
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
        df.match <- modelResampleObj$modelResamplePerfObj[[i]]$perfMetrics
        row.match <- modelResampleObj$modelResamplePerfObj[[i]]$perfMetrics$fold==parameters$compute.perf
        df.match[row.match,]
        
    })))
    
    avg <- colMeans(df.iter[,c(paste0(measures,'.covar'), paste0(measures,'.full'))])
    stdev <- apply(df.iter[,c(paste0(measures,'.covar'), paste0(measures,'.full'))],2,sd)
    ci <- sapply(names(avg), function(i){c(quantile(df.iter[,i], probs = qrange, na.rm = T))})
    df.summary <- as.data.frame(rbind(avg,stdev,ci))
    rownames(df.summary) <- c('avg','stdev',paste0(qrange*100,'%'))
    
    if(df.iter.out){
        return(list(df.iter=df.iter,
                    df.summary=df.summary,
                    parameters = parameters))
    } else {
        return(list(df.summary=df.summary,
                    parameters = parameters))
    }
    
}
