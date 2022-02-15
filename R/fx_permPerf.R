### Summarizes permutation performance

fx_permPerf <- function(permObj, modelResamplePerf, measures = NULL, compute.perf = 'within', df.iter.out = T){
    
    parameters <- permObj$parameters
    
    if(parameters$sample.type=='loocv'){
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
    
    df.iter <- as.data.frame(do.call(rbind,lapply(seq(parameters$nperm), function(i){
        df.match <- permObj$permPerfObj[[i]]$perfMetrics
        row.match <- permObj$permPerfObj[[i]]$perfMetrics$fold==compute.perf
        return(df.match[row.match,])
    })))
    
    obs <- modelResamplePerf$df.summary['avg',c(paste0(measures,'.covar'),paste0(measures,'.full'))]
    pval <- sapply(names(obs), function(i){
        (sum(df.iter[,i]>obs[[i]],na.rm=T)+(sum(df.iter[,i]==obs[[i]],na.rm=T)*0.5))/sum(!is.na(df.iter[,i]))
    })
    df.summary <- as.data.frame(rbind(obs,pval))
    rownames(df.summary) <- c('obs','pval')
    
    #HIGHER rmse is LESS desirable
    if (parameters$model.type%in%regmodels){
        df.summary['pval', grep('^(rmse)', colnames(df.summary))] <- 1-df.summary['pval', grep('^(rmse)', colnames(df.summary))]
    }

    if(df.iter.out){
        return(list(df.iter=df.iter,
                    df.summary=df.summary,
                    parameters = parameters))
    } else {
        return(list(df.summary=df.summary,
                    parameters = parameters))
    }
    
}
