### Performs bootstrap resmapling

# quick test

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
                               boot=T, outcome.class=outcome.class)
        
        modelObjPerm <- mclapply(seq(length(partitionList)), function(j){
            fx_model(fx_sample(df.boot,partitionList[[j]]),
                     covar = parameters$covar,
                     voi = parameters$voi,
                     outcome = parameters$outcome,
                     model.type = parameters$model.type)},
            mc.cores = n.cores)
        modelPerfObjPerm <- fx_modelPerf(modelObjPerm,
                                         decisionThreshold = decisionThreshold)
        
        modelPerfObjPerm$df.allfolds <- NULL
        modelPerfObjPerm$parameters <- NULL
        bootPerfObj[[i]] <- modelPerfObjPerm
    }
    
    writeLines('Bootstrap complete!')
    return(bootPerfObj)
}
