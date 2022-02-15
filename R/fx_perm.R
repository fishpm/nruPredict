### Perform permutation resampling

fx_perm <- function(df0, modelObj, nperm = 10, n.cores = 20){
    
    updateMarks <- seq(from = 0, to = nperm, length.out = 11)
    writeLines('Deriving permutation results...')
    
    parameters <- modelObj$parameters
    parameters$nperm <- nperm
    if(is.numeric(parameters$sample.type)){
        
        stop('Permutation does not support numeric sample types.')
        
    }
    
    permPerfObj <- lapply(seq(nperm), function(i){
        if (i%in%updateMarks){
            writeLines(paste0('\tPermutation: ', i, ' (', (i/nperm)*100, '% complete)'))
        }
        
        df.scramble <- fx_scramble(df0,parameters$outcome)
        if(is.numeric(parameters$sample.type)){
            
            stop('Permutation does not support numeric sample types.')
            
        } else {
            
            partition.list <- fx_partition(df.scramble, type = parameters$sample.type)
            
        }
        
        modelObjPerm <- mclapply(seq(length(partition.list)), function(j){
            fx_model(fx_sample(df.scramble,partition.list[[j]]),
                     covar = parameters$covar,
                     voi = parameters$voi,
                     outcome = parameters$outcome,
                     model.type = parameters$model.type)},
            mc.cores = n.cores)
        modelPerfObjPerm <- fx_modelPerf(modelObjPerm, 
                                         dthresh = parameters$dthresh)
        
        modelPerfObjPerm$df.allfolds <- NULL
        modelPerfObjPerm$parameters <- NULL
        return(modelPerfObjPerm)
    })
    
    writeLines('Permutation testing complete!')
    return(list(permPerfObj=permPerfObj,
                parameters=parameters))
}
