## * fx_perm (documentation)
##' @title Perform Permutation Resampling
##' @description Perform permutation resampling to obtain a null distribution
##'
##' @param df0 ???
##' @param modelObj ???
##' @param nperm ???
##' @param nresample ???
##' @param perm.positions ???
##' @param partitions ???
##' @param n.cores ??? 

## * fx_perm (code)
##' @export
fx_perm <- function(df0, modelObj, nperm = 10, nresample = 1, perm.positions = NULL, partitions = NULL, n.cores = 20){
    
    updateMarks <- seq(from = 0, to = nperm, length.out = 11)
    writeLines('Deriving permutation results...')
    
    if(xor(is.null(perm.positions),is.null(partitions))){
        stop('perm.positions and partitions must both be either null or specified')
    }
    
    parameters <- modelObj$parameters
    parameters$nperm <- nperm
    parameters$nperm.nresample <- nresample
    if(is.numeric(parameters$sample.type)){
        stop('Permutation does not support numeric sample types.')
    }
    
    permPerfObj <- list()
    for (i in seq(nperm)){
        
        if (i%in%updateMarks){
            writeLines(paste0('\tPermutation: ', i, ' (', (i/nperm)*100, '% complete)'))
        }
        
        for (j in seq(nresample)){
            
            if(!is.null(perm.positions) & !is.null(partitions)){
                df.scramble <- df0
                df.scramble[,parameters$outcome] <- df0[perm.positions[[i]],parameters$outcome]
                partition.list <- partitions[[j]]
            } else {
                df.scramble <- fx_scramble(df0,parameters$outcome)
                partition.list <- fx_partition(df.scramble, type = parameters$sample.type)
            }
            
            modelObjPerm <- parallel::mclapply(seq(length(partition.list)), function(j){
                fx_model(fx_sample(df.scramble,partition.list[[j]]),
                         covar = parameters$covar,
                         voi = parameters$voi,
                         outcome = parameters$outcome,
                         model.type = parameters$model.type)},
                mc.cores = n.cores)
            
        }
        
        modelPerfObjPerm <- fx_modelPerf(modelObjPerm, 
                                         dthresh = parameters$dthresh)
        
        modelPerfObjPerm$df.allfolds <- NULL
        modelPerfObjPerm$parameters <- NULL
        
        permPerfObj[[i]] <- modelPerfObjPerm
        
    }
    
    
    # permPerfObj <- lapply(seq(nperm), function(i){
    #     if (i%in%updateMarks){
    #         writeLines(paste0('\tPermutation: ', i, ' (', (i/nperm)*100, '% complete)'))
    #     }
    #     if(!is.null(perm.positions) & !is.null(partitions)){
    #         df.scramble <- df0
    #         df.scramble[,parameters$outcome] <- df0[perm.positions[[i]],parameters$outcome]
    #         partition.list <- partitions[[i]]
    #     } else {
    #         df.scramble <- fx_scramble(df0,parameters$outcome)
    #         partition.list <- fx_partition(df.scramble, type = parameters$sample.type)
    #     }
    #     
    #     modelObjPerm <- parallel::mclapply(seq(length(partition.list)), function(j){
    #         fx_model(fx_sample(df.scramble,partition.list[[j]]),
    #                  covar = parameters$covar,
    #                  voi = parameters$voi,
    #                  outcome = parameters$outcome,
    #                  model.type = parameters$model.type)},
    #         mc.cores = n.cores)
    #     modelPerfObjPerm <- fx_modelPerf(modelObjPerm, 
    #                                      dthresh = parameters$dthresh)
    #     
    #     modelPerfObjPerm$df.allfolds <- NULL
    #     modelPerfObjPerm$parameters <- NULL
    #     return(modelPerfObjPerm)
    # })
    
    writeLines('Permutation testing complete!')
    return(list(permPerfObj=permPerfObj,
                parameters=parameters))
}


## fx_perm <- function(df0, modelPerfObj, modelObj, partitionList, nperm = 10, n.cores = 20){
    
##     permPerfObj <- list()
##     updateMarks <- seq(from = 0, to = nperm, length.out = 11)
##     writeLines('Deriving permutation results...')
    
##     parameters <- modelObj[[1]]$parameters
##     if(is.numeric(parameters$sample.type)){
##         parameters$nresample <- partitionList[[1]]$nresample
##         parameters$balance.col <- partitionList[[1]]$balance.col
##     }
##     decisionThreshold <- modelPerfObj$parameters$decisionThreshold
    
##     for (i in seq(nperm)){
##         if (i%in%updateMarks){
##             writeLines(paste0('\tPermutation: ', i, ' (', (i/nperm)*100, '% complete)'))
##         }
        
##         df.scramble <- fx_scramble(df0,parameters$outcome)
##         if(is.numeric(parameters$sample.type)){
##             partitionList <- fx_partition(df.scramble, type = parameters$sample.type,
##                                           nresample = parameters$nresample, 
##                                           balance.col = parameters$balance.col)
##         } else {
##             partitionList <- fx_partition(df.scramble, type = parameters$sample.type)
##         }
                
##         modelObjPerm <- parallel::mclapply(seq(length(partitionList)), function(j){
##             fx_model(fx_sample(df.scramble,partitionList[[j]]),
##                      covar = parameters$covar,
##                      voi = parameters$voi,
##                      outcome = parameters$outcome,
##                      model.type = parameters$model.type)},
##             mc.cores = n.cores)
##         modelPerfObjPerm <- fx_modelPerf(modelObjPerm, 
##                                          decisionThreshold = decisionThreshold)
        
##         modelPerfObjPerm$df.allfolds <- NULL
##         modelPerfObjPerm$parameters <- NULL
##         permPerfObj[[i]] <- modelPerfObjPerm
##     }

##     writeLines('Permutation testing complete!')
##     return(permPerfObj)
## }

