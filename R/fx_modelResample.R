### Resample model multiple times

fx_modelResample <- function(df0, cv.type = NULL, covar = NULL, voi = NULL, outcome = NULL, model.type = NULL, nresample = 1, dthresh = 0.5, z.pred = F, n.cores = 20, balance.col = NULL){
    
    updateMarks <- seq(from = 0, to = nresample, length.out = 11)
    
    if(cv.type == 'loocv'){
        nresample <- 1
        writeLines('LOOCV - resetting nresamples to 1...')
    } else {
        writeLines('Generating resample results...')
    }
    
    modelResamplePerfObj <- lapply(seq(nresample), function(j){
        
        if (j%in%updateMarks){
            writeLines(paste0('\tResample: ', j, ' (', (j/nresample)*100, '% complete)'))
        }
        
        partition.list <- fx_partition(df0, type = cv.type, balance.col = balance.col)
        modelObj <- mclapply(seq(length(partition.list)), function(i){
            fx_model(fx_sample(df0,partition.list[[i]]), 
                     covar = covar, 
                     voi = voi, 
                     outcome = outcome, 
                     model.type = model.type, 
                     z.pred = z.pred)}, 
            mc.cores = n.cores)
        modelPerfObj <- fx_modelPerf(modelObj, dthresh=dthresh)
        
        # modelPerfObj$df.allfolds <- NULL
        modelPerfObj$parameters <- NULL
        return(modelPerfObj)
        
    })
    
    partition.list <- fx_partition(df0, type = cv.type, balance.col = balance.col)
    modelObj <- mclapply(seq(length(partition.list)), function(i){
        fx_model(fx_sample(df0,partition.list[[i]]), 
                 covar = covar, 
                 voi = voi, 
                 outcome = outcome, 
                 model.type = model.type)}, 
        mc.cores = n.cores)
    modelPerfObj <- fx_modelPerf(modelObj, dthresh=dthresh)
    parameters <- modelPerfObj$parameters
    parameters$z.pred <- z.pred
    parameters$nresample <- nresample

    writeLines('Model fitting completed!')
    return(list(modelResamplePerfObj = modelResamplePerfObj,
                parameters = parameters))
}