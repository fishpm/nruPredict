## * fx_partition2 (documentation)
##' @title Extract CV Fold Assignment
##' @description Extract CV fold assignment from a modelObj
##'
##' @param modelObj model object, containing observed model performances
##'
##' @return update
##' \itemize{
##' \item all.partitions: list of length nresamples (as defined within model Obj input). Each element contains list of length nfolds (i.e., number of folds within each resample). Each element of this sublist is a list containing three variables.
##' \item train: row indices to be assigned to training dataset
##' \item test: row indices to be assigned to testing dataset
##' \item sample.type: cross-validation sampling type (e.g, 5-fold)
##' }

## * fx_partition2 (code)
##' @export
fx_partition2 <- function(modelObj = NULL){
    
    if(is.null(modelObj)){
        stop('modelObj must be specified.')
    }
    
    nresamples <- modelObj$parameters$nresample
    sample.type <- modelObj$parameters$sample.type
    if (suppressWarnings(is.na(as.numeric(unlist(strsplit(sample.type, '-'))[1])))){
        stop('Only k-fold CV currently supported.')
    } else {
        nfolds <- as.numeric(unlist(strsplit(sample.type, '-'))[1])
    }
    
    writeLines(paste0('Identified model structure: ', sample.type, ' with ', nresamples, ' resamples.'))
    
    all.partitions <- list()
    for (i in seq(nresamples)){
        all.partitions[[i]] <- list()
        train <- list()
        test <- list()
        for(j in seq(nfolds)){
            unique.orig.df.row <- modelObj$modelResamplePerfObj[[i]]$df.allfolds$orig.df.row
            fold.elem <- modelObj$modelResamplePerfObj[[i]]$df.allfolds$fold==j
            test.orig.df.row <- modelObj$modelResamplePerfObj[[i]]$df.allfolds$orig.df.row[fold.elem]
            train.orig.df.row <- unique.orig.df.row[!unique.orig.df.row%in%test.orig.df.row]
            train[[j]] <- sort(train.orig.df.row)
            test[[j]] <- sort(test.orig.df.row)
            all.partitions[[i]][[j]] <- list('train' = train[[j]], 'test' = test[[j]], 'sample.type' = sample.type)
        }
    }
    
    return(all.partitions)
}
