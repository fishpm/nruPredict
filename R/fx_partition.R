## * fx_partition (documentation)
##' @title List of Partitions
##' @description List of partitions to apply to data frame for CV
##'
##' @param df0 data frame including all observations
##' @param type cross-validation type
##' @param nresaple ???
##' @param balance.col ???
##'
##' @return list of same length as subsamples of train/test partitions.
##' Each list element contains "test", "train" and "sample.type" variables:
##' \itemize{
##' \item "test": is numeric list, values correspond to rows in df0 assigned to model "test" group
##' \item "train": is numeric list, values correspond to rows in df0 assigned to model "train" group
##' \item "sample.type": description of cross-validation method (e.g., 5-fold)
##' }

## * fx_partition (code)
##' @export
fx_partition <- function(df0, type = NULL, nresample = NULL, balance.col = NULL){
    
    nsamples <- nrow(df0)
    n.sequence <- seq(nsamples)
    
    # stop if not allowed "type" is specified
    if (!(type %in% c('loocv', 'ltocv') | is.numeric(type) | grepl('(-fold)$', type))){
        stop('Invalid CV type.')
    
    # handle loocv and ltocv types
    } else if (type %in% c('loocv', 'ltocv')){
        
        if (type == 'loocv'){n.out <- 1} else {n.out <- 2}
        elem <- combn(nsamples, n.out)
        test.sets <- split(elem, col(elem))
        train.sets <- lapply(seq(length(test.sets)), function(i){
            n.sequence[!(seq(nsamples) %in% test.sets[[i]])]
        })
        partition.list <- lapply(seq(length(test.sets)), function(i){
            list('train' = train.sets[[i]], 'test' = test.sets[[i]], 'sample.type' = type)
        })
        
    }
    
    # handle n-fold cv type
    else if (grepl('(-fold)$', type)){
        
        # check that "n" is properly specified
        n.folds <- as.numeric(unlist(strsplit(type, '-fold')))
        if (is.na(n.folds)){stop(paste0('Bad n-fold: ', type))}
        
        # balance.col specified
        if (!is.null(balance.col)){
            
            # balance.col column must be of type factor with exactly two levels
            if(!is.factor(df0[,balance.col])){
                stop(paste0(balance.col, ' needs to be a factor'))
            }
            if(nlevels(df0[,balance.col])!=2){
                stop(paste0(balance.col, 
                            ' should have 2 levels (', 
                            nlevels(df0[,balance.col]), ' found).'))
            }
            
            # level names
            l1 <- levels(df0[,balance.col])[1]
            l2 <- levels(df0[,balance.col])[2]
            
            # rows indexes that belong to l1 and l2
            l1.elem <- which(df0[,balance.col]==l1)
            l2.elem <- which(df0[,balance.col]==l2)
            
            # rows available for assignment to test group from l1 and l2
            l1.testSet <- l1.elem
            l2.testSet <- l2.elem
            
            # total indexes in l1 and l2
            n1 <- sum(df0[,balance.col]==l1)
            n2 <- sum(df0[,balance.col]==l2)
            
            # min number of elements to be assigned to a test group
            n1.floor <- floor(n1/n.folds)
            n2.floor <- floor(n2/n.folds)
            
            # error if folds >= rows in l1 or l2
            if(0%in%c(n1.floor,n2.floor)){
                stop(paste0('Too many folds (', n.folds, 
                            '), too little data (n1.floor = ', n1.floor, 
                            '; n2.floor = ', n2.floor, ').'))
            }
            
            partition.list <- list()
            l1.test <- list()
            l1.train <- list()
            l2.test <- list()
            l2.train <- list()
            
            for(i in seq(n.folds)){
                # folds with "extra" row in test fold determined my modulo
                
                # group 1
                if (i <= n1 %% n.folds){
                    l1.test[[i]] <- sample(l1.testSet, n1.floor+1)
                    # update l1 elements available for future selection
                    l1.testSet <- l1.testSet[-which(l1.testSet%in%l1.test[[i]])]
                } else {
                    l1.test[[i]] <- sample(l1.testSet, n1.floor)
                    # update l1 elements available for future selection
                    l1.testSet <- l1.testSet[-which(l1.testSet %in% l1.test[[i]])]
                }
                
                # l1 rows assigned to ith train fold are those from l1.elem not in l1.test[[i]]
                l1.train[[i]] <- l1.elem[-which(l1.elem%in%l1.test[[i]])]
                
                # group 2
                if (i <= n2 %% n.folds){
                    l2.test[[i]] <- sample(l2.testSet, n2.floor+1)
                    # update l2 elements available for future selection
                    l2.testSet <- l2.testSet[-which(l2.testSet %in% l2.test[[i]])]
                } else {
                    l2.test[[i]] <- sample(l2.testSet, n2.floor)
                    # update l2 elements available for future selection
                    l2.testSet <- l2.testSet[-which(l2.testSet %in% l2.test[[i]])]
                }
                
                # l2 rows assigned to ith train fold are those from l2.elem not in l2.test[[i]]
                l2.train[[i]] <- l2.elem[-which(l2.elem%in%l2.test[[i]])]
                
                # update partition.list
                partition.list[[i]] <- list('train' = sort(c(l1.train[[i]],l2.train[[i]])), 
                                            'test' = sort(c(l1.test[[i]],l2.test[[i]])), 
                                            'sample.type' = type)
            }
        
        # balance.col not specified
        } else {
            
            # testSet tracks rows available for assignment to test group
            testSet <- n.sequence
            
            # error if folds >= rows
            group.size.floor <- floor(nsamples/n.folds)
            if (group.size.floor == 0){
                stop(paste0('Too many folds (', 
                            n.folds, '), too little data (', 
                            nsamples, ').'))
            }
            
            partition.list <- list()
            test <- list()
            train <- list()
            
            for(i in seq(n.folds)){
                # folds with "extra" row in test fold determined my modulo
                
                if (i <= nsamples %% n.folds){
                    test[[i]] <- sample(testSet, group.size.floor+1)
                    # update elements available for future selection
                    testSet <- testSet[-which(testSet %in% test[[i]])]
                } else {
                    test[[i]] <- sample(testSet, group.size.floor)
                    # update elements available for future selection
                    testSet <- testSet[-which(testSet %in% test[[i]])]
                }
                
                # rows assigned to ith train fold are those not in test[[i]]
                train[[i]] <- n.sequence[-test[[i]]]
                
                # update partition.list
                partition.list[[i]] <- list('train' = train[[i]], 'test' = test[[i]], 'sample.type' = type)
            }
        }
        
    } else if (is.numeric(type)){
        
        stop('Not currently available.')
        # if (is.null(nresample)){
        #     stop(paste0('If type is numeric, resample must be defined.'))
        # }
        # 
        # if (is.null(balance.col)){
        #     stop(paste0('If type is numeric, balance.col must be defined.'))
        # }
        # 
        # groups <- levels(df0[,balance.col])
        # groups.size <- table(df0[,balance.col])
        # 
        # if (length(groups)>2){
        #     stop(paste0('Only 2 group levels allowed. Group levels identified: ', length(groups.size)))
        # }
        # 
        # if (type >= min(groups.size)){
        #     stop(paste0('Specified group size (', type, ') >= smallest group size (', min(groups.size), ').'))
        # }
        # 
        # partition.list <- lapply(seq(nresample), function(i){
        #     g1 <- which(df0[,balance.col] == groups[1])
        #     g1_train.sets <- sample(g1, type)
        #     g1_test.sets <- g1[!(g1 %in% g1_train.sets)]
        #     g2 <- which(df0[,balance.col] == groups[2])
        #     g2_train.sets <- sample(g2, type)
        #     g2_test.sets <- g2[!(g2 %in% g2_train.sets)]
        #     train.sets <- c(g1_train.sets, g2_train.sets)
        #     test.sets <- c(g1_test.sets, g2_test.sets)
        #     list('train' = train.sets, 'test' = test.sets, 'sample.type' = type, 'nresample' = nresample, 'balance.col' = balance.col)
        # })
    }
    else {
        
        stop(paste0('Unexpected input: ', type))
        
    }
    
    return(partition.list)
}


## fx_partition <- function(df0, type = 'loocv', nresample = NULL, balance.col = NULL){
    
##     nSamples <- nrow(df0)
##     nSequence <- seq(nSamples)
    
##     if (!(type %in% c('loocv', 'ltocv') | is.numeric(type) | grepl('(-fold)$', type))){
##         stop('Invalid CV type.')
##     } else if (type == 'loocv' | type == 'ltocv'){
##         if (type == 'loocv'){nOut <- 1} else {nOut <- 2}
##         tmp <- combn(nSamples, nOut)
##         testSets <- split(tmp, col(tmp))
##         trainSets <- lapply(seq(length(testSets)), function(i){
##             nSequence[!(seq(nSamples) %in% testSets[[i]])]
##         })
##         partitionList <- lapply(seq(length(testSets)), function(i){
##             list('train' = trainSets[[i]], 'test' = testSets[[i]], 'sample.type' = type)
##         })
##         return(partitionList)
##     }
##     else if (grepl('(-fold)$', type)){
##         nFolds <- as.numeric(unlist(strsplit(type, '-fold')))
##         if (is.na(nFolds)){stop(paste0('Bad n-fold: ', type))}
##         tmp <- nSequence
##         gSizeFloor <- floor(nSamples/nFolds)
##         if (gSizeFloor == 0){
##             stop(paste0('Too many folds (', nFolds, '), too little data (', nSamples, ').'))
##         }
##         partitionList <- list()
##         for(i in seq(nFolds)){
##             if (i <= nSamples %% nFolds){
##                test <- sample(tmp, gSizeFloor+1)
##                tmp <- tmp[-which(tmp %in% test)]
##                }
##             else {
##                 test <- sample(tmp, gSizeFloor)
##                 tmp <- tmp[-which(tmp %in% test)]
##                 }
##             train <- nSequence[-test]
##             partitionList[[i]] <- list('train' = train, 'test' = test, 'sample.type' = type)
##         }
##         return(partitionList)
##     }
##     else if (is.numeric(type)){
##         if (is.null(nresample)){stop(paste0('If type is numeric, resample must be defined.'))}
##         if (is.null(balance.col)){stop(paste0('If type is numeric, balance.col must be defined.'))}
##         groups <- levels(df0[,balance.col])
##         nGroups <- table(df0[,balance.col])
##         if (length(groups)>2){stop(paste0('Only 2 group levels allowed. Group levels identified: ', length(nGroups)))}
##         if (type >= min(nGroups)){stop(paste0('Specified group size (', type, ') >= smallest group size (', min(nGroups), ').'))}

##         partitionList <- lapply(seq(nresample), function(i){
##             g1 <- which(df0[,balance.col] == groups[1])
##             g1_trainSets <- sample(g1, type)
##             g1_testSets <- g1[!(g1 %in% g1_trainSets)]
##             g2 <- which(df0[,balance.col] == groups[2])
##             g2_trainSets <- sample(g2, type)
##             g2_testSets <- g2[!(g2 %in% g2_trainSets)]
##             trainSets <- c(g1_trainSets, g2_trainSets)
##             testSets <- c(g1_testSets, g2_testSets)
##             list('train' = trainSets, 'test' = testSets, 'sample.type' = type, 'nresample' = nresample, 'balance.col' = balance.col)
##         })
##         return(partitionList)
##     }
##     else {stop(paste0('Unexpected input: ', type))}
##     return(partitionList)
## }
