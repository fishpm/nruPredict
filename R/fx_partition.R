### Partitions datasets for CV

fx_partition <- function(df0, type = NULL, nresample = NULL, balance.col = NULL){
    
    nsamples <- nrow(df0)
    n.sequence <- seq(nsamples)
    
    if (!(type %in% c('loocv', 'ltocv') | is.numeric(type) | grepl('(-fold)$', type))){
        
        stop('Invalid CV type.')
        
    } else if (type %in% c('loocv', 'ltocv')){
        
        if (type == 'loocv'){n.out <- 1} else {n.out <- 2}
        nout.comb <- combn(nsamples, n.out)
        test.sets <- split(nout.comb, col(nout.comb))
        train.sets <- lapply(seq(length(test.sets)), function(i){
            n.sequence[!(seq(nsamples) %in% test.sets[[i]])]
        })
        partition.list <- lapply(seq(length(test.sets)), function(i){
            list('train' = train.sets[[i]], 'test' = test.sets[[i]], 'sample.type' = type)
        })
        
    }
    
    else if (grepl('(-fold)$', type)){
        
        n.folds <- as.numeric(unlist(strsplit(type, '-fold')))
        if (is.na(n.folds)){stop(paste0('Bad n-fold: ', type))}
        
        if (!is.null(balance.col)){
            if(!is.factor(df0[,balance.col])){
                stop(paste0(balance.col, ' needs to be a factor'))
            }
            if(nlevels(df0[,balance.col])!=2){
                stop(paste0(balance.col, 
                            ' should have 2 levels (', 
                            nlevels(df0[,balance.col]), ' found).'))
            }
            
            l1 <- levels(df0[,balance.col])[1]
            l2 <- levels(df0[,balance.col])[2]
            l1.elem <- which(df0[,balance.col]==l1)
            l2.elem <- which(df0[,balance.col]==l2)
            l1.testSet <- l1.elem
            l2.testSet <- l2.elem
            n1 <- sum(df0[,balance.col]==l1)
            n2 <- sum(df0[,balance.col]==l2)
            n1.floor <- floor(n1/n.folds)
            n2.floor <- floor(n2/n.folds)
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
                if (i <= n1 %% n.folds){
                    l1.test[[i]] <- sample(l1.testSet, n1.floor+1)
                    l1.testSet <- l1.testSet[-which(l1.testSet%in%l1.test[[i]])]
                } else {
                    l1.test[[i]] <- sample(l1.testSet, n1.floor)
                    l1.testSet <- l1.testSet[-which(l1.testSet %in% l1.test[[i]])]
                }
                l1.train[[i]] <- l1.elem[-which(l1.elem%in%l1.test[[i]])]
                
                if (i <= n2 %% n.folds){
                    l2.test[[i]] <- sample(l2.testSet, n2.floor+1)
                    l2.testSet <- l2.testSet[-which(l2.testSet %in% l2.test[[i]])]
                } else {
                    l2.test[[i]] <- sample(l2.testSet, n2.floor)
                    l2.testSet <- l2.testSet[-which(l2.testSet %in% l2.test[[i]])]
                }
                l2.train[[i]] <- l2.elem[-which(l2.elem%in%l2.test[[i]])]
                partition.list[[i]] <- list('train' = sort(c(l1.train[[i]],l2.train[[i]])), 
                                            'test' = sort(c(l1.test[[i]],l2.test[[i]])), 
                                            'sample.type' = type)
            }
            
        } else {
            nout.comb <- n.sequence
            
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
                if (i <= nsamples %% n.folds){
                    test[[i]] <- sample(nout.comb, group.size.floor+1)
                    nout.comb <- nout.comb[-which(nout.comb %in% test[[i]])]
                } else {
                    test[[i]] <- sample(nout.comb, group.size.floor)
                    nout.comb <- nout.comb[-which(nout.comb %in% test[[i]])]
                }
                train[[i]] <- n.sequence[-test[[i]]]
                partition.list[[i]] <- list('train' = train[[i]], 'test' = test[[i]], 'sample.type' = type)
            }
            
        }
        
        
    } else if (is.numeric(type)){
        
        if (is.null(nresample)){
            stop(paste0('If type is numeric, resample must be defined.'))
        }
        
        if (is.null(balance.col)){
            stop(paste0('If type is numeric, balance.col must be defined.'))
        }
        
        groups <- levels(df0[,balance.col])
        groups.size <- table(df0[,balance.col])
        
        if (length(groups)>2){
            stop(paste0('Only 2 group levels allowed. Group levels identified: ', length(groups.size)))
        }
        
        if (type >= min(groups.size)){
            stop(paste0('Specified group size (', type, ') >= smallest group size (', min(groups.size), ').'))
        }
        
        partition.list <- lapply(seq(nresample), function(i){
            g1 <- which(df0[,balance.col] == groups[1])
            g1_train.sets <- sample(g1, type)
            g1_test.sets <- g1[!(g1 %in% g1_train.sets)]
            g2 <- which(df0[,balance.col] == groups[2])
            g2_train.sets <- sample(g2, type)
            g2_test.sets <- g2[!(g2 %in% g2_train.sets)]
            train.sets <- c(g1_train.sets, g2_train.sets)
            test.sets <- c(g1_test.sets, g2_test.sets)
            list('train' = train.sets, 'test' = test.sets, 'sample.type' = type, 'nresample' = nresample, 'balance.col' = balance.col)
        })
        
    }
    
    else {
        
        stop(paste0('Unexpected input: ', type))
        
    }
    
    return(partition.list)
}