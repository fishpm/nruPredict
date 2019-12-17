require('mvtnorm')
require('parallel')
require('pROC')
require('lava')
require('scales')
require('ggplot2')
library('randomForest')
library('e1071')
library('zoo')

####
## DEFINE FUNCTIONS
####

fx_scramble <- function(df, outcome){
    if(is.null(outcome)){
        stop('Specify variable for scrambling')
    } else {
        if(!outcome%in%colnames(df)){
            stop('Specified outcome variable (', outcome, ') not in data.frame')
        }
    }
    
    df[,outcome] <- sample(df[,outcome])
    return(df)
}

fx_partition <- function(df, type = 'loocv', nresample = NULL, balance.col = NULL){
    
    nSamples <- nrow(df)
    nSequence <- seq(nSamples)
    
    if (!(type %in% c('loocv', 'ltocv') | is.numeric(type) | grepl('(-fold)$', type))){
        stop('Invalid CV type.')
    } else if (type == 'loocv' | type == 'ltocv'){
        if (type == 'loocv'){nOut <- 1} else {nOut <- 2}
        tmp <- combn(nSamples, nOut)
        testSets <- split(tmp, col(tmp))
        trainSets <- lapply(seq(length(testSets)), function(i){
            nSequence[!(seq(nSamples) %in% testSets[[i]])]
        })
        partitionList <- lapply(seq(length(testSets)), function(i){
            list('train' = trainSets[[i]], 'test' = testSets[[i]], 'sample.type' = type)
        })
        return(partitionList)
    }
    else if (grepl('(-fold)$', type)){
        nFolds <- as.numeric(unlist(strsplit(type, '-fold')))
        if (is.na(nFolds)){stop(paste0('Bad n-fold: ', type))}
        tmp <- nSequence
        gSizeFloor <- floor(nSamples/nFolds)
        if (gSizeFloor == 0){
            stop(paste0('Too many folds (', nFolds, '), too little data (', nSamples, ').'))
        }
        partitionList <- list()
        for(i in seq(nFolds)){
            if (i <= nSamples %% nFolds){
               test <- sample(tmp, gSizeFloor+1)
               tmp <- tmp[-which(tmp %in% test)]
               }
            else {
                test <- sample(tmp, gSizeFloor)
                tmp <- tmp[-which(tmp %in% test)]
                }
            train <- nSequence[-test]
            partitionList[[i]] <- list('train' = train, 'test' = test, 'sample.type' = type)
        }
        return(partitionList)
    }
    else if (is.numeric(type)){
        if (is.null(nresample)){stop(paste0('If type is numeric, resample must be defined.'))}
        if (is.null(balance.col)){stop(paste0('If type is numeric, balance.col must be defined.'))}
        groups <- levels(df[,balance.col])
        nGroups <- table(df[,balance.col])
        if (length(groups)>2){stop(paste0('Only 2 group levels allowed. Group levels identified: ', length(nGroups)))}
        if (type >= min(nGroups)){stop(paste0('Specified group size (', type, ') >= smallest group size (', min(nGroups), ').'))}

        partitionList <- lapply(seq(nresample), function(i){
            g1 <- which(df[,balance.col] == groups[1])
            g1_trainSets <- sample(g1, type)
            g1_testSets <- g1[!(g1 %in% g1_trainSets)]
            g2 <- which(df[,balance.col] == groups[2])
            g2_trainSets <- sample(g2, type)
            g2_testSets <- g2[!(g2 %in% g2_trainSets)]
            trainSets <- c(g1_trainSets, g2_trainSets)
            testSets <- c(g1_testSets, g2_testSets)
            list('train' = trainSets, 'test' = testSets, 'sample.type' = type, 'nresample' = nresample, 'balance.col' = balance.col)
        })
        return(partitionList)
    }
    else {stop(paste0('Unexpected input: ', type))}
    return(partitionList)
}

fx_sample <- function(df, partition){
    df.train <- df[partition$train,]
    df.test <- df[partition$test,]
    parameters <- list(sample.type = partition$sample.type)
    parameters$train.rows <- partition$train
    parameters$test.rows <- partition$test
    parameters$data.frame <- as.character(match.call()$df)
    if (is.numeric(parameters$sample.type)){
        parameters$nresample <- partition$nresample
        parameters$balance.col <- partition$balance.col
        }
    return(list(df.train = df.train, 
                df.test = df.test, 
                parameters = parameters))
}

fx_model <- function(df.set, predvar = NULL, outcome = NULL, model.type = 'logistic', z.pred = T){
    
    model.set <- c('logistic', 'rf', 'svm')
    if(!tolower(model.type) %in% model.set){
        stop(paste0('Specify appropriate model type. Choose from: ', paste0(model.set, collapse = ', ')))
    } else {model.type <- tolower(model.type)}
    
    model.formula <- as.formula(paste0(
        outcome, ' ~ ', paste(predvar, collapse = '+')))
    
    class.levels <- levels(df.set$df.train[,outcome])
    parameters <- list(sample.type = df.set$parameters$sample.type, 
                       train.rows = df.set$parameters$train.rows, 
                       test.rows = df.set$parameters$test.rows, 
                       class.levels = class.levels,
                       model.type = model.type, 
                       predvar = predvar, 
                       outcome = outcome, 
                       model.formula = model.formula,
                       data.frame = df.set$parameters$data.frame
                       )
    
    # standardize non-factor predictor variables
    if(z.pred){
        pred.continuous <- predvar[sapply(predvar, function(i){!is.factor(df.set$df.train[,i])})]
        df.train <- df.set$df.train
        df.test <- df.set$df.test
        for(i in pred.continuous){
            elem.mean <- mean(df.set$df.train[,i])
            elem.sd <- sd(df.set$df.train[,i])
            df.train[,i] <- (df.set$df.train[,i]-elem.mean)/elem.sd
            df.test[,i] <- (df.set$df.test[,i]-elem.mean)/elem.sd
        }
    } else {
        df.train <- df.set$df.train
        df.test <- df.set$df.test
        }
    
    # Apply model
    if (model.type == 'logistic'){
        model.object <- glm(model.formula, data = df.train, family = 'binomial')
    } else if (model.type == 'rf'){
        model.object <- randomForest(model.formula, data = df.train)
    } else if (model.type == 'svm'){
        model.object <- svm(model.formula, data = df.train)
    }
    
    # Use model to predict test datasets
    if (model.type == 'logistic'){
        pred.class <- rep(NA, nrow(df.test))
        pred.prob <- predict(model.object, newdata = df.test, type = 'resp')
    } else if (model.type == 'rf'){
        lev <- levels(df.train[,outcome])
        pred.class <- rep(NA, nrow(df.test))
        pred.prob <- predict(model.object, newdata = df.test, type = 'prob')[,parameters$class.levels[2]]
    } else if (model.type == 'svm'){
        pred.class <- as.character(predict(model.object, newdata = df.test))
        pred.prob <- rep(NA, nrow(df.test))
    }
    
    actual.class <- as.character(df.set$df.test[,outcome])
    if (is.numeric(parameters$sample.type)){
        parameters$nresample <- df.set$parameters$nresample
        parameters$balance.col <- df.set$parameters$balance.col
    }
    return(list(pred.prob = pred.prob, pred.class = pred.class, actual.class = actual.class, parameters = parameters))
}

fx_modelPerf <- function(modelOutput, decisionThreshold = 0.5, many = T, perm = F, computeAUC = 'across'){

    if (!many) {
        
        parameters <- modelOutput$parameters
        actual.class <- modelOutput$actual.class
        pred.prob <- modelOutput$pred.prob
        class.levels <- modelOutput$parameters$class.levels
        
        if(!parameters$model.type=='svm'){
            pred.class <- class.levels[(pred.prob > decisionThreshold)+1]
        } else {
            pred.class <- modelOutput$pred.class
            decisionThreshold <- NA
        }
        
    } else {
        modelOutput <- modelObj # delete me!
        nmodels <- length(modelOutput)
        params.set <- !(names(modelOutput[[1]]$parameters)=='train.rows'|
            names(modelOutput[[1]]$parameters)=='test.rows')
        parameters <- modelOutput[[1]][['parameters']][params.set]
        class.levels <- parameters$class.levels
        
        nrows.df <- length(modelOutput[[1]]$parameters$train.rows)+
            length(modelOutput[[1]]$parameters$test.rows)
        
        df.perf <- do.call(rbind, lapply(
            lapply(seq(nmodels), function(j){
                return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
                            fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
                            actual.class = modelOutput[[j]]$actual.class))
                       }),
            data.frame))
        rownames(df.perf) <- seq(nrow(df.perf))
        
        # each observation in test set only once
        if (all(table(df.perf$orig.df.row)==1)){
            df.perf$pred.prob <- sapply(seq(nrow(df.perf)), function(i){
                list.elem <- df.perf$fold[i]
                pred.elem <- modelOutput[[df.perf$fold[i]]]$parameters$test.rows==df.perf$orig.df.row[i]
                modelOutput[[list.elem]]$pred.prob[pred.elem]
            })
            df.perf$pred.class <- sapply(seq(nrow(df.perf)), function(i){
                list.elem <- df.perf$fold[i]
                pred.elem <- modelOutput[[df.perf$fold[i]]]$parameters$test.rows==df.perf$orig.df.row[i]
                modelOutput[[list.elem]]$pred.class[pred.elem]
            })
            if(all(is.na(df.perf$pred.class))){
                df.perf$pred.class <- parameters$class.levels[(df.perf$pred.prob > decisionThreshold)+1]
            }
            
        # observations in multiple test sets
        } else {
            df.perf$pred.prob <- sapply(df.perf$orig.df.row, function(i){
                pred.prob.set <- unlist(sapply(seq(nmodels), function(j){
                    j.testrows <- modelOutput[[j]]$parameters$test.rows
                    if(i%in%j.testrows){
                        return(modelOutput[[j]]$pred.prob[j.testrows==i])
                    }
                }))
                return(median(pred.prob.set)) # MEDIAN!!
            })
            
            df.perf$pred.class <- sapply(df.perf$orig.df.row,function(i){
                pred.class.set <- unlist(sapply(seq(nmodels),function(j){
                    j.testrows <- modelOutput[[j]]$parameters$test.rows
                    if(i%in%j.testrows){
                        return(modelOutput[[j]]$pred.class[j.testrows==i])
                    }
                }))
                if (all(is.na(pred.class.set))){
                    pred.prob.set <- df.perf$pred.prob[df.perf$orig.df.row==i]
                    pred.class.set <- parameters$class.levels[(pred.prob.set > decisionThreshold)+1]
                }
                votesRanked <- sort(table(pred.class.set),decreasing = T)
                majorityClass <- names(votesRanked)[1]
                return(majorityClass) # MAJORITY RULES!
            })

        }
    }
    
    perf <- list()
    perf$df.perf <- df.perf
    perf$cMatrix <- list()
    perf$cMatrix_descrip <- 'Rows: prediction, Columns: actual'
    perf$negative.class <- class.levels[1]
    perf$positive.class <- class.levels[2]
    perf$decisionThreshold <- decisionThreshold
    
    # if(sample.type in specific group){}
    uniqueFolds <- 
    nFolds <- length(uniqueFolds)
    
    foldPerf <- lapply(c('all',unique(df.perf$fold)), function(i){

        if(i=='all'){
            firstrows <- sapply(unique(df.perf$orig.df.row), function(j){
                return(which(df.perf$orig.df.row==j)[1])
            })
            df.tmp <- df.perf[firstrows,]
        } else {
            df.tmp <- df.perf[df.perf$fold==i,]
        }
        
        cMatrix <- matrix(0,nrow=2,ncol=2,
                          dimnames=list(class.levels,
                                        class.levels))
        
        cMatrix[class.levels[1],class.levels[1]] <- 
            sum(df.tmp$pred.class==class.levels[1] & df.tmp$actual.class==class.levels[1])
        cMatrix[class.levels[2],class.levels[1]] <- 
            sum(df.tmp$pred.class==class.levels[2] & df.tmp$actual.class==class.levels[1])
        cMatrix[class.levels[1],class.levels[2]] <- 
            sum(df.tmp$pred.class==class.levels[1] & df.tmp$actual.class==class.levels[2])
        cMatrix[class.levels[2],class.levels[2]] <- 
            sum(df.tmp$pred.class==class.levels[2] & df.tmp$actual.class==class.levels[2])
        
        perfMetrics <- data.frame(TP = cMatrix[perf$positive.class, perf$positive.class],
                                  FP = cMatrix[perf$positive.class, perf$negative.class],
                                  TN = cMatrix[perf$negative.class, perf$negative.class],
                                  FN = cMatrix[perf$negative.class, perf$positive.class])
        if(!all(is.na(df.perf$pred.prob))){
            perfMetrics$auc.ROC <- fx_rocCompute(pred.prob = df.tmp$pred.prob, 
                                                 actual.class = df.tmp$actual.class, 
                                                 class.levels = class.levels)
        } else {
            perfMetrics$auc.ROC <- NA
        }
        
        perfMetrics$sensitivity <- perfMetrics$TP/(perfMetrics$TP + perfMetrics$FN)
        perfMetrics$specificity <- perfMetrics$TN/(perfMetrics$TN + perfMetrics$FP)
        perfMetrics$ppv <- perfMetrics$TP/(perfMetrics$TP + perfMetrics$FP)
        perfMetrics$npv <- perfMetrics$TN/(perfMetrics$TN + perfMetrics$FN)
        perfMetrics$accuracy <- (perfMetrics$TP + perfMetrics$TN)/sum(cMatrix)
        perfMetrics <- perfMetrics[,c('auc.ROC', 'accuracy', 'sensitivity', 'specificity', 'ppv', 'npv', 'TP', 'FP', 'TN', 'FN')]
        return(list(cMatrix = cMatrix, perfMetrics = perfMetrics, fold = i))
    })
    
    if(foldPerf[[1]]$fold=='all'){perf$cMatrix <- foldPerf[[1]]$cMatrix}
    
    perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold}))) 
    perfMetrics <- as.data.frame(cbind(perfMetrics, 
                                       do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
    perf$perfMetrics <- perfMetrics
    perf$parameters <- parameters
    
    return(perf)
}

fx_rocCompute <- function(pred.prob, actual.class, class.levels){
    
    pred.prob.sort <- pred.prob[order(pred.prob,decreasing=T)]
    actual.class.sort <- actual.class[order(pred.prob,decreasing=T)]
    
    # generate fpr and tpr values and plt
    roc.vals <- lapply(c(1,pred.prob.sort), function(i){
        tp <- sum(pred.prob.sort>=i&actual.class.sort==class.levels[2])
        fp <- sum(pred.prob.sort>=i&actual.class.sort==class.levels[1])
        fn <- sum(pred.prob.sort<i&actual.class.sort==class.levels[2])
        tn <- sum(pred.prob.sort<i&actual.class.sort==class.levels[1])
        fpr <- fp/(fp+tn) # false-positive rate
        tpr <- tp/(tp+fn) # true-positive rate
        return(c(fpr,tpr))
    })
    fpr <- unlist(roc.vals)[c(T,F)] # fpr are odd elements
    tpr <- unlist(roc.vals)[c(F,T)] # tpr are even elements
    id <- order(fpr)
    roc.auc <- sum(diff(fpr[id])*rollmean(tpr[id],2))
    return(roc.auc)
}

fx_cmatrix <- function(actual.class, pred.class, pred.prob = NULL, parameters = NULL){
    
    class.levels <- parameters$class.levels
    
    cMatrix <- matrix(0,nrow=2,ncol=2,
                      dimnames=list(class.levels,
                                    class.levels))
    
    cMatrix[class.levels[1],class.levels[1]] <- 
        sum(pred.class==class.levels[1] & actual.class==class.levels[1])
    cMatrix[class.levels[2],class.levels[1]] <- 
        sum(pred.class==class.levels[2] & actual.class==class.levels[1])
    cMatrix[class.levels[1],class.levels[2]] <- 
        sum(pred.class==class.levels[1] & actual.class==class.levels[2])
    cMatrix[class.levels[2],class.levels[2]] <- 
        sum(pred.class==class.levels[2] & actual.class==class.levels[2])
    
    if (!is.null(pred.prob)){
        rocObj <- roc(actual.class, pred.prob)
    }
    
    perf <- list()
    perf$actual.class <- actual.class
    perf$pred.class <- pred.class
    perf$pred.prob <- pred.prob
    perf$decisionThreshold <- decisionThreshold
    perf$cMatrix <- cMatrix
    perf$descrip <- 'Rows: prediction, Columns: actual'
    perf$negative <- class.levels[1]
    perf$positive <- class.levels[2]
    
    # true positive
    perf$TP <- cMatrix[perf$positive, perf$positive]
    # false positive
    perf$FP <- cMatrix[perf$positive, perf$negative]
    # true negative
    perf$TN <- cMatrix[perf$negative, perf$negative]
    # false negative
    perf$FN <- cMatrix[perf$negative, perf$positive]
    # sensitivity
    perf$sensitivity <- perf$TP/(perf$TP + perf$FN)
    # specificity
    perf$specificity <- perf$TN/(perf$TN + perf$FP)
    # positive predictive value
    perf$ppv <- perf$TP/(perf$TP + perf$FP)
    # negative predictive value
    perf$npv <- perf$TN/(perf$TN + perf$FN)
    # accuracy
    perf$accuracy <- (perf$TP + perf$TN)/sum(cMatrix)
    
    if(is.null(pred.prob)){
        perf$auc.ROC <- NA
        perf$auc.95CI.delong <- NA
        perf$auc.95CI.boot <- NA
    } else {
        perf$auc.ROC <- auc(rocObj)
        perf$auc.95CI.delong <- suppressWarnings(ci(rocObj, of = 'auc')) 
        if(auc.boot){
            perf$auc.95CI.boot <- suppressWarnings(ci(rocObj, of = 'auc', method = 'boot'))
        } else {
            perf$auc.95CI.boot <- NA
        }
    }
    
    perf$parameters <- parameters
    return(perf)
}

fx_perm <- function(df, modelPerfObj, modelObj, partitionList, nperm = 10, n.cores = 20){
    
    permPerfObj <- list()
    updateMarks <- seq(from = 0, to = nperm, length.out = 11)
    writeLines('Deriving permutation results...')
    
    parameters <- modelObj[[1]]$parameters
    if(is.numeric(parameters$sample.type)){
        parameters$nresample <- partitionList[[1]]$nresample
        parameters$balance.col <- partitionList[[1]]$balance.col
    }
    decisionThreshold <- modelPerfObj$decisionThreshold
    
    for (i in seq(nperm)){
        if (i%in%updateMarks){
            writeLines(paste0('\tPermutation: ', i, ' (', (i/nperm)*100, '% complete)'))
        }
        
        df.scramble <- fx_scramble(df,parameters$outcome)
        if(is.numeric(parameters$sample.type)){
            partitionList <- fx_partition(df.scramble, type = parameters$sample.type,
                                          nresample = parameters$nresample, 
                                          balance.col = parameters$balance.col)
        } else {
            partitionList <- fx_partition(df.scramble, type = parameters$sample.type)
        }
                
        modelObjPerm <- mclapply(seq(length(partitionList)), function(j){
            fx_model(fx_sample(df.scramble,partitionList[[j]]),
                     predvar = parameters$predvar,
                     outcome = parameters$outcome,
                     model.type = parameters$model.type)},
            mc.cores = n.cores)
        modelPerfObjPerm <- fx_modelPerf(modelObjPerm, decisionThreshold = decisionThreshold)
        permPerfObj[[i]] <- modelPerfObjPerm
    }

    writeLines('Permutation testing complete!')
    return(permPerfObj)
}

fx_perm2 <- function(df, modelPerfObj, modelObj, partitionList, nperm = 10, n.cores = 20){
    
    permPerfObj <- list()
    updateMarks <- seq(from = 0, to = nperm, length.out = 11)
    writeLines('Deriving permutation results...')
    
    parameters <- modelObj[[1]]$parameters
    if(is.numeric(parameters$sample.type)){
        parameters$nresample <- partitionList[[1]]$nresample
        parameters$balance.col <- partitionList[[1]]$balance.col
    }
    decisionThreshold <- modelPerfObj$overallPerf[[1]]$decisionThreshold
    
    for (i in seq(nperm)){
        if (i%in%updateMarks){
            writeLines(paste0('\tPermutation: ', i, ' (', (i/nperm)*100, '% complete)'))
        }
        
        df <- fx_scramble(get(parameters$data.frame),parameters$outcome)
        if(is.numeric(parameters$sample.type)){
            partitionList <- fx_partition(df, type = parameters$sample.type,
                                          nresample = parameters$nresample, 
                                          balance.col = parameters$balance.col)
        } else {
            partitionList <- fx_partition(df, type = parameters$sample.type)
        }
        
        modelObjPerm <- mclapply(seq(length(partitionList)), function(j){
            fx_model(fx_sample(df,partitionList[[j]]), 
                     predvar = parameters$predvar, 
                     outcome = parameters$outcome, 
                     model.type = parameters$model.type)}, 
            mc.cores = n.cores)
        modelPerfObjPerm <- fx_modelPerf2(modelObjPerm, decisionThreshold = decisionThreshold)
        permPerfObj[[i]] <- modelPerfObjPerm
    }
    
    writeLines('Permutation testing complete!')
    return(permPerfObj)
}

fx_permPerf <- function(modelPerfObj, permObj, measures = NULL, nkfcv = F){
    
    measuresSet <- c('accuracy', 'auc.ROC', 'sensitivity', 'specificity', 'ppv', 'npv')
    
    if(is.null(measures)){
        measures <- measuresSet
    } else {
        if(any(!measures %in% measuresSet)){
            stop(paste0('Unknown outcome measures: ', paste(measures[which(!measures%in%measuresSet)], collapse = ',')))
        }
    }
        
    nperm <- length(permObj)
    if (nkfcv){
        niter <- length(modelPerfObj)
        parameters <- modelPerfObj[[1]]$parameters
        parameters$nkfcv <- niter
    } else {
        parameters <- modelPerfObj$parameters
        parameters$nkfcv <- nkfcv
    }
    
    
    df.perm <- as.data.frame(sapply(measures, function(i){
        sapply(seq(nperm), function(j){permObj[[j]][[i]]})
    }))
    
    if(nkfcv){
        df.pval <- as.data.frame(
            sapply(measures, function(i){
                obs <- mean(sapply(seq(niter), function(j){
                    modelPerfObj[[j]][[i]]
                    }))
                pval <- (sum(df.perm[,i]>obs)+(sum(df.perm[,i]==obs)*0.5))/nperm
                c(obs,pval)
            }),
            row.names = c('obs','pval'))
    } else {
        df.pval <- as.data.frame(
            sapply(measures, function(i){
                obs <- modelPerfObj[[i]]
                pval <- (sum(df.perm[,i]>obs)+(sum(df.perm[,i]==obs)*0.5))/nperm
                c(obs,pval)
            }),
            row.names = c('obs','pval'))
    }
    
    return(list(df.perm=df.perm,
                df.pval=df.pval,
                parameters = parameters))
}

fx_permPerf2 <- function(modelPerfObj, permObj, measures = NULL, nkfcv = F){
    
    measuresSet <- c('accuracy', 'auc.ROC', 'sensitivity', 'specificity', 'ppv', 'npv')
    
    if(is.null(measures)){
        measures <- measuresSet
    } else {
        if(any(!measures %in% measuresSet)){
            stop(paste0('Unknown outcome measures: ', paste(measures[which(!measures%in%measuresSet)], collapse = ',')))
        }
    }
    
    nperm <- length(permObj)
    if (nkfcv){
        niter <- length(modelPerfObj)
        parameters <- modelPerfObj[[1]]$parameters
        parameters$nkfcv <- niter
    } else {
        # parameters <- modelPerfObj$parameters
        # parameters$nkfcv <- nkfcv
    }
    
    
    df.perm <- as.data.frame(sapply(measures, function(i){
        colMeans(sapply(seq(nperm), function(j){permObj[[j]]$summaryPerf[[i]]}))
    }))
    
    if(nkfcv){
        df.pval <- as.data.frame(
            sapply(measures, function(i){
                obs <- mean(sapply(seq(niter), function(j){
                    modelPerfObj[[j]][[i]]
                }))
                pval <- (sum(df.perm[,i]>obs)+(sum(df.perm[,i]==obs)*0.5))/nperm
                c(obs,pval)
            }),
            row.names = c('obs','pval'))
    } else {
        df.pval <- as.data.frame(
            sapply(measures, function(i){
                i <- measures[1]
                obs <- mean(modelPerfObj$summaryPerf[[i]])
                pval <- (sum(df.perm[,i]>obs)+(sum(df.perm[,i]==obs)*0.5))/nperm
                c(obs,pval)
            }),
            row.names = c('obs','pval'))
    }
    
    return(list(df.perm=df.perm,
                df.pval=df.pval,
                parameters = parameters))
}

fx_permPlot <- function(permPerfObj, measures = NULL, outFile = NULL){
    
    measuresSet <- c('accuracy', 'auc.ROC', 'sensitivity', 'specificity', 'ppv', 'npv')
    if (permPerfObj$parameters$model.type=='svm'){
        measuresSet <- measuresSet[!measuresSet=='auc.ROC']
    }
    
    if(is.null(measures)){
        measures <- measuresSet
    } else {
        if(any(!measures %in% measuresSet)){
            stop(paste0('Unknown outcome measures: ', paste(measures[which(!measures%in%measuresSet)], collapse = ',')))
        }
    }

    if (!is.null(outFile)){pdf(fx_outFile(outFile))}

    plots <- list()
    for (measure in measures){
        subtext <- paste0(measure, ' = ', signif(permPerfObj$df.pval['obs', measure],3), '; p-value = ', signif(permPerfObj$df.pval['pval', measure],3))
        
        if(permPerfObj$parameters$nkfcv){
            captext <- paste0('N perm = ', nrow(permPerfObj$df.perm), '; nkfcv = ', permPerfObj$parameters$nkfcv)
            perfValRange <- quantile(sapply(seq(length(mpo)), function(i){mpo[[i]]$accuracy}),probs = c(0.025, 0.975))
        } else if(is.numeric(permPerfObj$parameters$sample.type)){
            captext <- paste0('N perm = ', nrow(permPerfObj$df.perm), 
                              '; train group size = ', permPerfObj$parameters$sample.type,
                              '; nresamples = ', permPerfObj$parameters$nresample)
        }
        else {
            captext <- paste0('N perm = ', nrow(permPerfObj$df.perm))
        }
        
        plots[[length(plots)+1]] <-
            ggplot(data = permPerfObj$df.perm, aes_string(x=measure)) +
            geom_histogram(fill = 'darkblue') +
            geom_vline(data = permPerfObj$df.pval['obs',], aes_string(xintercept=measure),
                       color = 'darkorange', linetype = 'dashed', size = 2) +
            scale_x_continuous(limits=c(0,1)) +
            scale_y_continuous(name='Frequency') +
            labs(title = measure,
                 subtitle = subtext,
                 caption = captext) +
            theme(plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5),
                  plot.caption = element_text(hjust = 0.5))
        
    }
    
    suppressMessages(print(plots))

    if (!is.null(outFile)){
        writeLines(paste0('Output file written: ', fx_outFile(outFile)))
        dev.off()
    }
    
}

fx_roc <- function(modelPerfObj, permPerfObj = NULL, title.name = NULL, outFile = NULL){
    
    if(modelPerfObj$parameters$model.type=='svm'){
        stop('Cannot perform ROC on SVM model')
    }
    
    if(is.null(title.name)){title.name <- 'ROC Curve'}

    pred.prob <- modelPerfObj$pred.prob
    actual.class <- modelPerfObj$actual.class
    pred.prob.sort <- pred.prob[order(pred.prob,decreasing=T)]
    actual.class.sort <- actual.class[order(pred.prob,decreasing=T)]
    class.levels <- modelPerfObj$parameters$class.levels
    
    # generate fpr and tpr values and plt
    roc.vals <- lapply(c(1,pred.prob.sort), function(i){
        tp <- sum(pred.prob.sort>=i&actual.class.sort==class.levels[2])
        fp <- sum(pred.prob.sort>=i&actual.class.sort==class.levels[1])
        fn <- sum(pred.prob.sort<i&actual.class.sort==class.levels[2])
        tn <- sum(pred.prob.sort<i&actual.class.sort==class.levels[1])
        fpr <- fp/(fp+tn) # false-positive rate
        tpr <- tp/(tp+fn) # true-positive rate
        return(c(fpr,tpr))
    })
    fpr <- unlist(roc.vals)[c(T,F)] # fpr are odd elements
    tpr <- unlist(roc.vals)[c(F,T)] # tpr are even elements
    id <- order(fpr)
    roc.auc <- sum(diff(fpr[id])*rollmean(tpr[id],2))
    
    roc.df <- data.frame(fpr = fpr,
                         tpr = tpr
                         )
    p <- ggplot(roc.df, aes(x = fpr, y = tpr))
    
    if (!is.null(outFile)){pdf(fx_outFile(outFile))}
    
    if (!is.null(permPerfObj)){
        subtext <- paste0('AUC = ', signif(permPerfObj$df.pval['obs','auc.ROC'],3), 
                          '; p-value = ', signif(permPerfObj$df.pval['pval','auc.ROC'],3))
        if(permPerfObj$parameters$nkfcv){
            captext <- paste0('N perm = ', nrow(permPerfObj$df.perm), '; nkfcv = ', permPerfObj$parameters$nkfcv)
        } else if(is.numeric(permPerfObj$parameters$sample.type)){
            captext <- paste0('N perm = ', nrow(permPerfObj$df.perm), 
                              '; train group size = ', permPerfObj$parameters$sample.type,
                              '; nresamples = ', permPerfObj$parameters$nresample)
        }
        else {
            captext <- paste0('N perm = ', nrow(permPerfObj$df.perm))
        }
    } else {
        subtext <- paste0('AUC: ', signif(out$auc,3), '; 95%CI (Delong): ', 
                          signif(modelPerfObj$auc.95CI.delong[1], 3), '-', 
                          signif(modelPerfObj$auc.95CI.delong[3], 3))
        if(is.numeric(modelPerfObj$parameters$sample.type)){
            captext <- paste0('N resamples = ', modelPerfObj$parameters$nresample, 
                              '; train group size = ', modelPerfObj$parameters$sample.type
            )
        } else {
            captext <- 'No null distribution'
        }
        
    }
    
    print(p + 
              geom_segment(aes(x=0, y=0, xend=1, yend=1), color = 'gray', lwd = 2) +
              geom_point(size = 2.5, color = 'blue') + 
              geom_line(color = 'blue') + 
              labs(title = title.name,
                   subtitle = subtext,
                   x = 'False positive rate (1-specificity)',
                   y = 'True positive rate (sensitivity)',
                   caption = captext) + 
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    plot.caption = element_text(hjust = 0.5))
          )
    
    if (!is.null(outFile)){
        writeLines(paste0('Output file written: ', fx_outFile(outFile)))
        dev.off()
    }

}

fx_outFile <- function(outFile, ext.default = NULL){
    
    fname <- basename(outFile)
    dname <- dirname(outFile)
    if (!dir.exists(dname)){
        stop(paste0('Folder not found: ', dname, ''))
    }
    ext <- strsplit(fname, split="\\.")[[1]]
    if(!length(ext[-1]) & !is.null(ext.default)){
        fname <- paste0(fname, ext.default)
    } else if (!length(ext[-1]) & is.null(ext.default)){
        stop('File extension not specified.')
    }
    
    outFileNew <- paste(dname,fname,sep='/')
    return(outFileNew)
}

fx_summary <- function(){}

writeLines('Following functions loaded:')
writeLines('\tfx_scramble: Create df with scrambled group assignment')
writeLines('\tfx_partition: List of partitions to apply to data frame')
writeLines('\tfx_sample: Create train/test sub data frames')
writeLines('\tfx_model: Train/test model on sub data frames')
writeLines('\tfx_rocCompute: Computer AUC of ROC')
writeLines('\tfx_modelPerf: Confusion matrix and model performance metrics')
writeLines('\tfx_perm: Derive null distribution')
writeLines('\tfx_permPerf: Estimate p-values, organize null distributions')
writeLines('\tfx_permPlot: Plot observed vs. null distributions')
writeLines('\tfx_roc: Estimate and plot ROC')
writeLines('\tfx_outFile: Handles specified output files')
writeLines('\tfx_summary: Produces .txt summary of model info (incomplete)')


