## * fx_modelPerf
#' @title
#' @description Short description of the function
#' @name
#'
#' @param modelOutput
#' @param decisionThreshold
#' @param perm If TRUE, .... Otherwise ...
#' @param many
#'
#' @return Describe output of the function
#'
#' @examples
#' # R code showing how to use the function
#' 1+1
#'
#' @export
fx_modelPerf <- function(modelOutput, decisionThreshold = 0.5, many = T, perm = F){

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
        
        nmodels <- length(modelOutput)
        params.set <- !(names(modelOutput[[1]]$parameters)=='train.rows'|
            names(modelOutput[[1]]$parameters)=='test.rows')
        parameters <- modelOutput[[1]][['parameters']][params.set]
        class.levels <- parameters$class.levels
        
        nrows.df <- length(modelOutput[[1]]$parameters$train.rows)+
            length(modelOutput[[1]]$parameters$test.rows)
        
        regmodels <- c('regression')
        classmodels <- c('svm','rf','logistic')
        if(modelOutput[[1]]$parameters$model.type%in%regmodels){
            
            df.allfolds <- do.call(rbind, lapply(
                lapply(seq(nmodels), function(j){
                    return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
                                fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
                                pred.values.covar = modelOutput[[j]]$pred.covar$pred.values,
                                pred.values.full = modelOutput[[j]]$pred.full$pred.values,
                                actual.values = modelOutput[[j]]$pred.full$actual.values))
                }),
                data.frame))
            
        } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
            
            df.allfolds <- do.call(rbind, lapply(
                lapply(seq(nmodels), function(j){
                    return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
                                fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
                                pred.prob.covar = modelOutput[[j]]$pred.covar$pred.prob,
                                pred.prob.full = modelOutput[[j]]$pred.full$pred.prob,
                                pred.class.covar = modelOutput[[j]]$pred.covar$pred.class,
                                pred.class.full = modelOutput[[j]]$pred.full$pred.class,
                                actual.class = modelOutput[[j]]$pred.full$actual.class))
                }),
                data.frame))
            
            if(all(is.na(df.allfolds$pred.class.covar))){
                df.allfolds$pred.class.covar <- class.levels[as.numeric(df.allfolds$pred.prob.covar>decisionThreshold)+1]
                df.allfolds$pred.class.full <- class.levels[as.numeric(df.allfolds$pred.prob.full>decisionThreshold)+1]
            }
            
            parameters$cmat.descrip <- 'Rows: prediction, Columns: actual'
            parameters$negative.class <- parameters$class.levels[1]
            parameters$positive.class <- parameters$class.levels[2]
            parameters$decisionThreshold <- decisionThreshold
            
        } else {stop(paste0('Not sure how to handle: ', modelOutput[[1]]$parameters$model.type))}
    }
    
    foldPerf <- lapply(c('across',unique(df.allfolds$fold)), function(i){

        if(i=='across'){
            firstrows <- sapply(unique(df.allfolds$orig.df.row), function(j){
                return(which(df.allfolds$orig.df.row==j)[1])
            })
            df.tmp <- df.allfolds[firstrows,]
        } else {
            df.tmp <- df.allfolds[df.allfolds$fold==i,]
        }
        
        if(modelOutput[[1]]$parameters$model.type%in%regmodels){
            rmse.covar <- sqrt(mean((df.tmp$pred.values.covar-df.tmp$actual.values)**2))
            rmse.full <- sqrt(mean((df.tmp$pred.values.full-df.tmp$actual.values)**2))
            perfMetrics <- data.frame(rmse.covar = rmse.covar,
                                      rmse.full = rmse.full)
            return(list(perfMetrics = perfMetrics, fold = i))
                   
        } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
            
            cmat.covar <- cmat.full <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
            
            cmat.covar[class.levels[1],class.levels[1]] <-
                sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[1])
            cmat.covar[class.levels[2],class.levels[1]] <-
                sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[1])
            cmat.covar[class.levels[1],class.levels[2]] <-
                sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[2])
            cmat.covar[class.levels[2],class.levels[2]] <-
                sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[2])
            
            cmat.full[class.levels[1],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[1])
            cmat.full[class.levels[2],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[1])
            cmat.full[class.levels[1],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[2])
            cmat.full[class.levels[2],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[2])
            
            perfMetrics <- data.frame(TP.covar = cmat.covar[parameters$positive.class, parameters$positive.class],
                                      FP.covar = cmat.covar[parameters$positive.class, parameters$negative.class],
                                      TN.covar = cmat.covar[parameters$negative.class, parameters$negative.class],
                                      FN.covar = cmat.covar[parameters$negative.class, parameters$positive.class])
            perfMetrics$sens.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FN.covar)
            perfMetrics$spec.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FP.covar)
            perfMetrics$ppv.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FP.covar)
            perfMetrics$npv.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FN.covar)
            perfMetrics$acc.covar <- (perfMetrics$TP.covar + perfMetrics$TN.covar)/sum(cmat.covar)
            
            if(!all(is.na(df.tmp$pred.prob.covar))){
                perfMetrics$auc.ROC.covar <- fx_rocCompute(pred.prob = df.tmp$pred.prob.covar,
                                                     actual.class = df.tmp$actual.class,
                                                     class.levels = class.levels)
                perfMetrics$auc.ROC.full <- fx_rocCompute(pred.prob = df.tmp$pred.prob.full,
                                                           actual.class = df.tmp$actual.class,
                                                           class.levels = class.levels)
            } else {
                perfMetrics$auc.ROC.covar <- perfMetrics$auc.ROC.full <- NA
            }
            
            perfMetrics$TP.full <- cmat.full[parameters$positive.class, parameters$positive.class]
            perfMetrics$FP.full <- cmat.full[parameters$positive.class, parameters$negative.class]
            perfMetrics$TN.full <- cmat.full[parameters$negative.class, parameters$negative.class]
            perfMetrics$FN.full <- cmat.full[parameters$negative.class, parameters$positive.class]
            perfMetrics$sens.full <- perfMetrics$TP.full/(perfMetrics$TP.full + perfMetrics$FN.full)
            perfMetrics$spec.full <- perfMetrics$TN.full/(perfMetrics$TN.full + perfMetrics$FP.full)
            perfMetrics$ppv.full <- perfMetrics$TP.full/(perfMetrics$TP.full + perfMetrics$FP.full)
            perfMetrics$npv.full <- perfMetrics$TN.full/(perfMetrics$TN.full + perfMetrics$FN.full)
            perfMetrics$acc.full <- (perfMetrics$TP.full + perfMetrics$TN.full)/sum(cmat.full)
            
            return(list(cmat.covar = cmat.covar, cmat.full = cmat.full, perfMetrics = perfMetrics, fold = i))
            
        }
    })
    
    if(modelOutput[[1]]$parameters$model.type%in%regmodels){
        
        perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
        perfMetrics <- as.data.frame(cbind(perfMetrics,
                                           do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
        perfMetrics$fold <- as.character(perfMetrics$fold)
        perfMetrics[nrow(perfMetrics)+1,'fold'] <- 'within'
        perfMetrics[nrow(perfMetrics),colnames(perfMetrics)[colnames(perfMetrics)!='fold']] <- 
            unlist(lapply(colnames(perfMetrics)[colnames(perfMetrics)!='fold'], 
                          function(i){mean(perfMetrics[perfMetrics$fold%in%unique(df.allfolds$fold),i],na.rm=T)}))
        cmat.covar <- NULL
        cmat.full <- NULL
        
    } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
        
        perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
        perfMetrics <- as.data.frame(cbind(perfMetrics,
                                           do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
        perfMetrics$fold <- as.character(perfMetrics$fold)
        perfMetrics[nrow(perfMetrics)+1,'fold'] <- 'within'
        perfMetrics[nrow(perfMetrics),colnames(perfMetrics)[colnames(perfMetrics)!='fold']] <- 
            unlist(lapply(colnames(perfMetrics)[colnames(perfMetrics)!='fold'], 
                          function(i){mean(perfMetrics[perfMetrics$fold%in%unique(df.allfolds$fold),i], na.rm=T)}))
        cmat.covar <- foldPerf[[1]]$cmat.covar
        cmat.full <- foldPerf[[1]]$cmat.full
        
    }
    
    return(list(perfMetrics = perfMetrics, parameters = parameters, cmat.covar = cmat.covar, cmat.full = cmat.full, df.allfolds = df.allfolds))
}
## * fx_scramble
#'
#' @keywords internal
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

## * fx_partition
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

## * fx_sample
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

## * fx_model
fx_model <- function(df.set, covar = NULL, voi = NULL, outcome = NULL, model.type = 'logistic', z.pred = T){
    
    classmodels <- c('logistic', 'rf', 'svm')
    regmodels <- c('regression')
    model.set <- c(classmodels,regmodels)
    if(!tolower(model.type) %in% model.set){
        stop(paste0('Specify appropriate model type. Choose from: ', paste0(model.set, collapse = ', ')))
    } else {model.type <- tolower(model.type)}
    
    formula.covar <- as.formula(paste0(
        outcome, ' ~ ', paste(covar, collapse = '+')))
    formula.full <- as.formula(paste0(
        outcome, ' ~ ', paste(c(covar,voi), collapse = '+')))
    
    if(model.type=='regression'){
        if (is.factor(df.set$df.train[,outcome])){
            stop('Regression not allowed for factor outcomes')
        }
        class.levels <- NA
    } else {
        if (!is.factor(df.set$df.train[,outcome])){
            stop(paste0(model.type, ' (classification) not allowed for continuous outcomes'))
        }
        class.levels <- levels(df.set$df.train[,outcome])
    }
    
    parameters <- list(sample.type = df.set$parameters$sample.type, 
                       train.rows = df.set$parameters$train.rows, 
                       test.rows = df.set$parameters$test.rows, 
                       class.levels = class.levels,
                       model.type = model.type, 
                       covar = covar, 
                       voi = voi,
                       outcome = outcome, 
                       formula.covar = formula.covar,
                       formula.full = formula.full,
                       data.frame = df.set$parameters$data.frame
                       )
    
    # standardize non-factor predictor variables
    if(z.pred){
        
        pred.continuous <- c(covar,voi)[sapply(c(covar,voi), function(i){!is.factor(df.set$df.train[,i])})]
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
    
    # Apply and predict model
    if (model.type == 'logistic'){
        
        model.covar <- glm(formula.covar, data = df.train, family = 'binomial')
        pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                 pred.prob = predict(model.covar, newdata = df.test, type = 'resp'),
                                 actual.class = as.character(df.set$df.test[,outcome])
                                 )
        
        model.full <- glm(formula.full, data = df.train, family = 'binomial')
        pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                pred.prob = predict(model.full, newdata = df.test, type = 'resp'),
                                actual.class = as.character(df.set$df.test[,outcome])
                                )
        
    } else if (model.type == 'rf'){
        
        model.covar <- randomForest(formula.covar, data = df.train)
        pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                 pred.prob = predict(model.covar, 
                                                     newdata = df.test, 
                                                     type = 'prob')[,parameters$class.levels[2]],
                                 actual.class = as.character(df.set$df.test[,outcome]))
        
        model.full <- randomForest(formula.full, data = df.train)
        pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                pred.prob = predict(model.full, 
                                                     newdata = df.test, 
                                                     type = 'prob')[,parameters$class.levels[2]],
                                actual.class = as.character(df.set$df.test[,outcome]))
        
    } else if (model.type == 'svm'){
        
        model.covar <- svm(formula.covar, data = df.train)
        pred.covar <- data.frame(pred.class = as.character(predict(model.covar, newdata = df.test)),
                                 pred.prob = rep(NA, nrow(df.test)),
                                 actual.class = as.character(df.set$df.test[,outcome]))
        
        model.full <- svm(formula.full, data = df.train)
        pred.full <- data.frame(pred.class = as.character(predict(model.full, newdata = df.test)),
                                pred.prob = rep(NA, nrow(df.test)),
                                actual.class = as.character(df.set$df.test[,outcome]))
        
    } else if (model.type == 'regression'){
        
        model.covar <- lm(formula.covar, data = df.train)
        pred.covar <- data.frame(pred.values = predict(model.covar, newdata = df.test),
                                 actual.values = df.set$df.test[,outcome])
        
        model.full <- lm(formula.full, data = df.train)
        pred.full <- data.frame(pred.values = predict(model.full, newdata = df.test),
                                actual.values = df.set$df.test[,outcome])
        
    }
    
    if (is.numeric(parameters$sample.type)){
        parameters$nresample <- df.set$parameters$nresample
        parameters$balance.col <- df.set$parameters$balance.col
    }
    return(list(pred.covar = pred.covar,
                pred.full = pred.full,
                parameters = parameters))
}

## * fx_rocCompute
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

## * fx_cmatrix
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
} # deprecated

## * fx_perm
fx_perm <- function(df, modelPerfObj, modelObj, partitionList, nperm = 10, n.cores = 20){
    
    permPerfObj <- list()
    updateMarks <- seq(from = 0, to = nperm, length.out = 11)
    writeLines('Deriving permutation results...')
    
    parameters <- modelObj[[1]]$parameters
    if(is.numeric(parameters$sample.type)){
        parameters$nresample <- partitionList[[1]]$nresample
        parameters$balance.col <- partitionList[[1]]$balance.col
    }
    decisionThreshold <- modelPerfObj$parameters$decisionThreshold
    
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
                     covar = parameters$covar,
                     voi = parameters$voi,
                     outcome = parameters$outcome,
                     model.type = parameters$model.type)},
            mc.cores = n.cores)
        modelPerfObjPerm <- fx_modelPerf(modelObjPerm, 
                                         decisionThreshold = decisionThreshold)
        
        modelPerfObjPerm$df.allfolds <- NULL
        modelPerfObjPerm$parameters <- NULL
        permPerfObj[[i]] <- modelPerfObjPerm
    }

    writeLines('Permutation testing complete!')
    return(permPerfObj)
}

## * fx_permPerf
fx_permPerf <- function(modelPerfObj, permObj, measures = NULL, nkfcv = F, compute.perf = 'across', df.perm.out = T){
    
    if (nkfcv){
        niter <- length(modelPerfObj)
        parameters <- modelPerfObj[[1]]$parameters
        parameters$nkfcv <- niter
    } else {
        parameters <- modelPerfObj$parameters
        parameters$nkfcv <- nkfcv
    }
    
    parameters$nperm <- length(permObj)
    
    regmodels <- c('regression')
    classmodels <- c('svm','rf','logistic')
    
    if(parameters$model.type%in%regmodels){
        measuresSet <- c('rmse')
    } else if(parameters$model.type%in%classmodels) {
        measuresSet <- c('acc', 'auc.ROC', 'sens', 'spec', 'ppv', 'npv')
    }
    
    if(is.null(measures)){
        measures <- measuresSet
    } else {
        if(any(!measures %in% measuresSet)){
            stop(paste0('Unknown outcome measures: ', paste(measures[which(!measures%in%measuresSet)], collapse = ',')))
        }
    }
    
    if (parameters$model.type%in%regmodels){
        df.perm <- data.frame(rmse=sapply(seq(nperm), function(j){permObj[[j]]$summaryMetrics}))
    } else if(parameters$model.type%in%classmodels){
        
        df.perm <- as.data.frame(do.call(rbind,lapply(seq(nperm), function(j){
            permObj[[j]]$perfMetrics[permObj[[j]]$perfMetrics$fold==compute.perf,]
        })))
        
    }
    
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
        if (parameters$model.type%in%regmodels){
            obs <- modelPerfObj$summaryMetrics
            pval <- (sum(df.perm[,i]<obs)+(sum(df.perm[,i]==obs)*0.5))/nperm
        } else if(parameters$model.type%in%classmodels){
            obs <- modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,
                                            c(paste0(measures,'.covar'), paste0(measures,'.full'))]
            pval <- sapply(names(obs), function(i){(sum(df.perm[,i]>obs[[i]],na.rm=T)+(sum(df.perm[,i]==obs[[i]],na.rm=T)*0.5))/sum(!is.na(df.perm[,i]))})
            
            df.pval <- as.data.frame(rbind(obs,pval))
            rownames(df.pval) <- c('obs','pval')
        }
        
    }
    
    if(df.perm.out){
        return(list(df.perm=df.perm,
                    df.pval=df.pval,
                    parameters = parameters))
    } else {
        return(list(df.pval=df.pval,
                    parameters = parameters))
    }
    
    
}

## * fx_permPlot
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

## * fx_roc
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

## * fx_outFile
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

## * fx_summary
fx_summary <- function(){}



