require('mvtnorm')
require('parallel')
require('pROC')
require('lava')
require('scales')
require('ggplot2')
require('randomForest')
require('e1071')
require('zoo')

####
## DEFINE FUNCTIONS
####

fx_scramble <- function(df, outcome, boot = F, outcome.class = T){
    if(is.null(outcome)){
        stop('Specify variable for scrambling')
    } else {
        if(!outcome%in%colnames(df)){
            stop('Specified outcome variable (', outcome, ') not in data.frame')
        }
    }
    if(!boot){
        df[,outcome] <- sample(df[,outcome])
    } else {
        if(outcome.class){
            boot_sample <- sample(unlist(lapply(levels(df[,outcome]), function(i){
                sample(which(df[,outcome]==i), replace = T)
            }))) # outer resample randomizes wrt partitionList
            df <- df[boot_sample,]
        } else {
            boot_sample <- sample(nrow(df), replace = T)
            df <- df[boot_sample,]
        }
    }
    
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

fx_model <- function(df.set, covar = NULL, voi = NULL, outcome = NULL, model.type = 'logistic', z.pred = T){
    
    classmodels <- c('logistic', 'rf', 'svm')
    regmodels <- c('regression')
    model.set <- c(classmodels,regmodels)
    if(!tolower(model.type) %in% model.set){
        stop(paste0('Specify appropriate model type. Choose from: ', paste0(model.set, collapse = ', ')))
    } else {model.type <- tolower(model.type)}
    
    if(!is.null(covar)){
        formula.covar <- as.formula(paste0(
            outcome, ' ~ ', paste(covar, collapse = '+')))
    } else {
        formula.covar <- NULL
    }
    
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
        
        model.full <- glm(formula.full, data = df.train, family = 'binomial')
        pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                pred.prob = predict(model.full, newdata = df.test, type = 'resp'),
                                actual.class = as.character(df.set$df.test[,outcome])
        )
        
        if(!is.null(covar)){
            model.covar <- glm(formula.covar, data = df.train, family = 'binomial')
            pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                     pred.prob = predict(model.covar, newdata = df.test, type = 'resp'),
                                     actual.class = as.character(df.set$df.test[,outcome])
            )
        } else {
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.class <- NA
            pred.covar$pred.prob <- NA
        }
        
        
        
    } else if (model.type == 'rf'){
        
        model.full <- randomForest(formula.full, data = df.train)
        pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                pred.prob = predict(model.full, 
                                                    newdata = df.test, 
                                                    type = 'prob')[,parameters$class.levels[2]],
                                actual.class = as.character(df.set$df.test[,outcome]))
        
        if(!is.null(covar)){
            model.covar <- randomForest(formula.covar, data = df.train)
            pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                     pred.prob = predict(model.covar, 
                                                         newdata = df.test, 
                                                         type = 'prob')[,parameters$class.levels[2]],
                                     actual.class = as.character(df.set$df.test[,outcome]))
        } else {
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.class <- NA
            pred.covar$pred.prob <- NA
        }
        
        
        
    } else if (model.type == 'svm'){
        
        model.full <- svm(formula.full, data = df.train)
        pred.full <- data.frame(pred.class = as.character(predict(model.full, newdata = df.test)),
                                pred.prob = rep(NA, nrow(df.test)),
                                actual.class = as.character(df.set$df.test[,outcome]))
        
        if(!is.null(covar)){
            model.covar <- svm(formula.covar, data = df.train)
            pred.covar <- data.frame(pred.class = as.character(predict(model.covar, newdata = df.test)),
                                     pred.prob = rep(NA, nrow(df.test)),
                                     actual.class = as.character(df.set$df.test[,outcome]))
        } else {
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.class <- NA
            pred.covar$pred.prob <- NA
        }
        
    } else if (model.type == 'regression'){
        
        model.full <- lm(formula.full, data = df.train)
        pred.full <- data.frame(pred.values = predict(model.full, newdata = df.test),
                                actual.values = df.set$df.test[,outcome],
                                rsq = summary(model.full)$r.squared)
        
        if(!is.null(covar)){
            model.covar <- lm(formula.covar, data = df.train)
            pred.covar <- data.frame(pred.values = predict(model.covar, newdata = df.test),
                                     actual.values = df.set$df.test[,outcome],
                                     rsq = summary(model.covar)$r.squared)
        } else {
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.values <- NA
            pred.covar$actual.values <- NA
            pred.covar$rsq <- NA
        }
        
    }
    
    if (is.numeric(parameters$sample.type)){
        parameters$nresample <- df.set$parameters$nresample
        parameters$balance.col <- df.set$parameters$balance.col
    }
    return(list(pred.covar = pred.covar,
                pred.full = pred.full,
                parameters = parameters))
}

#' @title xxx
#' @description Short description of the function
#' @name fx_modelPerf
#' 
#' @param modelOutput aa
#' @param decisionThreshold bb
#' @param perm If TRUE, .... Otherwise ...
#' @param many cc
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
        
        if(parameters$model.type%in%regmodels){
            
            foldrsq.full <- sapply(seq(nmodels), function(j){modelOutput[[j]]$pred.full$rsq[1]})
            rmse.full <- sqrt(mean((df.tmp$pred.values.full-df.tmp$actual.values)**2))
            if(!is.null(parameters$covar)){
                foldrsq.covar <- sapply(seq(nmodels), function(j){modelOutput[[j]]$pred.covar$rsq[1]})
                rmse.covar <- sqrt(mean((df.tmp$pred.values.covar-df.tmp$actual.values)**2))
            } else {
                foldrsq.covar <- rep(NA,seq(nmodels))
                rmse.covar <- NA
            }
            
            if(i=='across'){
                
                rsq.full <- mean(foldrsq.full)
                if(!is.null(parameters$covar)){
                    rsq.covar <- mean(foldrsq.covar)
                } else {
                    rsq.covar <- NA
                }
                
            } else {
                
                rsq.full <- foldrsq.full[as.numeric(i)]
                if(!is.null(parameters$covar)){
                    rsq.covar <- foldrsq.covar[as.numeric(i)]
                } else {
                    rsq.covar <- NA
                }
            }
            
            perfMetrics <- data.frame(rmse.covar = rmse.covar,
                                      rmse.full = rmse.full,
                                      rsq.covar = rsq.covar,
                                      rsq.full = rsq.full)
            return(list(perfMetrics = perfMetrics, fold = i))
                   
        } else if(parameters$model.type%in%classmodels){
            
            cmat.full <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
            cmat.full[class.levels[1],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[1])
            cmat.full[class.levels[2],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[1])
            cmat.full[class.levels[1],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[2])
            cmat.full[class.levels[2],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[2])
            
            if(!is.null(parameters$covar)){
                cmat.covar <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
                cmat.covar[class.levels[1],class.levels[1]] <-
                    sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[1])
                cmat.covar[class.levels[2],class.levels[1]] <-
                    sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[1])
                cmat.covar[class.levels[1],class.levels[2]] <-
                    sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[2])
                cmat.covar[class.levels[2],class.levels[2]] <-
                    sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[2])
            } else {
                cmat.covar <- matrix(NA,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
            }
            
            perfMetrics <- data.frame(TP.covar = cmat.covar[parameters$positive.class, parameters$positive.class],
                                      FP.covar = cmat.covar[parameters$positive.class, parameters$negative.class],
                                      TN.covar = cmat.covar[parameters$negative.class, parameters$negative.class],
                                      FN.covar = cmat.covar[parameters$negative.class, parameters$positive.class])
            perfMetrics$sens.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FN.covar)
            perfMetrics$spec.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FP.covar)
            perfMetrics$ppv.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FP.covar)
            perfMetrics$npv.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FN.covar)
            perfMetrics$acc.covar <- (perfMetrics$TP.covar + perfMetrics$TN.covar)/sum(cmat.covar)
            
            if(!is.null(parameters$covar)){
                rocCompute.covar <- fx_rocCompute(pred.prob = df.tmp$pred.prob.covar,
                                                  actual.class = df.tmp$actual.class,
                                                  class.levels = class.levels)
                perfMetrics$auc.ROC.covar <- rocCompute.covar$roc.auc
                perfMetrics$optThresh.covar <- rocCompute.covar$optimal.threshold
            } else {
                perfMetrics$auc.ROC.covar <- perfMetrics$optThresh.covar <- NA
            }
            
            if(!all(is.na(df.tmp$pred.prob.full))){
                rocCompute.full <- fx_rocCompute(pred.prob = df.tmp$pred.prob.full,
                                                  actual.class = df.tmp$actual.class,
                                                  class.levels = class.levels)
                
                perfMetrics$auc.ROC.full <- rocCompute.full$roc.auc
                perfMetrics$optThresh.full <- rocCompute.full$optimal.threshold
            } else {
                perfMetrics$auc.ROC.full <- perfMetrics$optThresh.full <- NA
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
    
    return(list(perfMetrics = perfMetrics, 
                parameters = parameters, 
                cmat.covar = cmat.covar, 
                cmat.full = cmat.full, 
                df.allfolds = df.allfolds))
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
    
    roc.df <- data.frame(do.call(rbind, roc.vals))
    colnames(roc.df) <- c('fpr','tpr')
    roc.df$thresh <- c(1,pred.prob.sort)
    roc.df$tldist <- sqrt(((1-roc.df$tpr)**2)+(roc.df$fpr**2))
    # thresh.range <- c(ceiling(roc.df$thresh[which(roc.df$tldist==min(roc.df$tldist))+1]*1000)/1000, floor(roc.df$thresh[roc.df$tldist==min(roc.df$tldist)]*1000)/1000)
    optimal.threshold <- signif(mean(roc.df$thresh[roc.df$tldist==min(roc.df$tldist)]),3)
    fpr <- unlist(roc.vals)[c(T,F)] # fpr are odd elements
    tpr <- unlist(roc.vals)[c(F,T)] # tpr are even elements
    id <- order(fpr)
    roc.auc <- sum(diff(fpr[id])*rollmean(tpr[id],2))
    return(list(roc.auc=roc.auc,optimal.threshold=optimal.threshold))
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
} # deprecated

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

fx_boot <- function(df, modelPerfObj, modelObj, partitionList, nboot = 10, n.cores = 20){

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
        df.boot <- fx_scramble(df, parameters$outcome,
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

fx_bootPerf <- function(modelPerfObj, bootObj, measures = NULL, compute.perf = 'within', df.iter.out = T, qrange = c(0.025, 0.975)){
    
    parameters <- modelPerfObj$parameters
    parameters$nboot <- length(bootObj)
    parameters$nkfcv <- F
    parameters$compute.perf <- compute.perf
    
    regmodels <- c('regression')
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
    
    df.iter <- as.data.frame(do.call(rbind,lapply(seq(parameters$nboot), function(i){
        bootObj[[i]]$perfMetrics[bootObj[[i]]$perfMetrics$fold==compute.perf,]
    })))
    
    obs <- modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,
                                    c(paste0(measures,'.covar'), paste0(measures,'.full'))]
    ci <- sapply(names(obs), function(i){c(quantile(df.iter[,i], probs = qrange, na.rm = T))})
    df.pval <- as.data.frame(rbind(obs,ci))
    rownames(df.pval)[1] <- 'obs'
    
    if(df.iter.out){
        return(list(df.iter=df.iter,
                    df.pval=df.pval,
                    parameters = parameters))
    } else {
        return(list(df.pval=df.pval,
                    parameters = parameters))
    }
    
    
}

fx_permPerf <- function(modelPerfObj, permObj, measures = NULL, nkfcv = F, compute.perf = 'within', df.iter.out = T){
    
    if (nkfcv){
        niter <- length(modelPerfObj)
        parameters <- modelPerfObj[[1]]$parameters
        parameters$nkfcv <- niter
    } else {
        parameters <- modelPerfObj$parameters
        parameters$nkfcv <- nkfcv
    }
    
    parameters$nperm <- length(permObj)
    parameters$compute.perf <- compute.perf
    
    regmodels <- c('regression')
    classmodels <- c('svm','rf','logistic')
    
    if(parameters$model.type%in%regmodels){
        measuresSet <- c('rmse', 'rsq')
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
    
    df.iter <- as.data.frame(do.call(rbind,lapply(seq(parameters$nperm), function(i){
        permObj[[i]]$perfMetrics[permObj[[i]]$perfMetrics$fold==compute.perf,]
    })))
    
    if(nkfcv){
        df.pval <- as.data.frame(
            sapply(measures, function(i){
                obs <- mean(sapply(seq(niter), function(j){
                    modelPerfObj[[j]][[i]]
                    }))
                pval <- (sum(df.iter[,i]>obs)+(sum(df.iter[,i]==obs)*0.5))/nperm
                c(obs,pval)
            }),
            row.names = c('obs','pval'))
    } else {
        
        obs <- modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,
                                        c(paste0(measures,'.covar'), paste0(measures,'.full'))]
        pval <- sapply(names(obs), function(i){(sum(df.iter[,i]>obs[[i]],na.rm=T)+(sum(df.iter[,i]==obs[[i]],na.rm=T)*0.5))/sum(!is.na(df.iter[,i]))})
        df.pval <- as.data.frame(rbind(obs,pval))
        rownames(df.pval) <- c('obs','pval')
        if (parameters$model.type%in%regmodels){df.pval['pval', grep('^(rmse)', colnames(df.pval))] <- 1-df.pval['pval', grep('^(rmse)', colnames(df.pval))]} #HIGHER rmse is LESS desirable
        
    }
    
    if(df.iter.out){
        return(list(df.iter=df.iter,
                    df.pval=df.pval,
                    parameters = parameters))
    } else {
        return(list(df.pval=df.pval,
                    parameters = parameters))
    }
    
    
}

fx_plot <- function(perfObj, outFile = NULL){

    obj.type <- if('nperm'%in%names(perfObj$parameters)){
        obj.type <- 'perm'
    } else if('nboot'%in%names(perfObj$parameters)){
        obj.type <- 'boot'
    } else {stop('Cannot identify object type.')}

    regmodels <- c('regression')
    classmodels <- c('svm','rf','logistic')
    if(perfObj$parameters$model.type%in%regmodels){
        measures <- colnames(perfObj$df.iter)[colnames(perfObj$df.iter)!='fold']
    } else if(perfObj$parameters$model.type%in%classmodels){
        measures <- c("sens.covar", "spec.covar", "acc.covar", "auc.ROC.covar", "sens.full", "spec.full", "acc.full", "auc.ROC.full")
    }

    if(!is.null(outFile)){
        pdf(fx_outFile(outFile))
        writeLines(paste0('Plots being written to: ', fx_outFile(outFile)))
    }

    plots <- list()
    for (measure in measures){

        if(obj.type == 'perm'){
            
            subtext <- paste0('obs: ', signif(perfObj$df.pval['obs', measure],3),
                   ', p = ', signif(perfObj$df.pval['pval', measure],3))
            
        } else if(obj.type == 'boot'){
            
            if(grepl('^(auc.ROC)', measure)){
                pval <- signif(ecdf(perfObj$df.iter[[measure]])(0.5),3)
            } else {
                pval <- 'NA'
            }
            
            subtext <- paste0('obs: ', signif(perfObj$df.pval['obs', measure],3),
                              ' [', signif(perfObj$df.pval['2.5%', measure],3),
                              '; ', signif(perfObj$df.pval['97.5%', measure],3),
                              '], boot-p = ', pval)
            
        }
        
        captext <- paste0('N ', obj.type, ' = ', nrow(perfObj$df.iter))
        
        if(!grepl('^(auc.ROC)', measure)){
            captextEnd <- paste0('; dec. thresh = ', perfObj$parameters$decisionThreshold)
        } else {
            captextEnd <- NULL
        }

        if(perfObj$parameters$nkfcv){
            captext <- paste0(captext, 
                              '; nkfcv = ', perfObj$parameters$nkfcv,
                              captextEnd)
            
            perfValRange <- quantile(sapply(seq(length(mpo)), function(i){mpo[[i]]$accuracy}),probs = c(0.025, 0.975))
        } else if(is.numeric(perfObj$parameters$sample.type)){
            captext <- paste0(captext,
                              '; train group size = ', perfObj$parameters$sample.type,
                              '; nresamples = ', perfObj$parameters$nresample,
                              captextEnd)
        } else {
            captext <- paste0(captext, 
                              captextEnd)
        }

        if(perfObj$parameters$model.type%in%regmodels){
            plots[[length(plots)+1]] <-
                ggplot(data = perfObj$df.iter, aes_string(x=measure)) +
                geom_histogram(fill = 'darkblue') +
                geom_vline(data = perfObj$df.pval['obs',], aes_string(xintercept=measure),
                           color = 'darkorange', linetype = 'dashed', size = 2) +
                scale_y_continuous(name='Frequency') +
                labs(title = measure,
                     subtitle = subtext,
                     caption = captext) +
                theme(plot.title = element_text(hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5),
                      plot.caption = element_text(hjust = 0.5))

        } else if(perfObj$parameters$model.type%in%classmodels){
            plots[[length(plots)+1]] <-
                ggplot(data = perfObj$df.iter, aes_string(x=measure)) +
                geom_histogram(fill = 'darkblue') +
                geom_vline(data = perfObj$df.pval['obs',], aes_string(xintercept=measure),
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

    }

    suppressMessages(print(plots))

    if (!is.null(outFile)){
        writeLines(paste0('Output file written: ', fx_outFile(outFile)))
        dev.off()
    }

}

fx_rocPlot <- function(modelPerfObj, permPerfObj = NULL, bootPerfObj = NULL, title.name = NULL, compute.perf = 'within', plot.covar = T, plot.full = T, outFile = NULL){
    
    
    if(!plot.covar&!plot.full){
        stop('Why plot nothing?')
    }
    
    perm.exist <- !is.null(permPerfObj)
    boot.exist <- !is.null(bootPerfObj)
    
    parameters <- modelPerfObj$parameters
    
    regmodels <- c('regression')
    if (modelPerfObj$parameters$model.type%in%regmodels){
        stop('Cannot compute ROC for regression models')
    }
    
    if(modelPerfObj$parameters$model.type=='svm'){
        stop('Cannot perform ROC on SVM model')
    }
    
    if(is.null(title.name)){title.name <- 'ROC Curve'}
    
    pred.prob.covar.sorted <- modelPerfObj$df.allfolds$pred.prob.covar[order(modelPerfObj$df.allfolds$pred.prob.covar,decreasing=T)]
    actual.class.covar.sorted <- modelPerfObj$df.allfolds$actual.class[order(modelPerfObj$df.allfolds$pred.prob.covar,decreasing=T)]
    pred.prob.full.sorted <- modelPerfObj$df.allfolds$pred.prob.full[order(modelPerfObj$df.allfolds$pred.prob.full,decreasing=T)]
    actual.class.full.sorted <- modelPerfObj$df.allfolds$actual.class[order(modelPerfObj$df.allfolds$pred.prob.full,decreasing=T)]
    class.levels <- modelPerfObj$parameters$class.levels
    
    # generate fpr and tpr values and plt
    # c('covar','full')
    roc.df <- do.call(rbind, lapply(c('covar','full')[c(plot.covar,plot.full)], function(j){
        pred <- get(paste0("pred.prob.", j, ".sorted"))
        actual <- get(paste0("actual.class.", j, ".sorted"))
        
        roc.vals <- lapply(c(1,pred), function(i){
            tp <- sum(pred>=i&actual==class.levels[2])
            fp <- sum(pred>=i&actual==class.levels[1])
            fn <- sum(pred<i&actual==class.levels[2])
            tn <- sum(pred<i&actual==class.levels[1])
            fpr <- fp/(fp+tn) # false-positive rate
            tpr <- tp/(tp+fn) # true-positive rate
            return(c(fpr,tpr))
        })
        fpr <- unlist(roc.vals)[c(T,F)] # fpr are odd elements
        tpr <- unlist(roc.vals)[c(F,T)] # tpr are even elements
        
        tmp.df <- data.frame(fpr = c(0,fpr), tpr = c(0,tpr), type = j)
        return(tmp.df)
    }))
    
    if (!is.null(outFile)){pdf(fx_outFile(outFile))}
    
    if(perm.exist&boot.exist){
        
        covar.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.covar'],3)
        full.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.full'],3)
        covar.p <- signif(permPerfObj$df.pval['pval','auc.ROC.covar'],3)
        full.p <- signif(permPerfObj$df.pval['pval','auc.ROC.full'],3)
        covar.ci <- paste0('[',paste(signif(bootPerfObj$df.pval[c('2.5%', '97.5%'),'auc.ROC.covar'],3),collapse = ','),']')
        full.ci <- paste0('[',paste(signif(bootPerfObj$df.pval[c('2.5%', '97.5%'),'auc.ROC.full'],3),collapse = ','),']')
        nperm <- permPerfObj$parameters$nperm
        nboot <- bootPerfObj$parameters$nboot
        
        if(plot.covar&plot.full){
            subtext <- paste0('covar: ', covar.obs, ' ', covar.ci, ', p = ', covar.p, '; full: ', full.obs, ' ', full.ci, ', p = ', full.p)
        } else if(plot.covar&!plot.full){
            subtext <- paste0('covar: ', covar.obs, ' ', covar.ci, ', p = ', covar.p)
        } else if(!plot.covar&plot.full){
            subtext <- paste0('full: ', full.obs, ' ', full.ci, ', p = ', full.p)
        }
        
        captext <- paste0('nperm: ', nperm, '; nboot: ', nboot)
        
    } else if(perm.exist&!boot.exist){
        
        covar.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.covar'],3)
        covar.p <- signif(permPerfObj$df.pval['pval','auc.ROC.covar'],3)
        full.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.full'],3)
        full.p <- signif(permPerfObj$df.pval['pval','auc.ROC.full'],3)
        nperm <- permPerfObj$parameters$nperm
        
        subtext <- paste0('covar: ', covar.obs, ' 95% CI NA, p = ', covar.p, '; full: ', full.obs, ' 95% CI NA, p = ', full.p)
        captext <- paste0('nperm: ', nperm, '; nboot: NA')
        
    } else if(!perm.exist&boot.exist){
        
        covar.obs <- signif(bootPerfObj$df.pval['obs','auc.ROC.covar'],3)
        full.obs <- signif(bootPerfObj$df.pval['obs','auc.ROC.full'],3)
        covar.p <- signif(ecdf(bootPerfObj$df.iter$auc.ROC.covar)(0.5),3)
        full.p <- signif(ecdf(bootPerfObj$df.iter$auc.ROC.full)(0.5),3)
        covar.ci <- paste0('[',paste(signif(bootPerfObj$df.pval[c('2.5%', '97.5%'),'auc.ROC.covar'],3),collapse = ','),']')
        full.ci <- paste0('[',paste(signif(bootPerfObj$df.pval[c('2.5%', '97.5%'),'auc.ROC.full'],3),collapse = ','),']')
        nboot <- bootPerfObj$parameters$nboot
        
        if(plot.covar&plot.full){
            subtext <- paste0('covar: ', covar.obs, ' ', covar.ci, ', boot-p = ', covar.p, '; full: ', full.obs, ' ', full.ci, ', boot-p = ', full.p)
        } else if(plot.covar&!plot.full){
            subtext <- paste0('covar: ', covar.obs, ' ', covar.ci, ', boot-p = ', covar.p)
        } else if(!plot.covar&plot.full){
            subtext <- paste0('full: ', full.obs, ' ', full.ci, ', boot-p = ', full.p)
        }
        
        captext <- paste0('nperm: NA; nboot: ', nboot)
        
    } else if(!perm.exist&&!boot.exist&&!is.null(compute.perf)){
        
        covar.obs <- signif(modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,'auc.ROC.covar'],3)
        full.obs <- signif(modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,'auc.ROC.full'],3)
        
        if(plot.covar&plot.full){
            subtext <- paste0('covar: ', covar.obs, ' 95% CI NA, p = NA; full: ', full.obs, ' 95% CI NA, p = NA')
        } else if(plot.covar&!plot.full){
            subtext <- paste0('covar: ', covar.obs, ' 95% CI NA, p = NA')
        } else if(!plot.covar&plot.full){
            subtext <- paste0('full: ', full.obs, ' 95% CI NA, p = NA')
        }
        
        captext <- 'nperm: NA; nboot: NA'
        
    } else {
        subtext <- NULL
        captext <- NULL
    }
    
    
    p <- ggplot(data = roc.df, aes(x=fpr,y=tpr,group=type,color=type))
    print(p + 
              geom_segment(aes(x=0, y=0, xend=1, yend=1), color = 'gray', lwd = 2) +
              geom_line(lwd = 2)+
              labs(title = title.name,
                   subtitle = subtext,
                   x = 'False positive rate (1-specificity)',
                   y = 'True positive rate (sensitivity)',
                   caption = captext,
                   color = 'Model') + 
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
        ext.default <- '.pdf'
        fname <- paste0(fname, ext.default)
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
writeLines('\tfx_modelPerf: Confusion matrix and model performance metrics')
writeLines('\tfx_rocCompute: Compute AUC of ROC')
writeLines('\tfx_perm: Derive null distribution')
writeLines('\tfx_boot: Bootstrap confidence intervals')
writeLines('\tfx_permPerf: Estimate p-values, organize null distributions')
writeLines('\tfx_bootPerf: Estimate CIs and p-values from bootstrap distributions')
writeLines('\tfx_plot: Plot observed vs. null/bootstrap distributions')
writeLines('\tfx_rocPlot: Estimate and plot ROC')
writeLines('\tfx_outFile: Handles specified output files')
writeLines('\tfx_summary: Produces .txt summary of model info (incomplete)')

## TODO
# fill out fx_summary

