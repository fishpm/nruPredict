## * fx_modelPerf (documentation)
##' @title Summary Measures of Model Performance
##' @description Derive summary measures of model performance
##' 
##' @param modelOutput ???
##' @param dthresh ???
##' @param many ???
##' @param perm ???
##'
##' @return A list of length five, containing the following elements:
##' \itemize{
##' \item "perfMetrics" Model performance metrics for each individual fold and "across" and "within".
##' \cr "across":   sum or mean of metric across folds
##' \cr "within":   mean of metric across folds
##' \itemize{
##' \item TP: true positive
##' \item FP: false positive
##' \item TN: true negative
##' \item FN: false negative
##' \item sens: sensitivity
##' \item spec: specificity
##' \item ppv: positive predictive value
##' \item npv: negative predictive value
##' \item acc: accuracy
##' \item auc.ROC: area under the curve of ROC curve
##' \item optThresh: optimal decision threshold determined from training data
##' }
##' 
##' \item "cmat.covar": confusion matrix of covariate model (at "dthresh" decision threshold)
##' 
##' \item "cmat.full":    confusion matrix of full model (at "dthresh" decision threshold)
##' 
##' \item "df.allfolds":  data frame for test-related model predictions
##' \itemize{
##' \item orig.df.row: row in original data frame for specific observation,
##' \item fold: fold assignment
##' \item pred.prob.covar: predicted probability of class membership from covariate model
##' \item pred.prob.full: predicted probability of class membership from full model
##' \item pred.class.covar: predicted class from covariate model
##' \item pred.class.full: predicted class from full model
##' \item actual.class: actual class membership
##' }
##' 
##' \item "parameters": list of relevant specified parameters
##' \itemize{
##' \item "sample.type": cross-validation sampling procedure
##' \item "class.levels": class levels
##' \item "model.type": machine learning model framework
##' \item "covar": specified covariates
##' \item "voi": specified variables of interest
##' \item "outcome": name of class being predicted
##' \item "formula.covar": formula object for covariate model
##' \item "formula.full": formula object for full model
##' \item "data.frame": data frame specified (CURRENTLY NOT CORRECTLY SPECIFIED)
##' \item "cmat.descrip": key for how to understand confusion matrices ()
##' \item "negative.class": class assigned to probability = 0
##' \item "positive.class": class assigned to probability = 1
##' \item "dthresh": decision threshold
##' \item "z.pred": whether z-scoring of features is specified
##' \item "nresample": number of resamples
##' }
##' }
##' @examples
##' ## TO BE DONE


## * fx_modelPerf (code)
##' @export
fx_modelPerf <- function(modelOutput, dthresh = 0.5, many = T, perm = F){
    
    # (remove?)
    if (!many) {
        stop('Not supported if parameter "many" == F')
    } else {
        
        # number of models
        n.models <- length(modelOutput)
        
        # derive set of parameters, excluding list of train and test rows (keep train/test rows?)
        params.set <- !(names(modelOutput[[1]]$parameters)=='train.rows'|
                            names(modelOutput[[1]]$parameters)=='test.rows')
        
        # get parameters from first model
        parameters <- modelOutput[[1]][['parameters']][params.set]
        
        # class.levels
        class.levels <- parameters$class.levels
        
        # length of data frame
        nrows.df <- length(modelOutput[[1]]$parameters$train.rows)+
            length(modelOutput[[1]]$parameters$test.rows)
        
        # model types
        regmodels <- c('regression','rf.regression')
        classmodels <- c('svm','rf','logistic')
        
        # regression models
        if(modelOutput[[1]]$parameters$model.type%in%regmodels){
            
            # create df.allfolds data frame
            df.allfolds <- do.call(rbind, lapply(
                lapply(seq(n.models), function(j){
                    return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
                                fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
                                pred.values.covar = modelOutput[[j]]$pred.covar$pred.values,
                                pred.values.full = modelOutput[[j]]$pred.full$pred.values,
                                actual.values = modelOutput[[j]]$pred.full$actual.values))
                }),
                data.frame))
        
        # prediction models
        } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
            
            # create df.allfolds data frame
            df.allfolds <- do.call(rbind, lapply(
                lapply(seq(n.models), function(j){
                    return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
                                fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
                                pred.prob.covar = modelOutput[[j]]$pred.covar$pred.prob,
                                pred.prob.full = modelOutput[[j]]$pred.full$pred.prob,
                                pred.class.covar = modelOutput[[j]]$pred.covar$pred.class,
                                pred.class.full = modelOutput[[j]]$pred.full$pred.class,
                                actual.class = modelOutput[[j]]$pred.full$actual.class))
                }),
                data.frame))
            
            # determine whether predicted class defined for covariate model
            pred.class.covar.not.defined <- all(is.na(df.allfolds$pred.class.covar))
            
            # define predicted class for covariate model (if necessary)
            if(pred.class.covar.not.defined){
                
                df.allfolds$pred.class.covar <- class.levels[as.numeric(df.allfolds$pred.prob.covar>dthresh)+1]
                df.allfolds$pred.class.full <- class.levels[as.numeric(df.allfolds$pred.prob.full>dthresh)+1]
                
            }
            
            # update parameters
            parameters$cmat.descrip <- 'Rows: prediction, Columns: actual'
            parameters$negative.class <- parameters$class.levels[1]
            parameters$positive.class <- parameters$class.levels[2]
            parameters$dthresh <- dthresh
            
        } else {
            
            stop(paste0('Not sure how to handle: ', modelOutput[[1]]$parameters$model.type))
            
        }
        
    }
    
    # determine model performance measures for each fold
    foldPerf <- lapply(c('across',unique(df.allfolds$fold)), function(i){
        
        if(i=='across'){
            
            # take first instance of each index in df.allfolds
            firstrows <- sapply(unique(df.allfolds$orig.df.row), function(j){
                return(which(df.allfolds$orig.df.row==j)[1])
            })
            
            # subset of df.allfolds where all df.allfolds$orig.df.row values are unique
            df.tmp <- df.allfolds[firstrows,]
            
        } else {
            
            # subset of df.allfolds for ith fold
            df.tmp <- df.allfolds[df.allfolds$fold==i,]
        }
        
        # if regression model
        if(parameters$model.type%in%regmodels){
            
            # r-squared (NOTE: THIS IS FROM TRAINING MODEL)
            foldrsq.full <- sapply(seq(n.models), function(j){modelOutput[[j]]$pred.full$rsq[1]})
            
            # mean and root-mean squared error
            if (i == 'across'){
                mse.test <- NA
                g.full <- NA
            } else {
                mse.full.test <- mean((df.tmp$actual.values-df.tmp$pred.values.full)**2)
                
                # see "G" equation on p. 222 in Section 12.4.1.2 of Permutation, Parametric and Bootstrap Tests of Hypotheses by Good P (3rd edition)
                g.full <- mse.full.test/unique(modelOutput[[as.numeric(i)]]$pred.full$mse.train)
            }

            rmse.full <- sqrt(mean((df.tmp$actual.values-df.tmp$pred.values.full)**2))
            
            # if covariate model specified
            if(!is.null(parameters$covar)){
                
                # r-squared (NOTE: THIS IS FROM TRAINING MODEL)
                foldrsq.covar <- sapply(seq(n.models),function(j){modelOutput[[j]]$pred.covar$rsq[1]})
                
                # mean root-mean squared error
                if (i == 'across'){
                    mse.covar.test <- NA
                    g.covar <- NA
                } else {
                    mse.covar.test <- mean((df.tmp$actual.values-df.tmp$pred.values.covar)**2)
                    
                    # see "G" equation on p. 222 in Section 12.4.1.2 of Permutation, Parametric and Bootstrap Tests of Hypotheses by Good P (3rd edition)
                    g.covar <- mse.covar.test/unique(modelOutput[[as.numeric(i)]]$pred.covar$mse.train)
                }
                rmse.covar <- sqrt(mean((df.tmp$actual.values-df.tmp$pred.values.covar)**2))
            
            # no covariate model specified    
            } else {
                
                foldrsq.covar <- rep(NA,n.models)
                rmse.covar <- NA
                g.covar <- NA
                
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
            
            # create perfMetrics data frame
            perfMetrics <- data.frame(rmse.covar = rmse.covar,
                                      rmse.full = rmse.full,
                                      rsq.covar = rsq.covar,
                                      rsq.full = rsq.full,
                                      g.covar = g.covar,
                                      g.full = g.full)
            
            return(list(perfMetrics = perfMetrics, fold = i))
        
        # prediction model    
        } else if(parameters$model.type%in%classmodels){
            
            # confusion matrix
            cmat.full <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
            
            # true negatives
            cmat.full[class.levels[1],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[1])
            # false positives
            cmat.full[class.levels[2],class.levels[1]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[1])
            # false negatives
            cmat.full[class.levels[1],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[2])
            # true positives
            cmat.full[class.levels[2],class.levels[2]] <-
                sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[2])
            
            # covariate model confusion matrix
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
                # NAs if no covariate model
                cmat.covar <- matrix(NA,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
            }
            
            # create perfMetrics data frame
            perfMetrics <- data.frame(TP.covar = cmat.covar[parameters$positive.class, parameters$positive.class],
                                      FP.covar = cmat.covar[parameters$positive.class, parameters$negative.class],
                                      TN.covar = cmat.covar[parameters$negative.class, parameters$negative.class],
                                      FN.covar = cmat.covar[parameters$negative.class, parameters$positive.class])
            perfMetrics$sens.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FN.covar)
            perfMetrics$spec.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FP.covar)
            perfMetrics$ppv.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FP.covar)
            perfMetrics$npv.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FN.covar)
            perfMetrics$acc.covar <- (perfMetrics$TP.covar + perfMetrics$TN.covar)/sum(cmat.covar)
            
            # compute AUC ROC and optimal threshold
            if(!is.null(parameters$covar)){
                
                rocCompute.covar <- fx_rocCompute(pred.prob = df.tmp$pred.prob.covar,
                                                  actual.class = df.tmp$actual.class,
                                                  class.levels = class.levels)
                perfMetrics$auc.ROC.covar <- rocCompute.covar$roc.auc
                perfMetrics$optThresh.covar <- rocCompute.covar$optimal.threshold
                
            } else {
                
                perfMetrics$auc.ROC.covar <- perfMetrics$optThresh.covar <- NA
                
            }
            
            # compute AUC ROC and optimal threshold
            if(!all(is.na(df.tmp$pred.prob.full))){
                
                rocCompute.full <- fx_rocCompute(pred.prob = df.tmp$pred.prob.full,
                                                 actual.class = df.tmp$actual.class,
                                                 class.levels = class.levels)
                
                perfMetrics$auc.ROC.full <- rocCompute.full$roc.auc
                perfMetrics$optThresh.full <- rocCompute.full$optimal.threshold
                
            } else {
                
                perfMetrics$auc.ROC.full <- perfMetrics$optThresh.full <- NA
                
            }
            
            # add full model performance measures to perfMetrics
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
    
    # regression models
    if(modelOutput[[1]]$parameters$model.type%in%regmodels){
        
        perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
        perfMetrics <- as.data.frame(cbind(perfMetrics,do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
        perfMetrics$fold <- as.character(perfMetrics$fold)
        perfMetrics[nrow(perfMetrics)+1,'fold'] <- 'within'
        perfMetrics[nrow(perfMetrics),colnames(perfMetrics)[colnames(perfMetrics)!='fold']] <- 
            unlist(lapply(colnames(perfMetrics)[colnames(perfMetrics)!='fold'], 
                          function(i){mean(perfMetrics[perfMetrics$fold%in%unique(df.allfolds$fold),i],na.rm=T)}))
        cmat.covar <- NULL
        cmat.full <- NULL
    
    # prediction models
    } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
        
        perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
        perfMetrics <- as.data.frame(cbind(perfMetrics, do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
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

## fx_modelPerf <- function(modelOutput, decisionThreshold = 0.5, many = T, perm = F){

##     if (!many) {
        
##     } else {
        
##         nmodels <- length(modelOutput)
##         params.set <- !(names(modelOutput[[1]]$parameters)=='train.rows'|
##             names(modelOutput[[1]]$parameters)=='test.rows')
##         parameters <- modelOutput[[1]][['parameters']][params.set]
##         class.levels <- parameters$class.levels
        
##         nrows.df <- length(modelOutput[[1]]$parameters$train.rows)+
##             length(modelOutput[[1]]$parameters$test.rows)
        
##         regmodels <- c('regression')
##         classmodels <- c('svm','rf','logistic')
##         if(modelOutput[[1]]$parameters$model.type%in%regmodels){
            
##             df.allfolds <- do.call(rbind, lapply(
##                 lapply(seq(nmodels), function(j){
##                     return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
##                                 fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
##                                 pred.values.covar = modelOutput[[j]]$pred.covar$pred.values,
##                                 pred.values.full = modelOutput[[j]]$pred.full$pred.values,
##                                 actual.values = modelOutput[[j]]$pred.full$actual.values))
##                 }),
##                 data.frame))
            
##         } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
            
##             df.allfolds <- do.call(rbind, lapply(
##                 lapply(seq(nmodels), function(j){
                    
                    
##                     return(list(orig.df.row = modelOutput[[j]]$parameters$test.rows,
##                                 fold = rep(j,length(modelOutput[[j]]$parameters$test.rows)),
##                                 pred.prob.covar = modelOutput[[j]]$pred.covar$pred.prob,
##                                 pred.prob.full = modelOutput[[j]]$pred.full$pred.prob,
##                                 pred.class.covar = modelOutput[[j]]$pred.covar$pred.class,
##                                 pred.class.full = modelOutput[[j]]$pred.full$pred.class,
##                                 actual.class = modelOutput[[j]]$pred.full$actual.class))
##                 }),
##                 data.frame))
            
##             if(all(is.na(df.allfolds$pred.class.covar))){
##                 df.allfolds$pred.class.covar <- class.levels[as.numeric(df.allfolds$pred.prob.covar>decisionThreshold)+1]
##                 df.allfolds$pred.class.full <- class.levels[as.numeric(df.allfolds$pred.prob.full>decisionThreshold)+1]
##             }
            
##             parameters$cmat.descrip <- 'Rows: prediction, Columns: actual'
##             parameters$negative.class <- parameters$class.levels[1]
##             parameters$positive.class <- parameters$class.levels[2]
##             parameters$decisionThreshold <- decisionThreshold
            
##         } else {stop(paste0('Not sure how to handle: ', modelOutput[[1]]$parameters$model.type))}
##     }
    
##     foldPerf <- lapply(c('across',unique(df.allfolds$fold)), function(i){

##         if(i=='across'){
##             firstrows <- sapply(unique(df.allfolds$orig.df.row), function(j){
##                 return(which(df.allfolds$orig.df.row==j)[1])
##             })
##             df.tmp <- df.allfolds[firstrows,]
##         } else {
##             df.tmp <- df.allfolds[df.allfolds$fold==i,]
##         }
        
##         if(parameters$model.type%in%regmodels){
            
##             foldrsq.full <- sapply(seq(nmodels), function(j){modelOutput[[j]]$pred.full$rsq[1]})
##             rmse.full <- sqrt(mean((df.tmp$pred.values.full-df.tmp$actual.values)**2))
##             if(!is.null(parameters$covar)){
##                 foldrsq.covar <- sapply(seq(nmodels), function(j){modelOutput[[j]]$pred.covar$rsq[1]})
##                 rmse.covar <- sqrt(mean((df.tmp$pred.values.covar-df.tmp$actual.values)**2))
##             } else {
##                 foldrsq.covar <- rep(NA,seq(nmodels))
##                 rmse.covar <- NA
##             }
            
##             if(i=='across'){
                
##                 rsq.full <- mean(foldrsq.full)
##                 if(!is.null(parameters$covar)){
##                     rsq.covar <- mean(foldrsq.covar)
##                 } else {
##                     rsq.covar <- NA
##                 }
                
##             } else {
                
##                 rsq.full <- foldrsq.full[as.numeric(i)]
##                 if(!is.null(parameters$covar)){
##                     rsq.covar <- foldrsq.covar[as.numeric(i)]
##                 } else {
##                     rsq.covar <- NA
##                 }
##             }
            
##             perfMetrics <- data.frame(rmse.covar = rmse.covar,
##                                       rmse.full = rmse.full,
##                                       rsq.covar = rsq.covar,
##                                       rsq.full = rsq.full)
##             return(list(perfMetrics = perfMetrics, fold = i))
                   
##         } else if(parameters$model.type%in%classmodels){
            
##             cmat.full <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
##             cmat.full[class.levels[1],class.levels[1]] <-
##                 sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[1])
##             cmat.full[class.levels[2],class.levels[1]] <-
##                 sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[1])
##             cmat.full[class.levels[1],class.levels[2]] <-
##                 sum(df.tmp$pred.class.full==class.levels[1] & df.tmp$actual.class==class.levels[2])
##             cmat.full[class.levels[2],class.levels[2]] <-
##                 sum(df.tmp$pred.class.full==class.levels[2] & df.tmp$actual.class==class.levels[2])
            
##             if(!is.null(parameters$covar)){
##                 cmat.covar <- matrix(0,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
##                 cmat.covar[class.levels[1],class.levels[1]] <-
##                     sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[1])
##                 cmat.covar[class.levels[2],class.levels[1]] <-
##                     sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[1])
##                 cmat.covar[class.levels[1],class.levels[2]] <-
##                     sum(df.tmp$pred.class.covar==class.levels[1] & df.tmp$actual.class==class.levels[2])
##                 cmat.covar[class.levels[2],class.levels[2]] <-
##                     sum(df.tmp$pred.class.covar==class.levels[2] & df.tmp$actual.class==class.levels[2])
##             } else {
##                 cmat.covar <- matrix(NA,nrow=2,ncol=2,dimnames=list(class.levels,class.levels))
##             }
            
##             perfMetrics <- data.frame(TP.covar = cmat.covar[parameters$positive.class, parameters$positive.class],
##                                       FP.covar = cmat.covar[parameters$positive.class, parameters$negative.class],
##                                       TN.covar = cmat.covar[parameters$negative.class, parameters$negative.class],
##                                       FN.covar = cmat.covar[parameters$negative.class, parameters$positive.class])
##             perfMetrics$sens.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FN.covar)
##             perfMetrics$spec.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FP.covar)
##             perfMetrics$ppv.covar <- perfMetrics$TP.covar/(perfMetrics$TP.covar + perfMetrics$FP.covar)
##             perfMetrics$npv.covar <- perfMetrics$TN.covar/(perfMetrics$TN.covar + perfMetrics$FN.covar)
##             perfMetrics$acc.covar <- (perfMetrics$TP.covar + perfMetrics$TN.covar)/sum(cmat.covar)
            
##             if(!is.null(parameters$covar)){
##                 rocCompute.covar <- fx_rocCompute(pred.prob = df.tmp$pred.prob.covar,
##                                                   actual.class = df.tmp$actual.class,
##                                                   class.levels = class.levels)
##                 perfMetrics$auc.ROC.covar <- rocCompute.covar$roc.auc
##                 perfMetrics$optThresh.covar <- rocCompute.covar$optimal.threshold
##             } else {
##                 perfMetrics$auc.ROC.covar <- perfMetrics$optThresh.covar <- NA
##             }
            
##             if(!all(is.na(df.tmp$pred.prob.full))){
##                 rocCompute.full <- fx_rocCompute(pred.prob = df.tmp$pred.prob.full,
##                                                   actual.class = df.tmp$actual.class,
##                                                   class.levels = class.levels)
                
##                 perfMetrics$auc.ROC.full <- rocCompute.full$roc.auc
##                 perfMetrics$optThresh.full <- rocCompute.full$optimal.threshold
##             } else {
##                 perfMetrics$auc.ROC.full <- perfMetrics$optThresh.full <- NA
##             }
            
##             perfMetrics$TP.full <- cmat.full[parameters$positive.class, parameters$positive.class]
##             perfMetrics$FP.full <- cmat.full[parameters$positive.class, parameters$negative.class]
##             perfMetrics$TN.full <- cmat.full[parameters$negative.class, parameters$negative.class]
##             perfMetrics$FN.full <- cmat.full[parameters$negative.class, parameters$positive.class]
##             perfMetrics$sens.full <- perfMetrics$TP.full/(perfMetrics$TP.full + perfMetrics$FN.full)
##             perfMetrics$spec.full <- perfMetrics$TN.full/(perfMetrics$TN.full + perfMetrics$FP.full)
##             perfMetrics$ppv.full <- perfMetrics$TP.full/(perfMetrics$TP.full + perfMetrics$FP.full)
##             perfMetrics$npv.full <- perfMetrics$TN.full/(perfMetrics$TN.full + perfMetrics$FN.full)
##             perfMetrics$acc.full <- (perfMetrics$TP.full + perfMetrics$TN.full)/sum(cmat.full)
            
##             return(list(cmat.covar = cmat.covar, cmat.full = cmat.full, perfMetrics = perfMetrics, fold = i))
            
##         }
##     })
    
##     if(modelOutput[[1]]$parameters$model.type%in%regmodels){
        
##         perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
##         perfMetrics <- as.data.frame(cbind(perfMetrics,
##                                            do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
##         perfMetrics$fold <- as.character(perfMetrics$fold)
##         perfMetrics[nrow(perfMetrics)+1,'fold'] <- 'within'
##         perfMetrics[nrow(perfMetrics),colnames(perfMetrics)[colnames(perfMetrics)!='fold']] <- 
##             unlist(lapply(colnames(perfMetrics)[colnames(perfMetrics)!='fold'], 
##                           function(i){mean(perfMetrics[perfMetrics$fold%in%unique(df.allfolds$fold),i],na.rm=T)}))
##         cmat.covar <- NULL
##         cmat.full <- NULL
        
##     } else if(modelOutput[[1]]$parameters$model.type%in%classmodels){
        
##         perfMetrics <- data.frame(fold = unlist(lapply(foldPerf, function(i){i$fold})))
##         perfMetrics <- as.data.frame(cbind(perfMetrics,
##                                            do.call(rbind,lapply(foldPerf, function(i){return(i$perfMetrics)}))))
##         perfMetrics$fold <- as.character(perfMetrics$fold)
##         perfMetrics[nrow(perfMetrics)+1,'fold'] <- 'within'
##         perfMetrics[nrow(perfMetrics),colnames(perfMetrics)[colnames(perfMetrics)!='fold']] <- 
##             unlist(lapply(colnames(perfMetrics)[colnames(perfMetrics)!='fold'], 
##                           function(i){mean(perfMetrics[perfMetrics$fold%in%unique(df.allfolds$fold),i], na.rm=T)}))
##         cmat.covar <- foldPerf[[1]]$cmat.covar
##         cmat.full <- foldPerf[[1]]$cmat.full
        
##     }
    
##     return(list(perfMetrics = perfMetrics, 
##                 parameters = parameters, 
##                 cmat.covar = cmat.covar, 
##                 cmat.full = cmat.full, 
##                 df.allfolds = df.allfolds))
## }
