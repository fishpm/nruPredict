### DESCRIPTION ###

### Derive summary measures of model performance

# INPUTS:

#   modelOutput:
#   dthresh:
#   many:
#   perm:

# OUTPUTS:
#   A list of length five, containing the following elements:
#   "perfMetrics":  Model performance metrics for each individual fold and "across" and "within"
#                   "across":   sum or mean of metric across folds
#                   "within":   mean of metric across folds
#                   {TP: true positive, FP: false positive, TN: true negative, FN: false negative, sens: sensitivity, spec: specificity, ppv: positive predictive value, npv: negative predictive value, acc: accuracy, auc.ROC: area under the curve of ROC curve, optThresh: optimal decision threshold determined from training data}
#   "cmat.covar":   confusion matrix of covariate model (at "dthresh" decision threshold)
#   "cmat.full":    confusion matrix of full model (at "dthresh" decision threshold)
#   "df.allfolds":  data frame for test-related model predictions
#                   {orig.df.row: row in original data frame for specific observation, fold: fold assignment, pred.prob.covar: predicted probability of class membership from covariate model, pred.prob.full: predicted probability of class membership from full model, pred.class.covar: predicted class from covariate model, pred.class.full: predicted class from full model, actual.class: actual class membership}
#   "parameters": list of relevant specified parameters
#       "sample.type": cross-validation sampling procedure
#       "class.levels": class levels
#       "model.type": machine learning model framework
#       "covar": specified covariates
#       "voi": specified variables of interest
#       "outcome": name of class being predicted
#       "formula.covar": formula object for covariate model
#       "formula.full": formula object for full model
#       "data.frame": data frame specified (CURRENTLY NOT CORRECTLY SPECIFIED)
#       "cmat.descrip": key for how to understand confusion matrices ()
#       "negative.class": class assigned to probability = 0
#       "positive.class": class assigned to probability = 1
#       "dthresh": decision threshold
#       "z.pred": whether z-scoring of features is specified
#       "nresample": number of resamples

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
            
            # root mean squared error
            rmse.full <- sqrt(mean((df.tmp$pred.values.full-df.tmp$actual.values)**2))
            
            # if covariate model specified
            if(!is.null(parameters$covar)){
                
                # r-squared (NOTE: THIS IS FROM TRAINING MODEL)
                foldrsq.covar <- sapply(seq(n.models),function(j){modelOutput[[j]]$pred.covar$rsq[1]})
                
                # root mean squared error
                rmse.covar <- sqrt(mean((df.tmp$pred.values.covar-df.tmp$actual.values)**2))
            
            # no covariate model specified    
            } else {
                
                foldrsq.covar <- rep(NA,n.models)
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
            
            # create perfMetrics data frame
            perfMetrics <- data.frame(rmse.covar = rmse.covar,
                                      rmse.full = rmse.full,
                                      rsq.covar = rsq.covar,
                                      rsq.full = rsq.full)
            
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