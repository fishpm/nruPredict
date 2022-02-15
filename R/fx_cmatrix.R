## DEPRECATED
### Generate confusion matrix

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
