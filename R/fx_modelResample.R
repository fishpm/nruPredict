### DESCRIPTION ###

### Apply machine learning framework to specified dataset

# INPUTS:
#   df0: data frame including all observations (data frame)
#   cv.type: cross-validation type ('loocv', 'ltocv', 'n-fold', 'numeric') (string)
#   covar: list of df0 column names for "covariate" (not of specific interest) features (string/list)
#   voi: list of df0 column names for variables/features of interest (string/list)
#   outcome: df0 column name for outcome measure to be predicted (string)
#   model.type: machine learning model ('rf', 'logistic', 'regression', 'rf.regression', 'svm') (string)
#   nresample: number of resamples (numeric)
#   dthresh: decision threshold (numeric)
#   z.pred: standardize predictive features (boolean)
#   n.cores: number of cores (parallel processes) (numeric/integer)
#   balance.col: df0 column name used for ensuring balanced columns

# OUTPUTS:
#   A list of length two, containing the following elements:
#   modelResamplePerfObj: list of length "nresample" containing model information.
#       Each list element contains four data frames
#       "perfMetrics":  Model performance metrics for each individual fold and "across" and "within"
#                       "across":   sum or mean of metric across folds
#                       "within":   mean of metric across folds
#                       {TP: true positive, FP: false positive, TN: true negative, FN: false negative, sens: sensitivity, spec: specificity, ppv: positive predictive value, npv: negative predictive value, acc: accuracy, auc.ROC: area under the curve of ROC curve, optThresh: optimal decision threshold determined from training data}
#       "cmat.covar":   confusion matrix of covariate model (at "dthresh" decision threshold)
#       "cmat.full":    confusion matrix of full model (at "dthresh" decision threshold)
#       "df.allfolds":  data frame for test-related model predictions
#                       {orig.df.row: row in original data frame for specific observation, fold: fold assignment, pred.prob.covar: predicted probability of class membership from covariate model, pred.prob.full: predicted probability of class membership from full model, pred.class.covar: predicted class from covariate model, pred.class.full: predicted class from full model, actual.class: actual class membership}
#   parameters: list of relevant specified parameters
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

fx_modelResample <- function(df0, cv.type = NULL, covar = NULL, voi = NULL, outcome = NULL, model.type = NULL, nresample = 1, dthresh = 0.5, z.pred = F, n.cores = 20, balance.col = NULL){
    
    # For visual update on progress
    updateMarks <- seq(from = 0, to = nresample, length.out = 11)
    
    # only one resample if loocv
    if(cv.type == 'loocv'){
        nresample <- 1
        writeLines('LOOCV - resetting nresamples to 1...')
    } else {
        writeLines('Generating resample results...')
    }
    
    # fit model object for each resample
    modelResamplePerfObj <- lapply(seq(nresample), function(j){
        
        # update on progress
        if (j%in%updateMarks){
            writeLines(paste0('\tResample: ', j, ' (', (j/nresample)*100, '% complete)'))
        }
        
        # partition data in to folds
        partition.list <- fx_partition(df0, type = cv.type, balance.col = balance.col)
        
        # apply machine learning framework
        modelObj <- mclapply(seq(length(partition.list)), function(i){
            fx_model(fx_sample(df0,partition.list[[i]]), 
                     covar = covar, 
                     voi = voi, 
                     outcome = outcome, 
                     model.type = model.type, 
                     z.pred = z.pred)}, 
            mc.cores = n.cores)
        
        # summarize model performance
        modelPerfObj <- fx_modelPerf(modelObj, dthresh=dthresh)
        
        # parameters saved only once and as it's own list element
        modelPerfObj$parameters <- NULL
        
        return(modelPerfObj)
        
    })
    
    # run model once to extract parameter information
    partition.list <- fx_partition(df0, type = cv.type, balance.col = balance.col)
    modelObj <- mclapply(seq(length(partition.list)), function(i){
        fx_model(fx_sample(df0,partition.list[[i]]), 
                 covar = covar, 
                 voi = voi, 
                 outcome = outcome, 
                 model.type = model.type)}, 
        mc.cores = n.cores)
    modelPerfObj <- fx_modelPerf(modelObj, dthresh=dthresh)
    
    # update parameter information
    parameters <- modelPerfObj$parameters
    parameters$z.pred <- z.pred
    parameters$nresample <- nresample

    writeLines('Model fitting completed!')
    
    return(list(modelResamplePerfObj = modelResamplePerfObj,
                parameters = parameters))
}