### DESCRIPTION ###

### Apply machine learning framework to specified dataset

# INPUTS:

#   type: cross-validation type
#   nresaple: 
#   balance.col

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
#       "cmat.covar"    confusion matrix covariate model
#       "cmat.full":    description of cross-validation method (e.g., 5-fold)
#       "df.allfolds":  
#   parameters: list of machine learning model parameters
#       "test":         is numeric list, values correspond to rows in df0 assigned to model "test" group
#       "train"         is numeric list, values correspond to rows in df0 assigned to model "train" group
#       "sample.type":  description of cross-validation method (e.g., 5-fold)

fx_modelResample <- function(df0, cv.type = NULL, covar = NULL, voi = NULL, outcome = NULL, model.type = NULL, nresample = 1, dthresh = 0.5, z.pred = F, n.cores = 20, balance.col = NULL){
    
    updateMarks <- seq(from = 0, to = nresample, length.out = 11)
    
    if(cv.type == 'loocv'){
        nresample <- 1
        writeLines('LOOCV - resetting nresamples to 1...')
    } else {
        writeLines('Generating resample results...')
    }
    
    modelResamplePerfObj <- lapply(seq(nresample), function(j){
        
        if (j%in%updateMarks){
            writeLines(paste0('\tResample: ', j, ' (', (j/nresample)*100, '% complete)'))
        }
        
        partition.list <- fx_partition(df0, type = cv.type, balance.col = balance.col)
        modelObj <- mclapply(seq(length(partition.list)), function(i){
            fx_model(fx_sample(df0,partition.list[[i]]), 
                     covar = covar, 
                     voi = voi, 
                     outcome = outcome, 
                     model.type = model.type, 
                     z.pred = z.pred)}, 
            mc.cores = n.cores)
        modelPerfObj <- fx_modelPerf(modelObj, dthresh=dthresh)
        
        # modelPerfObj$df.allfolds <- NULL
        modelPerfObj$parameters <- NULL
        return(modelPerfObj)
        
    })
    
    partition.list <- fx_partition(df0, type = cv.type, balance.col = balance.col)
    modelObj <- mclapply(seq(length(partition.list)), function(i){
        fx_model(fx_sample(df0,partition.list[[i]]), 
                 covar = covar, 
                 voi = voi, 
                 outcome = outcome, 
                 model.type = model.type)}, 
        mc.cores = n.cores)
    modelPerfObj <- fx_modelPerf(modelObj, dthresh=dthresh)
    parameters <- modelPerfObj$parameters
    parameters$z.pred <- z.pred
    parameters$nresample <- nresample

    writeLines('Model fitting completed!')
    return(list(modelResamplePerfObj = modelResamplePerfObj,
                parameters = parameters))
}