## * fx_modelResample (documentation)
##' @title Apply Machine Learning Framework
##' @description Apply machine learning framework to specified dataset
##'
##' @param df0 data frame including all observations (data frame)
##' @param cv.type cross-validation type ('loocv', 'ltocv', 'n-fold', 'numeric') (string)
##' @param covar list of df0 column names for "covariate" (not of specific interest) features (string/list)
##' @param voi list of df0 column names for variables/features of interest (string/list)
##' @param outcome df0 column name for outcome measure to be predicted (string)
##' @param model.type machine learning model ('rf', 'logistic', 'regression', 'rf.regression', 'svm') (string)
##' @param nresample number of resamples (numeric)
##' @param dthresh decision threshold (numeric)
##' @param z.pred standardize predictive features (boolean)
##' @param n.cores number of cores (parallel processes) (numeric/integer)
##' @param balance.col df0 column name used for ensuring balanced columns
##' @param partitions pre-defined train/test partitions
##'
##' @return A list of length five, containing the following elements:
##' \itemize{
##' \item "perfMetrics" Model performance metrics for each individual fold and "across" and "within".
##' \cr "across":   sum or mean of metric across folds
##' \cr "within":   mean of metric across folds

##' \item "cmat.covar": confusion matrix of covariate model (at "dthresh" decision threshold)
##' 
##' \item "cmat.full":    confusion matrix of full model (at "dthresh" decision threshold)
##' 
##' \item "df.allfolds":  data frame for test-related model predictions
##' 
##' \item "parameters": list of relevant specified parameters
##' }
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

## * fx_modelResample (example)
##' @examples
##' #### Generate data ####
##' n <- 100
##' 
##' set.seed(1)
##' group <- factor(sample(c('MDD','HC'),n,replace=T))
##' age <- rnorm(n,25,5)
##' sex <- factor(sample(c('male','female'),n,replace=T))
##' rand.vals1 <- rnorm(n,0,0.75)
##' set.seed(2)
##' rand.vals2 <- rnorm(n,0,0.75)
##' dd <- data.frame(group = group,
##'                  age = age,
##'                  sex = sex,
##'                  f1 = rand.vals1 + as.numeric(group),
##'                  f2 = rand.vals2)
##' 
##' #### MODEL EXAMPLE 1 #####
##' ## covariates
##' covar <- c('age','sex')
##' ## variables of interest
##' voi <- c('f1','f2')
##' ## class outcome
##' y <- 'group'
##'
##' ## resamples and permutations
##' nresample <- 10
##' nperm <- 10
##' n.cores <- 1 ## 10
##' 
##' ## fit classification model
##' modelObj <- fx_modelResample(df0 = dd, 
##'                              cv.type = '5-fold',
##'                              covar = covar, 
##'                              voi = voi,  
##'                              outcome = y,
##'                              model.type = 'rf',
##'                              nresample = nresample, 
##'                              dthresh = 0.5,
##'                              z.pred = F,
##'                              balance.col = y,
##'                              n.cores = n.cores)
##'
##' ## determine overall model performance
##' modelPerfObj <- fx_modelResamplePerf(modelResampleObj = modelObj)
##' ## permutation testing
##' permObj <- fx_perm(df0 = dd, modelObj = modelObj, nperm = nperm, n.cores = n.cores)
##' ## determine permutation test performance
##' permPerfObj <- fx_permPerf(permObj = permObj, modelResamplePerf = modelPerfObj)
##'
##' ## Summary of performance measures based on observed data
##' modelPerfObj$df.summary
##' ## Outcome metrics for each resample
##' modelPerfObj$df.iter
##' ## Summary of permutation test outcomes
##' permPerfObj$df.summary
##' ## Outcome metrics for each permutation
##' permPerfObj$df.iter
##' ## create roc curve plot
##' fx_rocPlot(modelObj = modelObj, modelPerfObj = modelPerfObj, permPerfObj = permPerfObj, title.text = 'My Title')



## * fx_modelResample (code)
##' @export
fx_modelResample <- function(df0, cv.type = NULL, covar = NULL, voi = NULL, outcome = NULL, model.type = NULL,
                             nresample = 1, dthresh = 0.5, z.pred = F, n.cores = 20, balance.col = NULL, partitions = NULL){
    
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
        if(is.null(partitions)){
            partition.list <- fx_partition(df0, type = cv.type, balance.col = balance.col)
        } else {
            partition.list <- partitions[[j]]
        }
        
        # apply machine learning framework
        modelObj <- parallel::mclapply(seq(length(partition.list)), function(i){
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
    modelObj <- parallel::mclapply(seq(length(partition.list)), function(i){
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
