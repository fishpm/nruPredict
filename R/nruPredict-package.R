#' @docType package
#' @name nruPredict
#' @aliases nruPredict, nruPredict-package
#'
#' @title title of the package
#' @description
#' Short description of the function meant to be used by the user
#' User visible functions:
#' \item \code{\link{fx_permPerf}}: Estimate p-values, organize null distributions
#' \item \code{\link{fx_permPlot}}: Plot observed vs. null distributions
#' \item \code{\link{fx_roc}}: Estimate and plot ROC
#' \item \code{\link{fx_rocCompute}}: Computer AUC of ROC
#'
#' Core functions (called internally):
#' \item \code{\link{fx_outFile}}: Handles specified output files 
#' \item \code{\link{fx_summary}}: Produces .txt summary of model info (incomplete)
#' \item \code{\link{fx_modelPerf}}: Confusion matrix and model performance metrics
#' \item \code{\link{fx_perm}}: Derive null distribution
#' \item \code{\link{fx_partition}}: List of partitions to apply to data frame
#' \item \code{\link{fx_sample}}: Create train/test sub data frames
#' \item \code{\link{fx_model}}: Train/test model on sub data frames
#'
#' 
#' \itemize{
#' }


#' @import mvtnorm
#' @import parallel
#' @import pROC
#' @import lava
#' @import scales
#' @import ggplot2
#' @import randomForest
#' @import e1071
#' @import zoo
