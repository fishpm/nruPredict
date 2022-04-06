## * fx_sample (documentation)
##' @description Break up single data frame into train and test datasets.
##'
##' @param df0 data frame including all observations (data frame)
##' @param partition list object containing train and test group assignment
##'
##' @return A list of length three,containing subsets of original data frame and model parameters
##' \itemize{
##' \item df.train: data frame containing only rows assigned to train set
##' \item df.test: data frame containing only rows assigned to test set
##' \item parameters: list of relevant specified parameters
##' \itemize{
##' \item "sample.type": cross-validation sampling procedure
##' \item "train.rows": row indices assigned to train set
##' \item "test.rows": row indices assigned to test set
##' \item "data.frame": data frame specified (CURRENTLY NOT CORRECTLY SPECIFIED)
##' \item "nresample": number of resamples
##' \item "balance.col": column to use for balancing groups
##' }
##' }

## * fx_sample (code)
##' @export
fx_sample <- function(df0, partition){
    
    # data frame subsets
    df.train <- df0[partition$train,]
    df.test <- df0[partition$test,]
    
    # model parameters
    parameters <- list(sample.type = partition$sample.type)
    parameters$train.rows <- partition$train
    parameters$test.rows <- partition$test
    parameters$data.frame <- as.character(match.call()$df0)
    
    if (is.numeric(parameters$sample.type)){
        parameters$nresample <- partition$nresample
        parameters$balance.col <- partition$balance.col
    }
    
    return(list(df.train = df.train, 
                df.test = df.test, 
                parameters = parameters))
    
}


## fx_sample <- function(df0, partition){
##     df.train <- df0[partition$train,]
##     df.test <- df0[partition$test,]
##     parameters <- list(sample.type = partition$sample.type)
##     parameters$train.rows <- partition$train
##     parameters$test.rows <- partition$test
##     parameters$data.frame <- as.character(match.call()$df0)
##     if (is.numeric(parameters$sample.type)){
##         parameters$nresample <- partition$nresample
##         parameters$balance.col <- partition$balance.col
##         }
##     return(list(df.train = df.train, 
##                 df.test = df.test, 
##                 parameters = parameters))

