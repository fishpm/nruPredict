### DESCRIPTION ###

### Break up single data frame into train and test datasets.

# INPUTS:
#   df0: data frame including all observations (data frame)
#   partition: list object containing train and test group assignment

# OUTPUTS:
#   A list of length three,containing subsets of original data frame and model parameters
#   df.train: data frame containing only rows assigned to train set
#   df.test: data frame containing only rows assigned to test set
#   parameters: list of relevant specified parameters
#       "sample.type": cross-validation sampling procedure
#       "train.rows": row indices assigned to train set
#       "test.rows": row indices assigned to test set
#       "data.frame": data frame specified (CURRENTLY NOT CORRECTLY SPECIFIED)
#       "nresample": number of resamples
#       "balance.col": column to use for balancing groups

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