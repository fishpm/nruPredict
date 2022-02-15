### Break up single data frame into train and test datasets.

fx_sample <- function(df0, partition){
    
    df.train <- df0[partition$train,]
    df.test <- df0[partition$test,]
    
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