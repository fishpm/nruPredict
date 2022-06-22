## * fx_model (documentation)
##' @title Train/Test Model
##' @description Train/test model on sub data frames
##'
##' @param df.set list containing train and test data frames
##' @param outcome df0 column name for outcome measure to be predicted (string)
##' @param outcome df0 column name for outcome measure to be predicted (string)
##' @param model.type machine learning model ('rf', 'logistic', 'regression', 'rf.regression', 'svm') (string)
##' @param z.pred standardize predictive features (boolean)
##'
##' @return A list of length three, containing the following elements:
##' \itemize{
##' \item "pred.covar":   data frame of predicted values from covariate model
##' \item "pred.full":    data frame of predicted values from full model
##' \item "parameters":   model parameters
##' }
##'

## * fx_model (code)
##' @export
fx_model <- function(df.set, covar = NULL, voi = NULL, outcome = NULL, model.type = 'logistic', z.pred = F){
    
    # available models
    classmodels <- c('logistic', 'rf', 'svm')
    regmodels <- c('regression', 'rf.regression')
    model.set <- c(classmodels,regmodels)
    
    # check that available model specified
    if(!tolower(model.type) %in% model.set){
        stop(paste0('Specify appropriate model type. Choose from: ', paste0(model.set, collapse = ', ')))
    } else {
        model.type <- tolower(model.type)
    }
    
    # build covariate model formula object (if necessary)
    if(!is.null(covar)){
        formula.covar <- as.formula(paste0(outcome, ' ~ ', paste(covar, collapse = '+')))
    } else {
        formula.covar <- NULL
    }
    
    # build full model formula object
    formula.full <- as.formula(paste0(outcome, ' ~ ', paste(c(covar,voi), collapse = '+')))
    
    # regression models
    if(model.type %in% regmodels){
        
        # check that outcome is not a factor
        if (is.factor(df.set$df.train[,outcome])){
            stop('Regression not allowed for factor outcomes')
        }
        class.levels <- NA
    
    # prediction models
    } else {
        
        # ensure that outcome is a factor
        if (!is.factor(df.set$df.train[,outcome])){
            stop(paste0(model.type, ' (classification) not allowed for continuous outcomes'))
        }
        
        # assign class levels
        class.levels <- levels(df.set$df.train[,outcome])
        
    }
    
    # assign parameters
    parameters <- list(sample.type = df.set$parameters$sample.type, 
                       train.rows = df.set$parameters$train.rows, 
                       test.rows = df.set$parameters$test.rows, 
                       class.levels = class.levels,
                       model.type = model.type, 
                       covar = covar, 
                       voi = voi,
                       outcome = outcome, 
                       formula.covar = formula.covar,
                       formula.full = formula.full,
                       data.frame = df.set$parameters$data.frame
                       )
    
    # standardize non-factor features
    if(z.pred){
        
        pred.continuous <- c(covar,voi)[sapply(c(covar,voi), function(i){!is.factor(df.set$df.train[,i])})]
        df.train <- df.set$df.train
        df.test <- df.set$df.test
        
        for(i in pred.continuous){
            elem.mean <- mean(df.set$df.train[,i])
            elem.sd <- sd(df.set$df.train[,i])
            df.train[,i] <- (df.set$df.train[,i]-elem.mean)/elem.sd
            df.test[,i] <- (df.set$df.test[,i]-elem.mean)/elem.sd
        }
    
    # features not standardized
    } else {
        
        df.train <- df.set$df.train
        df.test <- df.set$df.test
        
    }
    
    ## Apply and predict model
    
    # logistic regression
    if (model.type == 'logistic'){
        
        # define model
        model.full <- glm(formula.full, data = df.train, family = 'binomial')
        
        # derive model predicted probabilities
        pred.prob.full <- predict(model.full, newdata = df.test, type = 'resp')
        
        # data frame containing full model predicted class, probability of class membership and actual class
        pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                pred.prob = pred.prob.full,
                                actual.class = as.character(df.set$df.test[,outcome]))
        
        # fit covariate model
        if(!is.null(covar)){
            
            # define model
            model.covar <- glm(formula.covar, data = df.train, family = 'binomial')
            
            # derive model predicted probabilities
            pred.prob.covar <- predict(model.covar, newdata = df.test, type = 'resp')
            
            # data frame containing full model predicted class, probability of class membership and actual class
            pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                     pred.prob = pred.prob.covar,
                                     actual.class = as.character(df.set$df.test[,outcome])
            )
        
        # no covariate model
        } else {
            
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.class <- NA
            pred.covar$pred.prob <- NA
            
        }
    
    # randomForest
    } else if (model.type == 'rf'){
        
        # define model
        model.full <- randomForest::randomForest(formula.full, data = df.train)
        
        # derive model predicted probabilities
        pred.prob.full <- predict(model.full, newdata = df.test, type = 'prob')[,parameters$class.levels[2]]
        
        # data frame containing model predicted class, probability of class membership and actual class
        pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                pred.prob = pred.prob.full,
                                actual.class = as.character(df.set$df.test[,outcome]))
        # fit covariate model
        if(!is.null(covar)){
            
            # define model
            model.covar <- randomForest::randomForest(formula.covar, data = df.train)
            
            # derive model predicted probabilities
            pred.prob.covar <- predict(model.covar, newdata = df.test, type = 'prob')[,parameters$class.levels[2]]
            
            # data frame containing model predicted class, probability of class membership and actual class
            pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                     pred.prob = pred.prob.covar,
                                     actual.class = as.character(df.set$df.test[,outcome]))
        # no covariate model
        } else {
            
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.class <- NA
            pred.covar$pred.prob <- NA
            
        }
    
    # support vector machine
    } else if (model.type == 'svm'){
        
        # define model
        model.full <- svm(formula.full, data = df.train, probability = T)
        
        # derive model predicted probabilities
        pred.prob.full <- attr(predict(model.full, newdata = df.test, probability = T), 'probabilities')[,class.levels[2]]
        
        # data frame containing model predicted class, probability of class membership and actual class
        pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                pred.prob = pred.prob.full,
                                actual.class = as.character(df.set$df.test[,outcome]))
        
        # fit covariate model
        if(!is.null(covar)){
            
            # define model
            model.covar <- svm(formula.covar, data = df.train, probability = T)
            
            # derive model predicted probabilities
            pred.prob.covar <- attr(predict(model.covar, newdata = df.test, probability = T), 'probabilities')[,class.levels[2]]
            
            # data frame containing model predicted class, probability of class membership and actual class
            pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
                                     pred.prob = pred.prob.covar,
                                     actual.class = as.character(df.set$df.test[,outcome]))
        
        # no covariate model
        } else {
            
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.class <- NA
            pred.covar$pred.prob <- NA
            
        }
    
    # linear regression
    } else if (model.type == 'regression'){
        
        # define model
        model.full <- lm(formula.full, data = df.train)
        
        # mean squared-error on observations in df.train
        mse.train <- mean((df.train[,outcome]-predict(model.full, newdata = df.train, type = 'response'))**2)
        # sum(model.covar$residuals**2)/length(model.covar$residuals) <- equivalent to this value
        
        # derive model predicted values
        pred.values.full <- predict(model.full, newdata = df.test)
        
        # data frame containing model predicted values, actual values, model r-squared, mean squared-error
        pred.full <- data.frame(pred.values = pred.values.full,
                                actual.values = df.set$df.test[,outcome],
                                rsq = summary(model.full)$r.squared,
                                mse.train = mse.train)
        
        # fit covariate model
        if(!is.null(covar)){
            
            # define model
            model.covar <- lm(formula.covar, data = df.train)
            
            # mean squared-error on observations in df.train
            mse.train <- mean((df.train[,outcome]-predict(model.covar, newdata = df.train, type = 'response'))**2)
            # sum(model.covar$residuals**2)/length(model.covar$residuals) <- equivalent to this value
            
            # derive model predicted values
            pred.values.covar <- predict(model.covar, newdata = df.test)
            
            # data frame containing model predicted values, actual values, and model r-squared
            pred.covar <- data.frame(pred.values = pred.values.covar,
                                     actual.values = df.set$df.test[,outcome],
                                     rsq = summary(model.covar)$r.squared,
                                     mse.train = mse.train)
        
        # no covariate model
        } else {
            
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.values <- NA
            pred.covar$actual.values <- NA
            pred.covar$rsq <- NA
            pred.covar$mse.train <- NA
            
        }
    
    # randomForest regression
    } else if (model.type == 'rf.regression'){
        
        # define model
        model.full <- randomForest(formula.full, data = df.train)
        
        # mean squared-error on observations in df.train
        mse.train <- mean((df.train[,outcome]-predict(model.full, newdata = df.train, type = 'response'))**2)
        
        # derive model predicted values
        pred.values.full <- predict(model.full, newdata = df.test, type = 'response')
        
        # data frame containing model predicted values, actual values, and model r-squared
        pred.full <- data.frame(pred.values = pred.values.full,
                                actual.values = df.set$df.test[,outcome],
                                rsq = NA,
                                mse.train = mse.train)
        
        # fit covariate model
        if(!is.null(covar)){
            
            # define model    
            model.covar <- randomForest(formula.covar, data = df.train)
            
            # mean squared-error on observations in df.train
            mse.train <- mean((df.train[,outcome]-predict(model.covar, newdata = df.train, type = 'response'))**2)
            
            # derive model predicted values
            pred.values.covar <- predict(model.covar, newdata = df.test, type = 'response')
            
            # data frame containing model predicted values, actual values, and model r-squared
            pred.covar <- data.frame(pred.values = pred.values.covar,
                                     actual.values = df.set$df.test[,outcome],
                                     rsq = NA,
                                     mse.train = mse.train)
        
        # no covariate model
        } else {
            
            model.covar <- NULL
            pred.covar <- pred.full
            pred.covar$pred.values <- NA
            pred.covar$actual.values <- NA
            pred.covar$rsq <- NA
            pred.covar$mse.train <- NA
            
        }
        
    }
    
    # update parameters
    if (is.numeric(parameters$sample.type)){
        
        parameters$nresample <- df.set$parameters$nresample
        parameters$balance.col <- df.set$parameters$balance.col
        
    }
    
    return(list(pred.covar = pred.covar,
                pred.full = pred.full,
                parameters = parameters))
    
}

## fx_model <- function(df.set, covar = NULL, voi = NULL, outcome = NULL, model.type = 'logistic', z.pred = T){
    
##     classmodels <- c('logistic', 'rf', 'svm')
##     regmodels <- c('regression')
##     model.set <- c(classmodels,regmodels)
##     if(!tolower(model.type) %in% model.set){
##         stop(paste0('Specify appropriate model type. Choose from: ', paste0(model.set, collapse = ', ')))
##     } else {model.type <- tolower(model.type)}
    
##     if(!is.null(covar)){
##         formula.covar <- as.formula(paste0(
##             outcome, ' ~ ', paste(covar, collapse = '+')))
##     } else {
##         formula.covar <- NULL
##     }
    
##     formula.full <- as.formula(paste0(
##         outcome, ' ~ ', paste(c(covar,voi), collapse = '+')))
    
##     if(model.type=='regression'){
##         if (is.factor(df.set$df.train[,outcome])){
##             stop('Regression not allowed for factor outcomes')
##         }
##         class.levels <- NA
##     } else {
##         if (!is.factor(df.set$df.train[,outcome])){
##             stop(paste0(model.type, ' (classification) not allowed for continuous outcomes'))
##         }
##         class.levels <- levels(df.set$df.train[,outcome])
##     }
    
##     parameters <- list(sample.type = df.set$parameters$sample.type, 
##                        train.rows = df.set$parameters$train.rows, 
##                        test.rows = df.set$parameters$test.rows, 
##                        class.levels = class.levels,
##                        model.type = model.type, 
##                        covar = covar, 
##                        voi = voi,
##                        outcome = outcome, 
##                        formula.covar = formula.covar,
##                        formula.full = formula.full,
##                        data.frame = df.set$parameters$data.frame
##                        )
    
##     # standardize non-factor predictor variables
##     if(z.pred){
        
##         pred.continuous <- c(covar,voi)[sapply(c(covar,voi), function(i){!is.factor(df.set$df.train[,i])})]
##         df.train <- df.set$df.train
##         df.test <- df.set$df.test
##         for(i in pred.continuous){
##             elem.mean <- mean(df.set$df.train[,i])
##             elem.sd <- sd(df.set$df.train[,i])
##             df.train[,i] <- (df.set$df.train[,i]-elem.mean)/elem.sd
##             df.test[,i] <- (df.set$df.test[,i]-elem.mean)/elem.sd
##         }
##     } else {
##         df.train <- df.set$df.train
##         df.test <- df.set$df.test
##         }
    
##     # Apply and predict model
##     if (model.type == 'logistic'){
        
##         model.full <- glm(formula.full, data = df.train, family = 'binomial')
##         pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
##                                 pred.prob = predict(model.full, newdata = df.test, type = 'resp'),
##                                 actual.class = as.character(df.set$df.test[,outcome])
##         )
        
##         if(!is.null(covar)){
##             model.covar <- glm(formula.covar, data = df.train, family = 'binomial')
##             pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
##                                      pred.prob = predict(model.covar, newdata = df.test, type = 'resp'),
##                                      actual.class = as.character(df.set$df.test[,outcome])
##             )
##         } else {
##             model.covar <- NULL
##             pred.covar <- pred.full
##             pred.covar$pred.class <- NA
##             pred.covar$pred.prob <- NA
##         }
        
        
        
##     } else if (model.type == 'rf'){
        
##         model.full <- randomForest(formula.full, data = df.train)
##         pred.full <- data.frame(pred.class = rep(NA, nrow(df.test)),
##                                 pred.prob = predict(model.full, 
##                                                     newdata = df.test, 
##                                                     type = 'prob')[,parameters$class.levels[2]],
##                                 actual.class = as.character(df.set$df.test[,outcome]))
        
##         if(!is.null(covar)){
##             model.covar <- randomForest(formula.covar, data = df.train)
##             pred.covar <- data.frame(pred.class = rep(NA, nrow(df.test)),
##                                      pred.prob = predict(model.covar, 
##                                                          newdata = df.test, 
##                                                          type = 'prob')[,parameters$class.levels[2]],
##                                      actual.class = as.character(df.set$df.test[,outcome]))
##         } else {
##             model.covar <- NULL
##             pred.covar <- pred.full
##             pred.covar$pred.class <- NA
##             pred.covar$pred.prob <- NA
##         }
        
        
        
##     } else if (model.type == 'svm'){
        
##         model.full <- svm(formula.full, data = df.train)
##         pred.full <- data.frame(pred.class = as.character(predict(model.full, newdata = df.test)),
##                                 pred.prob = rep(NA, nrow(df.test)),
##                                 actual.class = as.character(df.set$df.test[,outcome]))
        
##         if(!is.null(covar)){
##             model.covar <- svm(formula.covar, data = df.train)
##             pred.covar <- data.frame(pred.class = as.character(predict(model.covar, newdata = df.test)),
##                                      pred.prob = rep(NA, nrow(df.test)),
##                                      actual.class = as.character(df.set$df.test[,outcome]))
##         } else {
##             model.covar <- NULL
##             pred.covar <- pred.full
##             pred.covar$pred.class <- NA
##             pred.covar$pred.prob <- NA
##         }
        
##     } else if (model.type == 'regression'){
        
##         model.full <- lm(formula.full, data = df.train)
##         pred.full <- data.frame(pred.values = predict(model.full, newdata = df.test),
##                                 actual.values = df.set$df.test[,outcome],
##                                 rsq = summary(model.full)$r.squared)
        
##         if(!is.null(covar)){
##             model.covar <- lm(formula.covar, data = df.train)
##             pred.covar <- data.frame(pred.values = predict(model.covar, newdata = df.test),
##                                      actual.values = df.set$df.test[,outcome],
##                                      rsq = summary(model.covar)$r.squared)
##         } else {
##             model.covar <- NULL
##             pred.covar <- pred.full
##             pred.covar$pred.values <- NA
##             pred.covar$actual.values <- NA
##             pred.covar$rsq <- NA
##         }
        
##     }
    
##     if (is.numeric(parameters$sample.type)){
##         parameters$nresample <- df.set$parameters$nresample
##         parameters$balance.col <- df.set$parameters$balance.col
##     }
##     return(list(pred.covar = pred.covar,
##                 pred.full = pred.full,
##                 parameters = parameters))
## }
