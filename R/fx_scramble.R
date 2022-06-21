## * fx_scramble (documentation)
##' @title Scrambled Group Assignment
##' @description Create data frame with scrambled group assignment
##'
##' @param df0 ???
##' @param outcome ???
##' @param resample ???
##' @param outcome.class ???

## * fx_scramble (code)
##' @export
fx_scramble <- function(df0, outcome, resample = F, outcome.class = T){
    if(is.null(outcome)){
        stop('Specify variable for scrambling')
    } else {
        if(!outcome%in%colnames(df0)){
            stop('Specified outcome variable (', outcome, ') not in data.frame')
        }
    }
    if(!resample){
        df0[,outcome] <- sample(df0[,outcome])
    } else {
        if(outcome.class){
            boot_sample <- sample(unlist(lapply(levels(df0[,outcome]), function(i){
                sample(which(df0[,outcome]==i), replace = T)
            }))) # outer resample randomizes wrt partitionList
            df0 <- df0[boot_sample,]
        } else {
            boot_sample <- sample(nrow(df0), replace = T)
            df0 <- df0[boot_sample,]
        }
    }
    
    return(df0)
}

## fx_scramble <- function(df0, outcome){
    
##     if(is.null(outcome)){
##         stop('Specify variable for scrambling')
##     } else {
##         if(!outcome%in%colnames(df0)){
##             stop('Specified outcome variable (', outcome, ') not in data.frame')
##         }
##     }
##     df0[,outcome] <- sample(df0[,outcome])
##     return(df0)
## }
