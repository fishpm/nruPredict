### Scramble data frame


fx_scramble <- function(df0, outcome){
    
    if(is.null(outcome)){
        stop('Specify variable for scrambling')
    } else {
        if(!outcome%in%colnames(df0)){
            stop('Specified outcome variable (', outcome, ') not in data.frame')
        }
    }
    df0[,outcome] <- sample(df0[,outcome])
    return(df0)
}