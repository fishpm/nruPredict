## * fx_outFile (documentation)
##' @title Handles Specified Output Files
##' @description Handles specified output files
##'
##' @param outFile ???
##' @param ext.default ???

## * fx_outFile (code)
##' @export
fx_outFile <- function(outFile, ext.default = NULL){
    
    fname <- basename(outFile)
    dname <- dirname(outFile)
    if (!dir.exists(dname)){
        stop(paste0('Folder not found: ', dname, ''))
    }
    ext <- strsplit(fname, split="\\.")[[1]]
    if(!length(ext[-1]) & !is.null(ext.default)){
        fname <- paste0(fname, ext.default)
    } else if (!length(ext[-1]) & is.null(ext.default)){
        ext.default <- '.pdf'
        fname <- paste0(fname, ext.default)
    }
    
    outFileNew <- paste(dname,fname,sep='/')
    return(outFileNew)
}

## fx_outFile <- function(outFile, ext.default = NULL){
    
##     fname <- basename(outFile)
##     dname <- dirname(outFile)
##     if (!dir.exists(dname)){
##         stop(paste0('Folder not found: ', dname, ''))
##     }
##     ext <- strsplit(fname, split="\\.")[[1]]
##     if(!length(ext[-1]) & !is.null(ext.default)){
##         fname <- paste0(fname, ext.default)
##     } else if (!length(ext[-1]) & is.null(ext.default)){
##         ext.default <- '.pdf'
##         fname <- paste0(fname, ext.default)
##     }
    
##     outFileNew <- paste(dname,fname,sep='/')
##     return(outFileNew)
## }
