## * fx_plot (documentation)
##' @description Plot observed vs. null/bootstrap distributions

## * fx_plot (code)
##' @export
fx_plot <- function(perfObj, outFile = NULL){
    
    obj.type <- if('nperm'%in%names(perfObj$parameters)){
        obj.type <- 'perm'
    } else if('nboot'%in%names(perfObj$parameters)){
        obj.type <- 'boot'
    } else {stop('Cannot identify object type.')}
    
    regmodels <- c('regression')
    classmodels <- c('svm','rf','logistic')
    if(perfObj$parameters$model.type%in%regmodels){
        measures <- colnames(perfObj$df.iter)[colnames(perfObj$df.iter)!='fold']
    } else if(perfObj$parameters$model.type%in%classmodels){
        measures <- c("sens.covar", "spec.covar", "acc.covar", "auc.ROC.covar", "sens.full", "spec.full", "acc.full", "auc.ROC.full")
    }
    
    if(!is.null(outFile)){
        pdf(fx_outFile(outFile))
        writeLines(paste0('Plots being written to: ', fx_outFile(outFile)))
    }
    
    plots <- list()
    for (measure in measures){
        
        if(obj.type == 'perm'){
            
            subtext <- paste0('obs: ', signif(perfObj$df.pval['obs', measure],3),
                              ', p = ', signif(perfObj$df.pval['pval', measure],3))
            
        } else if(obj.type == 'boot'){
            
            if(grepl('^(auc.ROC)', measure)){
                pval <- signif(ecdf(perfObj$df.iter[[measure]])(0.5),3)
            } else {
                pval <- 'NA'
            }
            
            subtext <- paste0('obs: ', signif(perfObj$df.pval['obs', measure],3),
                              ' [', signif(perfObj$df.pval['2.5%', measure],3),
                              '; ', signif(perfObj$df.pval['97.5%', measure],3),
                              '], boot-p = ', pval)
            
        }
        
        captext <- paste0('N ', obj.type, ' = ', nrow(perfObj$df.iter))
        
        if(!grepl('^(auc.ROC)', measure)){
            captextEnd <- paste0('; dec. thresh = ', perfObj$parameters$decisionThreshold)
        } else {
            captextEnd <- NULL
        }
        
        if(perfObj$parameters$nkfcv){
            captext <- paste0(captext, 
                              '; nkfcv = ', perfObj$parameters$nkfcv,
                              captextEnd)
            
            perfValRange <- quantile(sapply(seq(length(mpo)), function(i){mpo[[i]]$accuracy}),probs = c(0.025, 0.975))
        } else if(is.numeric(perfObj$parameters$sample.type)){
            captext <- paste0(captext,
                              '; train group size = ', perfObj$parameters$sample.type,
                              '; nresamples = ', perfObj$parameters$nresample,
                              captextEnd)
        } else {
            captext <- paste0(captext, 
                              captextEnd)
        }
        
        if(perfObj$parameters$model.type%in%regmodels){
            plots[[length(plots)+1]] <-
                ggplot2::ggplot(data = perfObj$df.iter, ggplot2::aes_string(x=measure)) +
                ggplot2::geom_histogram(fill = 'darkblue') +
                ggplot2::geom_vline(data = perfObj$df.pval['obs',], ggplot2::aes_string(xintercept=measure),
                                    color = 'darkorange', linetype = 'dashed', size = 2) +
                ggplot2::scale_y_continuous(name='Frequency') +
                ggplot2::labs(title = measure,
                              subtitle = subtext,
                              caption = captext) +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                               plot.subtitle = ggplot2::element_text(hjust = 0.5),
                               plot.caption = ggplot2::element_text(hjust = 0.5))
            
        } else if(perfObj$parameters$model.type%in%classmodels){
            plots[[length(plots)+1]] <-
                ggplot2::ggplot(data = perfObj$df.iter, ggplot2::aes_string(x=measure)) +
                ggplot2::geom_histogram(fill = 'darkblue') +
                ggplot2::geom_vline(data = perfObj$df.pval['obs',], ggplot2::aes_string(xintercept=measure),
                                    color = 'darkorange', linetype = 'dashed', size = 2) +
                ggplot2::scale_x_continuous(limits=c(0,1)) +
                ggplot2::scale_y_continuous(name='Frequency') +
                ggplot2::labs(title = measure,
                              subtitle = subtext,
                              caption = captext) +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                               plot.subtitle = ggplot2::element_text(hjust = 0.5),
                               plot.caption = ggplot2::element_text(hjust = 0.5))
        }
        
    }
    
    suppressMessages(print(plots))
    
    if (!is.null(outFile)){
        writeLines(paste0('Output file written: ', fx_outFile(outFile)))
        dev.off()
    }
    
}


## fx_plot <- function(perfObj, outFile = NULL){

##     obj.type <- if('nperm'%in%names(perfObj$parameters)){
##         obj.type <- 'perm'
##     } else if('nboot'%in%names(perfObj$parameters)){
##         obj.type <- 'boot'
##     } else {stop('Cannot identify object type.')}

##     regmodels <- c('regression')
##     classmodels <- c('svm','rf','logistic')
##     if(perfObj$parameters$model.type%in%regmodels){
##         measures <- colnames(perfObj$df.iter)[colnames(perfObj$df.iter)!='fold']
##     } else if(perfObj$parameters$model.type%in%classmodels){
##         measures <- c("sens.covar", "spec.covar", "acc.covar", "auc.ROC.covar", "sens.full", "spec.full", "acc.full", "auc.ROC.full")
##     }

##     if(!is.null(outFile)){
##         pdf(fx_outFile(outFile))
##         writeLines(paste0('Plots being written to: ', fx_outFile(outFile)))
##     }

##     plots <- list()
##     for (measure in measures){

##         if(obj.type == 'perm'){
            
##             subtext <- paste0('obs: ', signif(perfObj$df.pval['obs', measure],3),
##                    ', p = ', signif(perfObj$df.pval['pval', measure],3))
            
##         } else if(obj.type == 'boot'){
            
##             if(grepl('^(auc.ROC)', measure)){
##                 pval <- signif(ecdf(perfObj$df.iter[[measure]])(0.5),3)
##             } else {
##                 pval <- 'NA'
##             }
            
##             subtext <- paste0('obs: ', signif(perfObj$df.pval['obs', measure],3),
##                               ' [', signif(perfObj$df.pval['2.5%', measure],3),
##                               '; ', signif(perfObj$df.pval['97.5%', measure],3),
##                               '], boot-p = ', pval)
            
##         }
        
##         captext <- paste0('N ', obj.type, ' = ', nrow(perfObj$df.iter))
        
##         if(!grepl('^(auc.ROC)', measure)){
##             captextEnd <- paste0('; dec. thresh = ', perfObj$parameters$decisionThreshold)
##         } else {
##             captextEnd <- NULL
##         }

##         if(perfObj$parameters$nkfcv){
##             captext <- paste0(captext, 
##                               '; nkfcv = ', perfObj$parameters$nkfcv,
##                               captextEnd)
            
##             perfValRange <- quantile(sapply(seq(length(mpo)), function(i){mpo[[i]]$accuracy}),probs = c(0.025, 0.975))
##         } else if(is.numeric(perfObj$parameters$sample.type)){
##             captext <- paste0(captext,
##                               '; train group size = ', perfObj$parameters$sample.type,
##                               '; nresamples = ', perfObj$parameters$nresample,
##                               captextEnd)
##         } else {
##             captext <- paste0(captext, 
##                               captextEnd)
##         }

##         if(perfObj$parameters$model.type%in%regmodels){
##             plots[[length(plots)+1]] <-
##                 ggplot(data = perfObj$df.iter, aes_string(x=measure)) +
##                 geom_histogram(fill = 'darkblue') +
##                 geom_vline(data = perfObj$df.pval['obs',], aes_string(xintercept=measure),
##                            color = 'darkorange', linetype = 'dashed', size = 2) +
##                 scale_y_continuous(name='Frequency') +
##                 labs(title = measure,
##                      subtitle = subtext,
##                      caption = captext) +
##                 theme(plot.title = element_text(hjust = 0.5),
##                       plot.subtitle = element_text(hjust = 0.5),
##                       plot.caption = element_text(hjust = 0.5))

##         } else if(perfObj$parameters$model.type%in%classmodels){
##             plots[[length(plots)+1]] <-
##                 ggplot(data = perfObj$df.iter, aes_string(x=measure)) +
##                 geom_histogram(fill = 'darkblue') +
##                 geom_vline(data = perfObj$df.pval['obs',], aes_string(xintercept=measure),
##                            color = 'darkorange', linetype = 'dashed', size = 2) +
##                 scale_x_continuous(limits=c(0,1)) +
##                 scale_y_continuous(name='Frequency') +
##                 labs(title = measure,
##                      subtitle = subtext,
##                      caption = captext) +
##                 theme(plot.title = element_text(hjust = 0.5),
##                       plot.subtitle = element_text(hjust = 0.5),
##                       plot.caption = element_text(hjust = 0.5))
##         }

##     }

##     suppressMessages(print(plots))

##     if (!is.null(outFile)){
##         writeLines(paste0('Output file written: ', fx_outFile(outFile)))
##         dev.off()
##     }

## }



