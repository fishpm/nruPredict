## * fx_rocPlot (documentation)
##' @description Create AUC-ROC plots
##'
##' @param modelObj model object, containing observed model performances
##' @param modelPerfObj object with summary measures of observed model performance
##' @param permPerfObj object with summary measures of permutation model performance
##' @param model.type string for title of ROC curve plot (string)
##'
##' @return ggplot object of ROC curve

## * fx_rocPlot (code)
##' @export
fx_rocPlot <- function(modelObj = NULL, modelPerfObj = NULL, permPerfObj = NULL, title.text = NULL){
    
    if(is.null(modelObj)){
        stop('modelObj not specified.')
    }
    
    writeLines('Preparing ROC curve plots...')
    
    # assign classes
    negClass <- modelObj$parameters$negative.class
    posClass <- modelObj$parameters$positive.class
    class.levels <- modelObj$parameters$class.levels
    
    # steps
    thresh.set <- seq(0,1,by=0.05)
    
    nresamples <- length(modelObj$modelResamplePerfObj)
    nresamples.seq <- seq(nresamples)
    
    # data framce for roc curve generation
    dd.cv <- data.frame(resample = rep(nresamples.seq, each = length(thresh.set)),
                        threshold = rep(thresh.set, nresamples))
    
    covar.is.null <- all(is.na(unlist(lapply(nresamples.seq, function(nres){
        modelObj$modelResamplePerfObj[[nres]]$df.allfolds$pred.prob.covar
    }))))
    
    if(!covar.is.null){
        # compute tpr and fpr at all steps for covariate model
        covar.out <- lapply(nresamples.seq, function(nres){
            out <- t(sapply(thresh.set, function(currThresh){
                if(currThresh == 0){
                    tpr <- 1
                    fpr <- 1
                } else if(currThresh == 1){
                    tpr <- 0
                    fpr <- 0
                } else {
                    covar.prob <- modelObj$modelResamplePerfObj[[nres]]$df.allfolds$pred.prob.covar
                    covar.bin <- as.numeric(covar.prob > currThresh) + 1
                    currGuess <- class.levels[covar.bin]
                    actualClass <- modelObj$modelResamplePerfObj[[nres]]$df.allfolds$actual.class
                    tp <- sum(currGuess == posClass & actualClass == posClass) # true positive
                    tn <- sum(currGuess == negClass & actualClass == negClass) # true negative
                    fp <- sum(currGuess == posClass & actualClass == negClass) # false positive
                    fn <- sum(currGuess == negClass & actualClass == posClass) # false negative
                    tpr <- tp/(tp+fn) # true positive rate
                    tnr <- tn/(tn+fp) # true negative rate
                    if(any(is.infinite(c(tpr,tnr)))){
                        stop('tpr or tnr is infinite.')
                    }
                    fpr <- 1 - tnr # false positive rate == 1 - tnr
                }
                return(c(tpr,fpr))
            }))
            colnames(out) <- c('tpr','fpr')
            return(list(tpr=out[,'tpr'],fpr=out[,'fpr']))
        })
    } else {
        covar.out <- NULL
    }
    
    full.is.null <- all(is.na(unlist(lapply(nresamples.seq, function(nres){
        modelObj$modelResamplePerfObj[[nres]]$df.allfolds$pred.prob.full
    }))))
    if(full.is.null){stop('Full model has no predicted probabilities...')}

    # compute tpr and fpr at all steps for full model
    full.out <- lapply(nresamples.seq, function(nres){
        out <- t(sapply(thresh.set, function(currThresh){
            if(currThresh == 0){
                tpr <- 1
                fpr <- 1
            } else if(currThresh == 1){
                tpr <- 0
                fpr <- 0
            } else {
                full.prob <- modelObj$modelResamplePerfObj[[nres]]$df.allfolds$pred.prob.full
                full.bin <- as.numeric(full.prob > currThresh) + 1
                currGuess <- class.levels[full.bin]
                actualClass <- modelObj$modelResamplePerfObj[[nres]]$df.allfolds$actual.class
                tp <- sum(currGuess == posClass & actualClass == posClass) # true positive
                tn <- sum(currGuess == negClass & actualClass == negClass) # true negative
                fp <- sum(currGuess == posClass & actualClass == negClass) # false positive
                fn <- sum(currGuess == negClass & actualClass == posClass) # false negative
                tpr <- tp/(tp+fn) # true positive rate
                tnr <- tn/(tn+fp) # true negative rate
                if(any(is.infinite(c(tpr,tnr)))){stop('tpr or tnr is infinite.')}
                fpr <- 1 - tnr # false positive rate == 1 - tnr
            }
            return(c(tpr,fpr))
        }))
        colnames(out) <- c('tpr','fpr')
        return(list(tpr=out[,'tpr'],fpr=out[,'fpr']))
    })
    
    
    # assign values to dd.cv
    if(!is.null(covar.out)){
        dd.cv$tpr.covar <- as.vector(sapply(nresamples.seq, function(nres){
            covar.out[[nres]]$tpr
        }))
        dd.cv$fpr.covar <- as.vector(sapply(nresamples.seq, function(nres){
            covar.out[[nres]]$fpr
        }))
    } else {
        dd.cv$tpr.covar <- NA
        dd.cv$fpr.covar <- NA
    }
    
    dd.cv$tpr.full <- as.vector(sapply(nresamples.seq, function(nres){
        full.out[[nres]]$tpr
    }))
    dd.cv$fpr.full <- as.vector(sapply(nresamples.seq, function(nres){
        full.out[[nres]]$fpr
    }))
    
    # summary values for plots
    dd.cv.summary <- data.frame(threshold = thresh.set)
    summary.vals <- t(sapply(thresh.set, function(j){
        
        covar.tpr.mean <- mean(dd.cv$tpr.covar[dd.cv$threshold==j])
        full.tpr.mean <- mean(dd.cv$tpr.full[dd.cv$threshold==j])
        
        covar.tpr.lwr <- covar.tpr.mean - sd(dd.cv$tpr.covar[dd.cv$threshold==j])
        covar.tpr.upr <- covar.tpr.mean + sd(dd.cv$tpr.covar[dd.cv$threshold==j])
        full.tpr.lwr <- full.tpr.mean - sd(dd.cv$tpr.full[dd.cv$threshold==j])
        full.tpr.upr <- full.tpr.mean + sd(dd.cv$tpr.full[dd.cv$threshold==j])
        
        covar.fpr.mean <- mean(dd.cv$fpr.covar[dd.cv$threshold==j])
        full.fpr.mean <- mean(dd.cv$fpr.full[dd.cv$threshold==j])
        
        covar.fpr.lwr <- covar.fpr.mean - sd(dd.cv$fpr.covar[dd.cv$threshold==j])
        covar.fpr.upr <- covar.fpr.mean + sd(dd.cv$fpr.covar[dd.cv$threshold==j])
        full.fpr.lwr <- full.fpr.mean - sd(dd.cv$fpr.full[dd.cv$threshold==j])
        full.fpr.upr <- full.fpr.mean + sd(dd.cv$fpr.full[dd.cv$threshold==j])
        
        return(c(covar.tpr.mean,
                 covar.tpr.lwr,
                 covar.tpr.upr,
                 full.tpr.mean,
                 full.tpr.lwr,
                 full.tpr.upr,
                 covar.fpr.mean,
                 covar.fpr.lwr,
                 covar.fpr.upr,
                 full.fpr.mean,
                 full.fpr.lwr,
                 full.fpr.upr))
    }))
    colnames(summary.vals) <- c('covar.tpr.mean',
                                'covar.tpr.lwr',
                                'covar.tpr.upr',
                                'full.tpr.mean',
                                'full.tpr.lwr',
                                'full.tpr.upr',
                                'covar.fpr.mean',
                                'covar.fpr.lwr',
                                'covar.fpr.upr',
                                'full.fpr.mean',
                                'full.fpr.lwr',
                                'full.fpr.upr')
    
    summary.vals <- as.data.frame(summary.vals)
    dd.cv.summary[,c('covar.tpr.mean',
                     'covar.tpr.lwr',
                     'covar.tpr.upr',
                     'full.tpr.mean',
                     'full.tpr.lwr',
                     'full.tpr.upr',
                     'covar.fpr.mean',
                     'covar.fpr.lwr',
                     'covar.fpr.upr',
                     'full.fpr.mean',
                     'full.fpr.lwr',
                     'full.fpr.upr')] <- summary.vals
    
    if(!is.null(modelPerfObj)){
        if(!covar.is.null){
            covar.mean <- signif(modelPerfObj$df.summary['avg','auc.ROC.covar'],3)
            covar.lwr <- signif(modelPerfObj$df.summary['2.5%','auc.ROC.covar'],3)
            covar.upr <- signif(modelPerfObj$df.summary['97.5%','auc.ROC.covar'],3)
        }
        full.mean <- signif(modelPerfObj$df.summary['avg','auc.ROC.full'],3)
        full.lwr <- signif(modelPerfObj$df.summary['2.5%','auc.ROC.full'],3)
        full.upr <- signif(modelPerfObj$df.summary['97.5%','auc.ROC.full'],3)
    } else {
        covar.mean <- covar.lwr <- covar.upr <- full.mean <- full.upr <- full.lwr <- NA
    }
    if(!is.null(permPerfObj)){
        if(!covar.is.null){
            covar.p <- signif(permPerfObj$df.summary['pval','auc.ROC.covar'],3)
        }
        
        full.p <- signif(permPerfObj$df.summary['pval','auc.ROC.full'],3)
    } else {
        covar.p <- full.p <- NA
    }
    subtitle.text <- paste0('covar: ',
                            covar.mean, ' [',
                            covar.lwr, ', ',
                            covar.upr, ']; p = ',
                            covar.p, '; ',
                            'full: ',
                            full.mean, ' [',
                            full.lwr, ', ',
                            full.upr, ']; p = ',
                            full.p)
    
    if(is.null(title.text)){
        title.text <- 'ROC Curve'
    }
        
    color.vals <- c('full' = 'red', 'covar' = 'blue')
                                        # Generate ROC curves
    p <- ggplot2::ggplot(dd.cv.summary, ggplot2::aes(x=full.fpr.mean,y=full.tpr.mean)) +
        ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=1, yend=1), color = 'black', lwd = 2) +
        ggplot2::geom_line(ggplot2::aes(color = 'full'), lwd = 2) +
        ggplot2::geom_ribbon(ggplot2::aes(x=full.fpr.mean,
                                        ymin=full.tpr.lwr,
                                        ymax=full.tpr.upr,
                                        fill = 'full'), 
                            alpha = 0.2) +
        ggplot2::labs(title = title.text,
                     subtitle = subtitle.text,
                     x = '1-Specificity',
                     y = 'Sensitivity',
                     color = 'Models') +
        ggplot2::guides(fill = 'none') +
        ggplot2::scale_color_manual(values = color.vals) +
        ggplot2::scale_fill_manual(values = color.vals) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = 20),
                      plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                           size = 16),
                      legend.title.align = 0.5,
                      legend.title = ggplot2::element_text(size = 16),
                      legend.text = ggplot2::element_text(size = 14),
                      axis.title.x = ggplot2::element_text(size = 18),
                      axis.title.y = ggplot2::element_text(size = 18),
                      axis.text.x = ggplot2::element_text(size = 16),
                      axis.text.y = ggplot2::element_text(size = 16))
    if(all(!is.na(dd.cv.summary$covar.tpr.mean))){
        p <- p +
            ggplot2::geom_line(ggplot2::aes(x=covar.fpr.mean,y=covar.tpr.mean,color='covar'),
                              lwd = 2) +
            ggplot2::geom_ribbon(ggplot2::aes(x=covar.fpr.mean,
                                            ymin=covar.tpr.lwr,
                                            ymax=covar.tpr.upr,
                                            fill = 'blue'), 
                                alpha = 0.2
                                )
        
    }
    print(p)
    return(p)
}

## fx_rocPlot <- function(modelPerfObj, permPerfObj = NULL, modelVarPerfObj = NULL, title.name = NULL, compute.perf = 'within', plot.covar = T, plot.full = T, outFile = NULL){
    
    
##     if(!plot.covar&!plot.full){
##         stop('Why plot nothing?')
##     }
    
##     perm.exist <- !is.null(permPerfObj)
##     modelVar.exist <- !is.null(modelVarObj)
    
##     parameters <- modelPerfObj$parameters
    
##     regmodels <- c('regression')
##     if (modelPerfObj$parameters$model.type%in%regmodels){
##         stop('Cannot compute ROC for regression models')
##     }
    
##     if(modelPerfObj$parameters$model.type=='svm'){
##         stop('Cannot perform ROC on SVM model')
##     }
    
##     if(is.null(title.name)){title.name <- 'ROC Curve'}
    
##     pred.prob.covar.sorted <- modelPerfObj$df.allfolds$pred.prob.covar[order(modelPerfObj$df.allfolds$pred.prob.covar,decreasing=T)]
##     actual.class.covar.sorted <- modelPerfObj$df.allfolds$actual.class[order(modelPerfObj$df.allfolds$pred.prob.covar,decreasing=T)]
##     pred.prob.full.sorted <- modelPerfObj$df.allfolds$pred.prob.full[order(modelPerfObj$df.allfolds$pred.prob.full,decreasing=T)]
##     actual.class.full.sorted <- modelPerfObj$df.allfolds$actual.class[order(modelPerfObj$df.allfolds$pred.prob.full,decreasing=T)]
##     class.levels <- modelPerfObj$parameters$class.levels
    
##     # generate fpr and tpr values and plt
##     # c('covar','full')
##     roc.df <- do.call(rbind, lapply(c('covar','full')[c(plot.covar,plot.full)], function(j){
##         pred <- get(paste0("pred.prob.", j, ".sorted"))
##         actual <- get(paste0("actual.class.", j, ".sorted"))
        
##         pred.mid <- sapply(seq(length(pred)),function(i){
##             if(i==1){
##                 return((1+pred[i])/2)
##             } else {
##                 return((pred[i-1]+pred[i])/2)
##             }
##         })
##         pred.mid <- c(1,pred.mid,0)
        
##         roc.vals <- t(sapply(pred.mid, function(i){
##             tp <- sum(pred>=i&actual==class.levels[2])
##             fp <- sum(pred>=i&actual==class.levels[1])
##             fn <- sum(pred<i&actual==class.levels[2])
##             tn <- sum(pred<i&actual==class.levels[1])
##             fpr <- fp/(fp+tn) # false-positive rate
##             tpr <- tp/(tp+fn) # true-positive rate
##             return(c(fpr,tpr))
##         }))
##         roc.vals <- data.frame(roc.vals)
##         colnames(roc.vals) <- c('fpr','tpr')
##         roc.vals$type <- j
##         return(roc.vals)
##     }))
    
##     if (!is.null(outFile)){pdf(fx_outFile(outFile))}
    
##     if(perm.exist&modelVar.exist){
        
##         covar.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.covar'],3)
##         full.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.full'],3)
##         covar.p <- signif(permPerfObj$df.pval['pval','auc.ROC.covar'],3)
##         full.p <- signif(permPerfObj$df.pval['pval','auc.ROC.full'],3)
##         covar.std <- signif(modelVarPerfObj$df.pval['stdev','auc.ROC.covar'],3)
##         full.std <- signif(modelVarPerfObj$df.pval['stdev','auc.ROC.full'],3)
##         nperm <- permPerfObj$parameters$nperm
##         nresample <- modelVarPerfObj$parameters$nresample
        
##         if(plot.covar&plot.full){
##             subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = ', covar.p, '; full: ', full.obs, ' \u00B1 ', full.std, ', p = ', full.p)
##         } else if(plot.covar&!plot.full){
##             subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = ', covar.p)
##         } else if(!plot.covar&plot.full){
##             subtext <- paste0('full: ', full.obs, ' \u00B1 ', full.std, ', p = ', full.p)
##         }
        
##         captext <- paste0('nperm: ', nperm, '; nresample: ', nresample)
        
##     } else if(perm.exist&!modelVar.exist){
        
##         covar.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.covar'],3)
##         covar.p <- signif(permPerfObj$df.pval['pval','auc.ROC.covar'],3)
##         full.obs <- signif(permPerfObj$df.pval['obs','auc.ROC.full'],3)
##         full.p <- signif(permPerfObj$df.pval['pval','auc.ROC.full'],3)
##         nperm <- permPerfObj$parameters$nperm
        
##         subtext <- paste0('covar: ', covar.obs, ' \u00B1 NA, p = ', covar.p, '; full: ', full.obs, ' \u00B1 NA, p = ', full.p)
##         captext <- paste0('nperm: ', nperm, '; nresample: NA')
        
##     } else if(!perm.exist&modelVar.exist){
        
##         covar.obs <- signif(modelVarPerfObj$df.pval['avg','auc.ROC.covar'],3)
##         full.obs <- signif(modelVarPerfObj$df.pval['avg','auc.ROC.full'],3)
##         covar.std <- signif(modelVarPerfObj$df.pval['stdev','auc.ROC.covar'],3)
##         full.std <- signif(modelVarPerfObj$df.pval['stdev','auc.ROC.full'],3)
##         nresample <- modelVarPerfObj$parameters$nresample
        
##         if(plot.covar&plot.full){
##             subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = NA; full: ', full.obs, ' \u00B1 ', full.std, ', p = NA')
##         } else if(plot.covar&!plot.full){
##             subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = NA')
##         } else if(!plot.covar&plot.full){
##             subtext <- paste0('full: ', full.obs, ' \u00B1 ', full.std, ', p = NA')
##         }
        
##         captext <- paste0('nperm: NA; nresample: ', nresample)
        
##     } else if(!perm.exist&&!boot.exist&&!is.null(compute.perf)){
        
##         covar.obs <- signif(modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,'auc.ROC.covar'],3)
##         full.obs <- signif(modelPerfObj$perfMetrics[modelPerfObj$perfMetrics$fold==compute.perf,'auc.ROC.full'],3)
        
##         if(plot.covar&plot.full){
##             subtext <- paste0('covar: ', covar.obs, ' \u00B1 NA, p = NA; full: ', full.obs, ' \u00B1 NA, p = NA')
##         } else if(plot.covar&!plot.full){
##             subtext <- paste0('covar: ', covar.obs, ' \u00B1 NA, p = NA')
##         } else if(!plot.covar&plot.full){
##             subtext <- paste0('full: ', full.obs, ' \u00B1 NA, p = NA')
##         }
        
##         captext <- 'nperm: NA; nresample: NA'
        
##     } else {
##         subtext <- NULL
##         captext <- NULL
##     }
    
    
##     p <- ggplot(data = roc.df, aes(x=fpr,y=tpr,group=type,color=type))
##     print(p + 
##               geom_segment(aes(x=0, y=0, xend=1, yend=1), color = 'gray', lwd = 2) +
##               geom_line(lwd = 2)+
##               labs(title = title.name,
##                    subtitle = subtext,
##                    x = 'False positive rate (1-specificity)',
##                    y = 'True positive rate (sensitivity)',
##                    caption = captext,
##                    color = 'Model') + 
##               theme(plot.title = element_text(hjust = 0.5),
##                     plot.subtitle = element_text(hjust = 0.5),
##                     plot.caption = element_text(hjust = 0.5))
##           )
    
##     if (!is.null(outFile)){
##         writeLines(paste0('Output file written: ', fx_outFile(outFile)))
##         dev.off()
##     }

## }
