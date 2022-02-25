### DESCRIPTION ###

### Create AUC-ROC plots

# INPUTS:
#   modelObj: model object, containing observed model performances
#   modelPerfObj:   object with summary measures of observed model performance
#   permPerfObj:    object with summary measures of permutation model performance
#   model.type:     string for title of ROC curve plot (string)

# OUTPUTS:
#   p: ggplot object of ROC curve


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
    p <- ggplot(dd.cv.summary, aes(x=full.fpr.mean,y=full.tpr.mean)) +
        geom_segment(aes(x=0, y=0, xend=1, yend=1), color = 'black', lwd = 2) +
        geom_line(aes(color = 'full'), lwd = 2) +
        geom_ribbon(aes(x=full.fpr.mean,
                        ymin=full.tpr.lwr,
                        ymax=full.tpr.upr,
                        fill = 'full'), 
                    alpha = 0.2) +
        labs(title = title.text,
             subtitle = subtitle.text,
             x = '1-Specificity',
             y = 'Sensitivity',
             color = 'Models') +
        guides(fill = 'none') +
        scale_color_manual(values = color.vals) +
        scale_fill_manual(values = color.vals) +
        theme(plot.title = element_text(hjust = 0.5,
                                        size = 20),
              plot.subtitle = element_text(hjust = 0.5,
                                           size = 16),
              legend.title.align = 0.5,
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 14),
              axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              axis.text.x = element_text(size = 16),
              axis.text.y = element_text(size = 16))
    if(all(!is.na(dd.cv.summary$covar.tpr.mean))){
        p <- p +
            geom_line(aes(x=covar.fpr.mean,y=covar.tpr.mean,color='covar'),
                      lwd = 2) +
            geom_ribbon(aes(x=covar.fpr.mean,
                            ymin=covar.tpr.lwr,
                            ymax=covar.tpr.upr,
                            fill = 'blue'), 
                        alpha = 0.2
                        )
        
    }
    print(p)
    return(p)
}