### Generates ROC plot

fx_rocPlot <- function(modelObj, permPerfObj = NULL, modelResamplePerf = NULL, title.name = NULL, plot.covar = T, plot.full = T, outFile = NULL, d.steps = 0.01){
    
    
    if(!plot.covar&!plot.full){
        stop('Why plot nothing?')
    }
    
    perm.exist <- !is.null(permPerfObj)
    resample.exist <- !is.null(modelResamplePerf)
    
    parameters <- modelObj$parameters
    
    regmodels <- c('regression')
    if (modelObj$parameters$model.type%in%regmodels){
        stop('Cannot compute ROC for regression models')
    }
    
    if(modelObj$parameters$model.type=='svm'){
        stop('Cannot perform ROC on SVM model')
    }
    
    if(is.null(title.name)){title.name <- 'ROC Curve'}
    
    d.range <- seq(0,1,by=d.steps)
    
    class.levels <- parameters$class.levels
    roc.list <- lapply(seq(length(modelObj$modelResamplePerfObj)), function(i){
        
        actual.class <- modelObj$modelResamplePerfObj[[i]]$df.allfolds$actual.class
        n.neg <- sum(actual.class==class.levels[1])
        n.pos <- sum(actual.class==class.levels[2])
        
        roc.iter <- data.frame(do.call(rbind,lapply(d.range, function(j){
            
            if(!is.null(parameters$covar)){
                
                pred.covar <- class.levels[as.numeric(modelObj$modelResamplePerfObj[[i]]$df.allfolds$pred.prob.covar>j)+1]
                tp.covar <- sum(pred.covar==class.levels[2] & actual.class==class.levels[2])
                fp.covar <- sum(pred.covar==class.levels[2] & actual.class==class.levels[1])
                tpr.covar <- tp.covar/n.pos
                fpr.covar <- fp.covar/n.neg
                
            } else {
                
                tpr.covar <- NULL
                fpr.covar <- NULL
                
            }
            
            pred.full <- class.levels[as.numeric(modelObj$modelResamplePerfObj[[i]]$df.allfolds$pred.prob.full>j)+1]
            tp.full <- sum(pred.full==class.levels[2] & actual.class==class.levels[2])
            fp.full <- sum(pred.full==class.levels[2] & actual.class==class.levels[1])
            tpr.full <- tp.full/n.pos
            fpr.full <- fp.full/n.neg
            
            return(list(tpr.covar=tpr.covar,
                        fpr.covar=fpr.covar,
                        tpr.full=tpr.full,
                        fpr.full=fpr.full))
        })))
        return(roc.iter)
    })
    
    if(!is.null(parameters$covar)){
        
        tpr.covar.array <- rowMeans(sapply(seq(length(roc.list)), function(i){
            unlist(roc.list[[i]]$tpr.covar)
        }))
        fpr.covar.array <- rowMeans(sapply(seq(length(roc.list)), function(i){
            unlist(roc.list[[i]]$fpr.covar)
        }))    
        
    } else {
        
        tpr.covar.array <- NULL
        fpr.covar.array <- NULL
        
    }
    
    tpr.full.array <- rowMeans(sapply(seq(length(roc.list)), function(i){
        unlist(roc.list[[i]]$tpr.full)
    }))
    fpr.full.array <- rowMeans(sapply(seq(length(roc.list)), function(i){
        unlist(roc.list[[i]]$fpr.full)
    }))
    
    roc.df <- data.frame(tpr.array = c(tpr.covar.array,tpr.full.array),
                         fpr.array = c(fpr.covar.array,fpr.full.array),
                         type = c(rep('covar',length(tpr.covar.array)),rep('full',length(tpr.covar.array)))
                         )
    
    if (!is.null(outFile)){pdf(fx_outFile(outFile))}
    
    if(perm.exist&resample.exist){
        
        covar.obs <- signif(permPerfObj$df.summary['obs','auc.ROC.covar'],3)
        full.obs <- signif(permPerfObj$df.summary['obs','auc.ROC.full'],3)
        covar.p <- signif(permPerfObj$df.summary['pval','auc.ROC.covar'],3)
        full.p <- signif(permPerfObj$df.summary['pval','auc.ROC.full'],3)
        covar.std <- signif(modelResamplePerf$df.summary['stdev','auc.ROC.covar'],3)
        full.std <- signif(modelResamplePerf$df.summary['stdev','auc.ROC.full'],3)
        nperm <- permPerfObj$parameters$nperm
        nresample <- modelResamplePerf$parameters$nresample
        
        if(plot.covar&plot.full){
            subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = ', covar.p, '; full: ', full.obs, ' \u00B1 ', full.std, ', p = ', full.p)
        } else if(plot.covar&!plot.full){
            subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = ', covar.p)
        } else if(!plot.covar&plot.full){
            subtext <- paste0('full: ', full.obs, ' \u00B1 ', full.std, ', p = ', full.p)
        }
        
        captext <- paste0('nperm: ', nperm, '; nresample: ', nresample)
        
    } else if(perm.exist&!resample.exist){
        
        covar.obs <- signif(permPerfObj$df.summary['obs','auc.ROC.covar'],3)
        full.obs <- signif(permPerfObj$df.summary['obs','auc.ROC.full'],3)
        covar.p <- signif(permPerfObj$df.summary['pval','auc.ROC.covar'],3)
        full.p <- signif(permPerfObj$df.summary['pval','auc.ROC.full'],3)
        nperm <- permPerfObj$parameters$nperm
        
        subtext <- paste0('covar: ', covar.obs, ' \u00B1 NA, p = ', covar.p, '; full: ', full.obs, ' \u00B1 NA, p = ', full.p)
        captext <- paste0('nperm: ', nperm, '; nresample: ', length(modelObj$modelResamplePerfObj))
        
    } else if(!perm.exist&resample.exist){
        
        covar.obs <- signif(modelResamplePerf$df.summary['avg','auc.ROC.covar'],3)
        full.obs <- signif(modelResamplePerf$df.summary['avg','auc.ROC.full'],3)
        covar.std <- signif(modelResamplePerf$df.summary['stdev','auc.ROC.covar'],3)
        full.std <- signif(modelResamplePerf$df.summary['stdev','auc.ROC.full'],3)
        nresample <- modelResamplePerf$parameters$nresample
        
        if(plot.covar&plot.full){
            subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = NA; full: ', full.obs, ' \u00B1 ', full.std, ', p = NA')
        } else if(plot.covar&!plot.full){
            subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = NA')
        } else if(!plot.covar&plot.full){
            subtext <- paste0('full: ', full.obs, ' \u00B1 ', full.std, ', p = NA')
        }
        
        captext <- paste0('nperm: NA; nresample: ', nresample)
        
    } else if(!perm.exist&&!resample.exist){
        
        modelResamplePerf <- fx_modelResamplePerf(modelObj)
        covar.obs <- signif(modelResamplePerf$df.summary['avg','auc.ROC.covar'],3)
        full.obs <- signif(modelResamplePerf$df.summary['avg','auc.ROC.full'],3)
        covar.std <- signif(modelResamplePerf$df.summary['stdev','auc.ROC.covar'],3)
        full.std <- signif(modelResamplePerf$df.summary['stdev','auc.ROC.full'],3)
        nresample <- modelResamplePerf$parameters$nresample
        
        if(plot.covar&plot.full){
            subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = NA; full: ', full.obs, ' \u00B1 ', full.std, ', p = NA')
        } else if(plot.covar&!plot.full){
            subtext <- paste0('covar: ', covar.obs, ' \u00B1 ', covar.std, ', p = NA')
        } else if(!plot.covar&plot.full){
            subtext <- paste0('full: ', full.obs, ' \u00B1 ', full.std, ', p = NA')
        }
        
        captext <- paste0('nperm: NA; nresample: ', nresample)
        
    } else {
        
        subtext <- NULL
        captext <- NULL
    }
    
    
    p <- ggplot(data = roc.df, aes(x=fpr.array,y=tpr.array,group=type,color=type))
    print(p + 
              geom_segment(aes(x=0, y=0, xend=1, yend=1), color = 'gray', lwd = 2) +
              geom_line(lwd = 2)+
              labs(title = title.name,
                   subtitle = subtext,
                   x = 'False positive rate (1-specificity)',
                   y = 'True positive rate (sensitivity)',
                   caption = captext,
                   color = 'Model') + 
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    plot.caption = element_text(hjust = 0.5))
    )
    
    if (!is.null(outFile)){
        writeLines(paste0('Output file written: ', fx_outFile(outFile)))
        dev.off()
    }
    
}
