### DESCRIPTION ###

### Derive AUC ROC and optimal threshold values

# INPUTS:
#   pred.prob:
#   actual.class
#   class.levels:

# OUTPUTS:
#   A list of length five, containing the following elements:

#   roc.auc: 
#   optimal.threshold: 

fx_rocCompute <- function(pred.prob, actual.class, class.levels){
    
    pred.prob.sort <- pred.prob[order(pred.prob,decreasing=T)]
    actual.class.sort <- actual.class[order(pred.prob,decreasing=T)]
    
    # generate fpr and tpr values and plt
    roc.vals <- lapply(c(1,pred.prob.sort), function(i){
        tp <- sum(pred.prob.sort>=i&actual.class.sort==class.levels[2])
        fp <- sum(pred.prob.sort>=i&actual.class.sort==class.levels[1])
        fn <- sum(pred.prob.sort<i&actual.class.sort==class.levels[2])
        tn <- sum(pred.prob.sort<i&actual.class.sort==class.levels[1])
        fpr <- fp/(fp+tn) # false-positive rate
        tpr <- tp/(tp+fn) # true-positive rate
        return(c(fpr,tpr))
    })
    
    roc.df <- data.frame(do.call(rbind, roc.vals))
    colnames(roc.df) <- c('fpr','tpr')
    roc.df$thresh <- c(1,pred.prob.sort)
    roc.df$tldist <- sqrt(((1-roc.df$tpr)**2)+(roc.df$fpr**2))
    # thresh.range <- c(ceiling(roc.df$thresh[which(roc.df$tldist==min(roc.df$tldist))+1]*1000)/1000, floor(roc.df$thresh[roc.df$tldist==min(roc.df$tldist)]*1000)/1000)
    optimal.threshold <- signif(mean(roc.df$thresh[roc.df$tldist==min(roc.df$tldist)]),3)
    fpr <- unlist(roc.vals)[c(T,F)] # fpr are odd elements
    tpr <- unlist(roc.vals)[c(F,T)] # tpr are even elements
    id <- order(fpr)
    roc.auc <- sum(diff(fpr[id])*rollmean(tpr[id],2))
    return(list(roc.auc=roc.auc,optimal.threshold=optimal.threshold))
}