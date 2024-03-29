## source('~/github/nruPredict/R/fx_nruPredict.R')
## source('~/github/nruPredict/vignettes/fx_nruPredict.R')

## Other users
## remotes::install_github('fishpm/nruPredict')
library(nruPredict)

## you
## devtools::load_all()

library(randomForest)
library(e1071)

# Make data frame

# sample size
n <- 100

# create variables and data frame
set.seed(1)
group <- factor(sample(c('MDD','HC'),n,replace=T))
age <- rnorm(n,25,5)
sex <- factor(sample(c('male','female'),n,replace=T))
rand.vals1 <- rnorm(n,0,0.75)
set.seed(2)
rand.vals2 <- rnorm(n,0,0.75)
dd <- data.frame(group = group,
                 age = age,
                 sex = sex,
                 f1 = rand.vals1 + as.numeric(group),
                 f2 = rand.vals2)

# linear model confirming f1 associated with group
summary(lm(f1 ~ group, data = dd))

####
## MODEL EXAMPLE 1
####

# covariates
covar <- c('age','sex')
# variables of interest
voi <- c('f1','f2')
# class outcome
y <- 'group'

# resamples and permutations
nresample <- 10
nperm <- 50
n.cores <- 1 ## 10

# fit classification model
modelObj <- fx_modelResample(df0 = dd, # data frame
                             cv.type = '5-fold', # type of cross-validation
                             covar = covar, # covariate set
                             voi = voi,  # variables of interest (i.e., brain regions)
                             outcome = y, # class
                             model.type = 'rf', # model type (randomForest)
                             nresample = nresample, # number of resamples
                             dthresh = 0.5, # threshold (not used)
                             z.pred = F, # standardize continuous predictors
                             balance.col = y, # stratified cv
                             n.cores = n.cores) # parallel processing

# determine overall model performance
modelPerfObj <- fx_modelResamplePerf(modelResampleObj = modelObj)
# permutation testing
permObj <- fx_perm(df0 = dd, modelObj = modelObj, nperm = nperm, n.cores = n.cores)
# determine permutation test performance
permPerfObj <- fx_permPerf(permObj = permObj, modelResamplePerf = modelPerfObj)

### Additional information

## Observed model performance
# Summary of performance measures based on observed data
modelPerfObj$df.summary
# Outcome metrics for each resample
modelPerfObj$df.iter

## Permutation test results
# Summary of permutation test outcomes
permPerfObj$df.summary
# Outcome metrics for each permutation
permPerfObj$df.iter

## Compare performance of covariate and full model
# Mean difference between nested models
diff.obs <- modelPerfObj$df.summary['avg','auc.ROC.full']-modelPerfObj$df.summary['avg','auc.ROC.covar']
# Null distribution of differences between nested models
diff.perm <- modelPerfObj$df.iter$auc.ROC.full-modelPerfObj$df.iter$auc.ROC.covar
# p-value (one-tailed) between nested models
diff.p <- sum(diff.perm > diff.obs)/nrow(modelPerfObj$df.iter)

# create roc curve plot
fx_rocPlot(modelObj = modelObj, modelPerfObj = modelPerfObj, permPerfObj = permPerfObj, title.text = 'My Title')


## Explicitly control fold assignment to compare non-nested models

set.seed(3)
rand.vals3 <- rnorm(n,0,0.25)
dd$f3 <- rand.vals3 + as.numeric(dd$group)

# all.partitions is a list of length nresample. Each element of this list is a list of length k (i.e., k-fold CV). Each element of this list contains indices to be assigned to train and test datasets
all.partitions <- lapply(seq(nresample), function(i){
    return(fx_partition(dd, type = '5-fold', balance.col = y))
})


# fit classification model
modelObj1 <- fx_modelResample(df0 = dd, # data frame
                             cv.type = '5-fold', # type of cross-validation
                             covar = covar, # covariate set
                             voi = voi,  # variables of interest (i.e., brain regions)
                             outcome = y, # class
                             model.type = 'logistic', # model type (randomForest)
                             nresample = nresample, # number of resamples
                             dthresh = 0.5, # threshold (not used)
                             z.pred = F, # standardize continuous predictors
                             balance.col = y, # stratified cv
                             partitions = all.partitions, # defines fold assignment for each resample
                             n.cores = n.cores) # parallel processing

voi2 <- 'f3'
# fit classification model
modelObj2 <- fx_modelResample(df0 = dd, # data frame
                             cv.type = '5-fold', # type of cross-validation
                             covar = covar, # covariate set
                             voi = voi2,  # variables of interest (i.e., brain regions)
                             outcome = y, # class
                             model.type = 'rf', # model type (randomForest)
                             nresample = nresample, # number of resamples
                             dthresh = 0.5, # threshold (not used)
                             z.pred = F, # standardize continuous predictors
                             balance.col = y, # stratified cv
                             partitions = all.partitions, # defines fold assignment for each resample
                             n.cores = n.cores) # parallel processing

# model1 summary objects
modelPerfObj1 <- fx_modelResamplePerf(modelResampleObj = modelObj1)
permObj1 <- fx_perm(df0 = dd, modelObj = modelObj1, nperm = nperm, n.cores = n.cores)
permPerfObj1 <- fx_permPerf(permObj = permObj1, modelResamplePerf = modelPerfObj1)

# model2 summary objects
modelPerfObj2 <- fx_modelResamplePerf(modelResampleObj = modelObj2)
permObj2 <- fx_perm(df0 = dd, modelObj = modelObj2, nperm = nperm, n.cores = n.cores)
permPerfObj2 <- fx_permPerf(permObj = permObj2, modelResamplePerf = modelPerfObj2)

modelPerfObj1$df.summary['avg', 'auc.ROC.full']-modelPerfObj2$df.summary['avg', 'auc.ROC.full']

perm.positions <- lapply(seq(nperm), function(i){
    sample(seq(nrow(dd)), replace = F)
})
all.partitions <- lapply(seq(nperm), function(i){
    return(fx_partition(dd, type = '5-fold', balance.col = y))
})

# model1 summary objects
modelPerfObj1 <- fx_modelResamplePerf(modelResampleObj = modelObj1)
permObj1 <- fx_perm(df0 = dd, modelObj = modelObj1, nperm = nperm, perm.positions = perm.positions, partitions = all.partitions, n.cores = n.cores)

permPerfObj1 <- fx_permPerf(permObj = permObj1, modelResamplePerf = modelPerfObj1)

# model2 summary objects
modelPerfObj2 <- fx_modelResamplePerf(modelResampleObj = modelObj2)
permObj2 <- fx_perm(df0 = dd, modelObj = modelObj2, nperm = nperm, n.cores = n.cores)
permPerfObj2 <- fx_permPerf(permObj = permObj2, modelResamplePerf = modelPerfObj2)


permPerfObj1$df.iter$auc.ROC.full-permPerfObj2$df.iter$auc.ROC.full

diff.obs <- modelPerfObj1$df.summary['avg', 'auc.ROC.full']-modelPerfObj2$df.summary['avg', 'auc.ROC.full']
diff.perm <- permPerfObj1$df.iter$auc.ROC.full-permPerfObj2$df.iter$auc.ROC.full
sum(abs(diff.perm)>abs(diff.obs))/nperm






modelObj.rf <- fx_modelResample(df0 = dd, # data frame
                             cv.type = '5-fold', # type of cross-validation
                             covar = covar, # covariate set
                             voi = voi,  # variables of interest (i.e., brain regions)
                             outcome = y, # class
                             model.type = 'rf', # model type (randomForest)
                             nresample = nresample, # number of resamples
                             dthresh = 0.5, # threshold (not used)
                             z.pred = F, # standardize continuous predictors
                             balance.col = y, # stratified cv
                             n.cores = n.cores) # parallel processing

all.partitions <- fx_partition2(modelObj.rf)
modelPerfObj.rf <- fx_modelResamplePerf(modelResampleObj = modelObj.rf)
modelObj.logistic <- fx_modelResample(df0 = dd,
                              cv.type = '5-fold',
                              covar = covar,
                              voi = voi,
                              outcome = y,
                              model.type = 'logistic',
                              nresample = nresample,
                              dthresh = 0.5,
                              z.pred = F,
                              balance.col = y,
                              n.cores = n.cores,
                              partitions = all.partitions)
modelPerfObj.logistic <- fx_modelResamplePerf(modelResampleObj = modelObj.logistic)

modelPerfObj.rf$df.summary['avg','auc.ROC.covar'] - modelPerfObj.logistic$df.summary['avg','auc.ROC.covar']

hist(modelPerfObj.rf$df.iter$auc.ROC.covar-modelPerfObj.logistic$df.iter$auc.ROC.covar)

f <- as.formula(paste0(y, ' ~ ', paste(c(covar,voi), collapse = '+')))
l1 <- glm(f, data = dd, family = 'binomial')
l2 <- randomForest(f, data = dd)
l3 <- svm(f, data = dd, probability = T)

p1 <- predict(l1, newdata = dd, type = 'resp')
p2 <- predict(l2, newdata = dd, type = 'prob')[,levels(dd[[y]])[2]]
p3 <- attr(predict(l3, newdata = dd, probability = T), 'probabilities')[,levels(dd[[y]])[2]]

l1 <- glm(f, data = dd, family = 'binomial')
p1 <- predict(l1, newdata = dd, type = 'resp')
l1a <- glm(f, data = dd, family = 'binomial')
p1a <- predict(l1a, newdata = dd, type = 'resp')
cor(p1,p1a)# note they are NOT perfectly correlated

l2 <- randomForest(f, data = dd)
p2 <- predict(l2, newdata = dd, type = 'prob')[,levels(dd[[y]])[2]]
l2a <- randomForest(f, data = dd)
p2a <- predict(l2a, newdata = dd, type = 'prob')[,levels(dd[[y]])[2]]
cor(p2,p2a) # note they are PERFECTLY correlated

l3 <- svm(f, data = dd, probability = T)
p3 <- attr(predict(l3, newdata = dd, probability = T), 'probabilities')[,levels(dd[[y]])[2]]
l3a <- svm(f, data = dd, probability = T)
p3a <- attr(predict(l3a, newdata = dd, probability = T), 'probabilities')[,levels(dd[[y]])[2]]
cor(p3,p3a) # note they are NOT perfectly correlated
plot(p3,p3a)




dd.out <- data.frame(p1 = p1,
                     p2 = p2,
                     p3 = p3)
cor(dd.out)


l <- lm(f1 ~ group, data = dd)
summary(l)
plot(f1 ~ group, dd)

mdd.n <- length(dd$f1[dd$group=='MDD'])
mdd.mean <- mean(dd$f1[dd$group=='MDD'])
mdd.sd <- sd(dd$f1[dd$group=='MDD'])
hc.n <- length(dd$f1[dd$group=='HC'])
hc.mean <- mean(dd$f1[dd$group=='HC'])
hc.sd <- sd(dd$f1[dd$group=='HC'])


diff.mean <- (mdd.mean-hc.mean)
pooled.sd <- sqrt((((mdd.n-1)*mdd.sd**2)+((hc.n-1)*hc.sd**2))/((mdd.n+hc.n-2)))
d <- diff.mean/pooled.sd


### continuous prediction example

# Make data frame

# sample size
n <- 100

# create variables and data frame
set.seed(1)
bpnd <- rnorm(n,2.5,1)
age <- rnorm(n,25,5)
sex <- factor(sample(c('male','female'),n,replace=T))
rand.vals1 <- rnorm(n,0,0.75)
set.seed(2)
rand.vals2 <- rnorm(n,0,0.75)
dd <- data.frame(bpnd = bpnd,
                 age = age,
                 sex = sex,
                 f1 = rand.vals1 + bpnd,
                 f2 = rand.vals2)

# linear model confirming f1 associated with group
summary(lm(bpnd ~ age + sex + f1 + f2, data = dd))

####
## MODEL EXAMPLE 1
####

# covariates
covar <- c('age','sex')
# variables of interest
voi <- c('f1','f2')
# class outcome
y <- 'bpnd'

# resamples and permutations
nresample <- 10
nperm <- 50

all.partitions <- lapply(seq(nresample), function(i){
    return(fx_partition(dd, type = '5-fold'))
})
perm.positions <- lapply(seq(nperm), function(i){
    sample(seq(nrow(dd)), replace = F)
})

# fit classification model
source('~/github/nruPredict/vignettes/fx_nruPredict.R')
modelObj <- fx_modelResample(df0 = dd, # data frame
                             cv.type = '5-fold', # type of cross-validation
                             covar = covar, # covariate set
                             voi = voi,  # variables of interest (i.e., brain regions)
                             outcome = y, # outcome variable
                             model.type = 'regression', # model type (randomForest)
                             nresample = nresample, # number of resamples
                             partitions = all.partitions,
                             z.pred = F, # standardize continuous predictors
                             n.cores = 10) # parallel processing

# determine overall model performance
modelPerfObj <- fx_modelResamplePerf(modelResampleObj = modelObj)
# permutation testing
permObj <- fx_perm(df0 = dd, 
                   modelObj = modelObj, 
                   nperm = nperm, 
                   nresample = nresample,
                   perm.positions = perm.positions, 
                   partitions = all.partitions,
                   n.cores = 10)
# determine permutation test performance
permPerfObj <- fx_permPerf(permObj = permObj, modelResamplePerf = modelPerfObj)

modelPerfObj

source('~/github/nruPredict/vignettes/fx_nruPredict.R')