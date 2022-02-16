source('~/github/nruPredict/R/fx_nruPredict.R')

rbinom(100, 1, 0.5)
n <- 100
dd <- data.frame(group = factor(sample(c('MDD','HC'),n,replace=T)),
                 age = rnorm(n,25,5),
                 sex = factor(sample(c('male','female'),n,replace=T)))


dd$f1 <- rnorm(n,0,0.5) + as.numeric(dd$group)
dd$f2 <- rnorm(n,0,0.5)
summary(lm(f1 ~ group, data = dd))
voi <- c('f1','f2')
covar <- c('age','sex')
y <- 'group'
nresample <- 10
nperm <- 10

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
                             n.cores = 10) # parallel processing

table(dd$group)
table(modelObj$modelResamplePerfObj[[1]]$df.allfolds$actual.class)
table(modelObj$modelResamplePerfObj[[1]]$df.allfolds$orig.df.row)
fx_partition(dd, type = '5-fold', balance.col = NULL)


seq(100)%in%unlist(lapply(partition.list, function(i){i$test}))
table(unlist(lapply(partition.list, function(i){i$test})))

lapply(partition.list, function(i){
    table(dd[i$train,'group'])
    #i$train
    })


# determine overall model performance
modelPerfObj <- fx_modelResamplePerf(modelResampleObj = modelObj)

# permutation testing
permObj <- fx_perm(df0 = dd, modelObj = modelObj, nperm = nperm, n.cores = 10)

# determine permutation test performance
permPerfObj <- fx_permPerf(permObj = permObj, modelResamplePerf = modelPerfObj)


nresample <- 10
nperm <- 100
