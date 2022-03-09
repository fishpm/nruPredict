### DESCRIPTION ###

### Load relevant libraries and functions

# INPUTS:
#   NONE

# OUTPUTS:
#   NONE

# relevant libraries
require('mvtnorm')
require('parallel')
require('pROC')
require('lava')
require('scales')
require('ggplot2')
require('randomForest')
require('e1071')
require('zoo')

# relevant functions
functions.to.load <- c('fx_scramble.R',
                       'fx_partition.R',
                       'fx_partition2.R',
                       'fx_sample.R',
                       'fx_model.R',
                       'fx_modelPerf.R',
                       'fx_rocCompute.R',
                       'fx_modelResample.R',
                       'fx_modelResamplePerf.R',
                       'fx_perm.R',
                       'fx_permPerf.R',
                       'fx_rocPlot.R')

for (i in functions.to.load){
    source(paste0('/users/patrick/github/nruPredict/R/',i))
    writeLines(paste0(i, ' loaded!'))
}