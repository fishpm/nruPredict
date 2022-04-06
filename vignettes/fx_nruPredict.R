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


writeLines('Following functions loaded:')
writeLines('\tfx_scramble: Create df with scrambled group assignment')
writeLines('\tfx_partition: List of partitions to apply to data frame')
writeLines('\tfx_sample: Create train/test sub data frames')
writeLines('\tfx_model: Train/test model on sub data frames')
writeLines('\tfx_modelPerf: Confusion matrix and model performance metrics')
writeLines('\tfx_rocCompute: Compute AUC of ROC')
writeLines('\tfx_perm: Derive null distribution')
writeLines('\tfx_boot: Bootstrap confidence intervals')
writeLines('\tfx_permPerf: Estimate p-values, organize null distributions')
writeLines('\tfx_bootPerf: Estimate CIs and p-values from bootstrap distributions')
writeLines('\tfx_plot: Plot observed vs. null/bootstrap distributions')
writeLines('\tfx_rocPlot: Estimate and plot ROC')
writeLines('\tfx_outFile: Handles specified output files')
writeLines('\tfx_summary: Produces .txt summary of model info (incomplete)')

## TODO
# fill out fx_summary
