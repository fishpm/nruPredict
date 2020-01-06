library(devtools)
library(roxygen2)
setwd("c:/Users/hpl802/Documents/GitHub/")

## refresh documentation
roxygenise("nruPredict")

## load package
load_all("nruPredict")

## install package 
install("nruPredict", upgrade = "never", dependencies = "never")

## test package
test()


help(t.test)
