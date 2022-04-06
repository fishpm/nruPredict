### 0onload.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: Apr 16 2021 (11:59) 
## Version: 
## Last-Updated: apr  6 2022 (16:37) 
##           By: Brice Ozenne
##     Update #: 8
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:
.onAttach <- function(lib, pkg="nruPredict") {
    desc <- utils::packageDescription(pkg)
    packageStartupMessage(desc$Package, " version ",desc$Version)
}

##----------------------------------------------------------------------
### 0onload.R ends here
