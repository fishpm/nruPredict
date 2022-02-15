library(lava)
m0 <- lvm(Y~X)
class(m0)

m <- list(f = Y~X)
class(m) <- "lvm2"

m
class(m)

m

summary.lvm2 <- function(object, ...){
 print("Patrick is great!")
}
summary(m)

lava:::compare

calcPerf <- function (object, ...) UseMethod("calcPerf")

calcPerf.lvm2 <- function(object, ...){
    object$perf <- 1
    class(object) <- append("perf.lvm2",class(object))
    return(object)
    }

class(calcPerf(m))

summary(calcPerf(m))




