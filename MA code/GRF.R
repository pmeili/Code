library(grf)
library(future.apply) #For parallel computing with future_sapply.

plan(multiprocess) #Initialize parallel.

retGRFh <- retGRFd <- args

retGRFh$Class <- TRUE
retGRFd$Class <- FALSE


NumTrees<-250

fitfun <- function(d, honestyFactor = TRUE) {

  if(attributes(d)$truth$mod == "weibull"){
    d$y<-log(d$y)  #Transform skewed distribution, since grf assumes symmetric.
  }
  
  W05 <- max(abs(predict(d, newdata = testx)[, "efct"] - .5)) < .Machine$double.eps
  mtry <- floor(sqrt(ncol(d) - 2))

  #The setups where e=0.5:
  if(W05) {
    #ci.group.size must be less than 2 because with CI's enabled, the sampling
    #fraction would have to be less than 0.5.
    cf <- causal_forest(X = as.matrix(d[, grep("^X", colnames(d))]), 
                        Y = d$y, W = (0:1)[d$trt], W.hat = 0.5,
                        min.node.size = 20, sample.fraction = 0.632,
                        mtry = mtry, ci.group.size = 1,
                        num.trees = NumTrees, honesty=honestyFactor)
  }
  else{
    cf <- causal_forest(X = as.matrix(d[, grep("^X", colnames(d))]), 
                        Y = d$y, W = (0:1)[d$trt], 
                        min.node.size = 20, sample.fraction = 0.632,
                        mtry = mtry,ci.group.size = 1,
                        num.trees = NumTrees, honesty=honestyFactor)
  }
  
  that <- predict(cf, newdat = testx)$predictions
  tTruth<-predict(d, newdata = testx)[, "tfct"]
  mean((tTruth - that)^2)
}

set.seed(123)
retGRFh$MSE_t_CF <- future_sapply(learn_yxw[1:nrow(retGRFh)], fitfun, honestyFactor = TRUE)
retGRFd$MSE_t_CF <- future_sapply(learn_yxw[1:nrow(retGRFd)], fitfun, honestyFactor = FALSE)

save(retGRFh, retGRFd, file = "GRFfit.rda")