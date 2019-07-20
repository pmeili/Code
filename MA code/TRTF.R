library(future.apply)
library(randomForest)
library(trtf)
library(tram)
library(survival)

load('dgp.rda')

plan(multiprocess)  #Initialize parallel.

testxdf<-as.data.frame(testx)
testxdf$trt <- factor(c(0, 1))[2] #Set treatment indicator.

retYOUm <- retYOUq <-args
retYOUm$Class <- "maximum"
retYOUq$Class <- "quadratic"

NumTrees<-250

fitfun <- function(d, statFactor = c("maximum", "quadratic")) {

  W05 <- max(abs(predict(d, newdata = testx)[, "efct"] - .5)) < .Machine$double.eps
  mtry <- floor(sqrt(ncol(d) - 2))

  #Where e = 0.5:
  if(W05) {
    #Check if the conditional response is based on normal or Weibull.
    if (attributes(d)$truth$mod == "normal") {
        m <- as.mlt(Lm(y ~ trt, data = d))
    } else {
        m <- as.mlt(Survreg(y ~ trt, data = d))
    }
    tf <- traforest(m, formula = y | trt ~ ., data = d, ntree = NumTrees,
                    minbucket=20, mtry = mtry,
                    control= ctree_control(teststat = statFactor, 
                                           testtype = 'Univariate',
                                           mincriterion = 0,     
                                           saveinfo = FALSE))  
    
    #Transformation steps to arrive at only a main- and treatment effect:
    cf <- predict(tf, newdata = testxdf, type = "coef")
    cf <- do.call("rbind", cf)
    cfout <- t(t(cf[,c(1, 3)] / cf[,2]) * c(-1, 1)) #Divide through Y in cf[,2].
    colnames(cfout) <- c("m", "tau")
    that<-cfout[,'tau']
  }
  
  else{ #Setups where e!=0.5
    #Calculate treatment probabilities.
    rf <- randomForest(trt ~ ., data=d[,-which(colnames(d)=='y')], ntree = NumTrees)
    #Subtract  treatment probability from treatment indicator.
    d$trtA <- (0:1)[d$trt] - predict(rf, type = "prob")[,2] 
    
    if (attributes(d)$truth$mod == "normal") {
      m <- as.mlt(Lm(y ~ trtA, data = d))
    } else {
      m <- as.mlt(Survreg(y ~ trtA, data = d))
    }

    tf <- traforest(m, formula = y | trtA ~ ., data = d, ntree = NumTrees,
                    minbucket=20, mtry = mtry,
                    control= ctree_control(teststat = statFactor, 
                                           testtype = 'Univariate',
                                           mincriterion = 0,
                                           saveinfo = FALSE))
    
    cf <- predict(tf, newdata = testxdf, type = "coef")
    cf <- do.call("rbind", cf)
    cfout <- t(t(cf[,c(1, 3)] / cf[,2]) * c(-1, 1))
    colnames(cfout) <- c("m", "tau")
    that<-cfout[,'tau']
  }
  
  tTruth <- predict(d, newdata = testxdf)[, "tfct"]
  mean((tTruth - that)^2)
}


set.seed(123)
retYOUm$MSE_t_TF <- future_sapply(learn_yxw[1:nrow(retYOUm)], fitfun, statFactor = "maximum")
retYOUq$MSE_t_TF <- future_sapply(learn_yxw[1:nrow(retYOUq)], fitfun, statFactor = "quadratic")

save(retYOUm, retYOUq, file = "YOUfit.rda")