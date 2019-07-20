#DGP and Simulate.dgp function-------------------------------------------------
dgp<-function(e=0.5, m=0, z=0, sd=1,model = c("normal", "weibull")){

  if (!is.function(efct <- e))
    efct <- function(x) return(rep(e, nrow(x)))

  if (!is.function(mfct <- m))
    mfct <- function(x) return(rep(m, nrow(x)))

  if (!is.function(zfct <- z))
    zfct <- function(x) return(rep(z, nrow(x)))

  #Tau(x) = zeta(x1) * zeta(x2)
  tfct <- function(x) zfct(x[,"X1"])*zfct(x[,"X2"]) 

  if (!is.function(sdfct <- sd))
    sdfct <- function(x) return(rep(sd, nrow(x)))

  model<-model
  ret <- list(efct = efct, mfct = mfct, tfct = tfct, sdfct = sdfct, model = model)
  class(ret) <- "dgp"
  return(ret)
}

simulate.dgp<-function(object, nsim = 1, seed = NULL, dim = 3) {
  if (!is.null(seed))
      set.seed(seed)
  
  x<-matrix(runif(nsim*dim,0,1),nrow = nsim, ncol = dim)
  colnames(x) <- paste("X", 1:ncol(x), sep = "")
  
  tX<-object$tfct(x)
  eX<-object$efct(x)
  mX<-object$mfct(x)
  sd<-object$sdfct(x)
  model<-object$model

  trt<-rbinom(nsim, size = 1, prob = eX)   #Wi|Xi ~Bernoulli (e(Xi))

  y <- switch(model, 
      "normal"  = rnorm(nsim, mean = mX+(trt-0.5)*tX, sd = sd),
      "weibull" = rweibull(nsim, shape = 1, scale = exp(mX+(trt-0.5)*tX))
  )
  
  df <- data.frame(x, y = y, trt = factor(trt))
  attributes(df)$truth <- object
  class(df) <- c("simdgp", class(df))
  return(df)
}


#Evaluate the truth.
predict.simdgp <- function(object, newdata, ...) {
    atr <- attributes(object)$truth
    atr <- atr[sapply(atr, is.function)]
    sapply(atr, function(f) f(newdata))
}

#Define the different zeta, treatment propensity and main effect functions-----
#Zeta function
zF<-function(x){
  1+1/(1+exp(-20*(x-1/3))) 
}

#Treatment propensity functions
eF_x1<-function(x){
  1/4*(1+dbeta(x[,"X1"],2,4))
}

eF_x3<-function(x){
  1/4*(1+dbeta(x[,"X3"],2,4))
}

eF_sin_x3<-function(x){
  sin(2*pi*x[,"X3"]) / 4 + .5
}

#Main effect functions
mF_x1<-function(x){
  2*x[,"X1"]-1
}

mF_x3<-function(x){
  2*x[,"X3"]-1
}

#Generate the data-------------------------------------------------------------
setups <- vector(mode = "list", length = 1)
setups[[1]] <- dgp(e = 0.5,       m = 0,     z = zF, sd = 1, model = "normal")
setups[[2]] <- dgp(e = 0.5,       m = mF_x1, z = zF, sd = 1, model = "normal")
setups[[3]] <- dgp(e = 0.5,       m = mF_x3, z = zF, sd = 1, model = "normal")
setups[[4]] <- dgp(e = eF_x1,     m = 0,     z = zF, sd = 1, model = "normal")
setups[[5]] <- dgp(e = eF_x1,     m = mF_x1, z = zF, sd = 1, model = "normal")
setups[[6]] <- dgp(e = eF_x1,     m = mF_x3, z = zF, sd = 1, model = "normal")
setups[[7]] <- dgp(e = eF_x3,     m = 0,     z = zF, sd = 1, model = "normal")
setups[[8]] <- dgp(e = eF_x3,     m = mF_x1, z = zF, sd = 1, model = "normal")
setups[[9]] <- dgp(e = eF_x3,     m = mF_x3, z = zF, sd = 1, model = "normal")
setups[[10]] <-dgp(e = eF_sin_x3, m = 0,     z = zF, sd = 1, model = "normal")
setups[[11]] <-dgp(e = eF_sin_x3, m = mF_x1, z = zF, sd = 1, model = "normal")
setups[[12]] <-dgp(e = eF_sin_x3, m = mF_x3, z = zF, sd = 1, model = "normal")
setups[[13]] <-dgp(e = 0.5,       m = 0,     z = zF, sd = 1, model = "weibull")
setups[[14]] <-dgp(e = 0.5,       m = mF_x1, z = zF, sd = 1, model = "weibull")
setups[[15]] <-dgp(e = 0.5,       m = mF_x3, z = zF, sd = 1, model = "weibull")
setups[[16]] <-dgp(e = eF_x1,     m = 0,     z = zF, sd = 1, model = "weibull")
setups[[17]] <-dgp(e = eF_x1,     m = mF_x1, z = zF, sd = 1, model = "weibull")
setups[[18]] <-dgp(e = eF_x1,     m = mF_x3, z = zF, sd = 1, model = "weibull")
setups[[19]] <-dgp(e = eF_x3,     m = 0,     z = zF, sd = 1, model = "weibull")
setups[[20]] <-dgp(e = eF_x3,     m = mF_x1, z = zF, sd = 1, model = "weibull")
setups[[21]] <-dgp(e = eF_x3,     m = mF_x3, z = zF, sd = 1, model = "weibull")
setups[[22]] <-dgp(e = eF_sin_x3, m = 0,     z = zF, sd = 1, model = "weibull")
setups[[23]] <-dgp(e = eF_sin_x3, m = mF_x1, z = zF, sd = 1, model = "weibull")
setups[[24]] <-dgp(e = eF_sin_x3, m = mF_x3, z = zF, sd = 1, model = "weibull")


NSIM <- 25  #How many replicas per dataset
args <- expand.grid(setup = 1:length(setups),
                    nsim = c(100, 250, 500, 1000),
                    dim = c(10, 50, 500),
                    repl = 1:NSIM)

set.seed(123)
learn_yxw <- lapply(1:nrow(args), function(i) {
  simulate(setups[[args$setup[i]]], nsim = args$nsim[i], 
           dim = args$dim[i])
})

testx <- matrix(runif(10000 * max(args$dim)), nrow = 10000)
colnames(testx) <- paste("X", 1:ncol(testx), sep = "")

#save(args, learn_yxw, testx, predict.simdgp, zF, file = "dgp.rda")

rm(setups,NSIM,dgp,eF_sin_x3,eF_x1,eF_x3,mF_x1,mF_x3, simulate.dgp ,zF)
