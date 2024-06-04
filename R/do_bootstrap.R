est.fun1 <- function(X,e,n)
{
  K <- L <- matrix(NA,nrow = n,ncol = n)
  I <- matrix(1,nrow=n,ncol=1)
  H <- diag(1,nrow = n,ncol = n)-I %*% t(I)/n

  L <- exp(-(e%*%t(I) - I%*%t(e))^2)
  K <- exp(-(X%*%t(I) - I%*%t(X))^2)

  sum(diag(K%*%H%*%L%*%H))/n^2
}

##xony
do.one.xony.seperate <- function(seed.id,dataset,theta,fit_resid_xiony,predict_xiony)
{
  ## randomization
  set.seed(seed.id+2333)
  res <- fit_resid_xiony(dataset)
  fitxiony <- res$model
  # fitxiony <- lm(xiony, data=dataset)
  # e.yi <- residuals(fitxiony)
  e.yi <- res$residuals
  e.centered <- e.yi - mean(e.yi)

  n <- nrow(dataset)
  bindex1 <- sample(1:n,n,replace = TRUE)
  bindex2 <- sample(1:n,n,replace = TRUE)
  xi <- data.frame(dataset[,"y"])
  colnames(xi) <- "y"

  X.star <- data.frame(xi[bindex2,])
  colnames(X.star) <- "y"
  newdata <- data.frame(X.star)
  eta.star <- as.matrix(e.centered[bindex1])

  Y.star <- predict_xiony(fitxiony, newdata) + eta.star
  newdata["x"] <- Y.star
  # Y.star <- predict(fitxiony,newdata = newdata) + eta.star
  # fit.star <- lm(Y.star~.,data = newdata)
  res.star <- fit_resid_xiony(newdata)
  fit.star <- res.star$model
  # fit.star <- lm(Y.star~.,data = newdata)
  # e.star <- residuals(fit.star)
  e.star <- res.star$residuals

  thetai <- est.fun1(X.star[,1],e.star,n)
  thetahat.star <- thetai

  return(list(thetahat.star,thetahat.star >= theta))
}

##yonx
do.one.yonx.seperate <- function(seed.id,dataset,theta,fit_resid_yonxi,predict_yonxi)
{
  ## randomization
  set.seed(seed.id+2333)
  xi <- data.frame(dataset[,"x"])
  colnames(xi) <- "x"
  res <- fit_resid_yonxi(dataset)
  fityonxi <- res$model
  e.yi <- res$residuals
  # fityonxi <- lm(yonxi, data=dataset)
  # e.yi <- residuals(fityonxi)
  e.centered <- e.yi - mean(e.yi)

  n <- nrow(dataset)
  bindex1 <- sample(1:n,n,replace = TRUE)
  bindex2 <- sample(1:n,n,replace = TRUE)

  X.star <- data.frame(xi[bindex2,])
  colnames(X.star) <- "x"
  newdata <- data.frame(X.star)
  eta.star <- as.matrix(e.centered[bindex1])

  Y.star <- predict_yonxi(fityonxi, newdata) + eta.star
  newdata["y"] <- Y.star
  res.star <- fit_resid_yonxi(newdata)
  fit.star <- res.star$model
  e.star <- res.star$residuals

  # Y.star <- predict(fityonxi,newdata = newdata) + eta.star
  # fit.star <- lm(Y.star~.,data = newdata)
  # e.star <- residuals(fit.star)

  thetahat.star <- est.fun1(X.star[,1],e.star,n)

  return(list(thetahat.star,thetahat.star >= theta))
}

verify_boot <- function(i, j, sampsize, main_dir, fit_resid_xiony, predict_xiony, fit_resid_yonxi, predict_yonxi, nsubsamp = 100)
{
  do_one(i, j, sampsize, main_dir, fit_resid_xiony, predict_xiony, fit_resid_yonxi, predict_yonxi, nsubsamp = nsubsamp)
}

do_one <- function(i, j, sampsize, main_dir, fit_resid_xiony, predict_xiony, fit_resid_yonxi, predict_yonxi, nsubsamp = 100)
{
  sum_pvals <- 0
  seed.vec <- seq(1, nsubsamp)
  seed <- seed.vec[i]
  main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
  current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",sampsize))
  xony_dir <- file.path(current_dir,"xony")
  yonx_dir <- file.path(current_dir,"yonx")
  dataset <- read.csv(paste0(current_dir,"/dataset.csv"),header = T)[,-1]

  theta_xony <- read.csv(paste0(xony_dir,"/thetahat.csv"),header = T)[,-1]
  theta_yonx <- read.csv(paste0(yonx_dir,"/thetahat.csv"),header = T)[,-1]

  seed.id <- j
  res_xony <- do.one.xony.seperate(seed.id,dataset,theta_xony,fit_resid_xiony, predict_xiony)
  sum_pvals <- sum_pvals + res_xony[[2]]

  write.csv(res_xony[[1]],paste0(xony_dir,"/btheta",seed.id,".csv"))
  write.csv(res_xony[[2]],paste0(xony_dir,"/bp",seed.id,".csv"))

  res_yonx <- do.one.yonx.seperate(seed.id,dataset,theta_yonx,fit_resid_yonxi, predict_yonxi)
  sum_pvals <- sum_pvals + res_yonx[[2]]

  write.csv(res_yonx[[1]],paste0(yonx_dir,"/btheta",seed.id,".csv"))
  write.csv(res_yonx[[2]],paste0(yonx_dir,"/bp",seed.id,".csv"))
  return(sum_pvals)
}

do_bootstrap <- function(main_dir, sampsize, fit_resid_xony, predict_xony, fit_resid_yonx, predict_yonx, run_parallel = T, nsubsamp = 100){
  nboot <- 100
  seed.vec <- seq(1, nsubsamp)
  i <- 1
  for (i in 1:nsubsamp) {
    i <- i
    trials <- 1:nboot
    if (run_parallel){
      all <- parallel::mclapply(trials,verify_boot,i=i,sampsize = sampsize, nsubsamp = nsubsamp, fit_resid_xiony = fit_resid_xony, predict_xiony = predict_xony, fit_resid_yonxi = fit_resid_yonx, predict_yonxi = predict_yonx, main_dir = main_dir, mc.cores=parallelly::availableCores()-1)
    }else{
      all <- lapply(trials,verify_boot,i=i,sampsize = sampsize, nsubsamp = nsubsamp, fit_resid_xiony = fit_resid_xony, predict_xiony = predict_xony, fit_resid_yonxi = fit_resid_yonx, predict_yonxi = predict_yonx, main_dir = main_dir)
    }
  }

}
