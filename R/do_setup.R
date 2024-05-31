pointest <- function(arg, samplesize, main_dir, subdata_all, yonx, xiony, seed.vec)
{
  ## randomization
  seed <- seed.vec[arg]
  current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",samplesize))
  dir.create(current_dir)
  xony_dir <- file.path(current_dir,"xony")
  yonx_dir <- file.path(current_dir,"yonx")

  dir.create(xony_dir)
  dir.create(yonx_dir)

  ## sample w replacement
  index <- sample(1:nrow(subdata_all),samplesize, replace = T)
  randomdataset1 <- subdata_all[index,]

  write.csv(randomdataset1,file.path(current_dir,"dataset.csv"))

  ############################################################yonx############################################################
  ####################combined
  est.fun <- function(X,e,n)
  {
    K <- L <- matrix(NA,nrow = n,ncol = n)
    I <- matrix(1,nrow=n,ncol=1)
    H <- diag(1,nrow = n,ncol = n)-I %*% t(I)/n

    L <- exp(-(e%*%t(I) - I%*%t(e))^2)
    K <- exp(-(X%*%t(I) - I%*%t(X))^2)

    sum(diag(K%*%H%*%L%*%H))/n^2
  }

  # yonx <- formula("mbdi~sleep")
  # ## for anm for bmd data
  # yonx <- formula("mbdi~splines::bs(sleep, knots = c(12, 16))")
  X <- randomdataset1[,"x"]
  fityonx <- lm(yonx, data=randomdataset1)
  beta <- coef(fityonx)
  e.y <- residuals(fityonx)
  theta <- est.fun(X,e.y,nrow(randomdataset1))

  write.csv(theta,file.path(yonx_dir,"thetahat.csv"))
  ####################

  ############################################################xony######################################
  # xiony <- formula("sleep~mbdi")
  # ## for anm for bmd data
  # xiony <- formula("sleep~splines::bs(mbdi, knots = c(0.8))")
  fitxiony <- lm(xiony, data=randomdataset1)
  betai <- coef(fitxiony)
  e.x <- residuals(fitxiony)

  x_theta <- est.fun(e.x,randomdataset1[,"y"], nrow(randomdataset1))

  write.csv(x_theta,file.path(xony_dir,"thetahat.csv"))
}


verify_setup <- function(i, sampsize, main_dir, subdata_all, yonx, xiony, seed.vec)
{
  pointest(i, sampsize, main_dir, subdata_all, yonx, xiony, seed.vec)
}

do_setup <- function(data, main_dir, sampsize, yonx, xiony, nsubsamp = 100){
  colnames(data) <- c("x","y")
  seed.vec <- seq(1, nsubsamp)
  trials <- 1:nsubsamp
  main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
  dir.create(main_dir)
  lapply(trials, verify_setup, sampsize = sampsize, main_dir = main_dir, subdata_all = data, yonx = yonx, xiony = xiony, seed.vec = seed.vec)
}

