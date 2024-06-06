#' @importFrom parallelly availableCores
pointest <- function(arg, samplesize, main_dir, subdata_all, fit_resid_yonxi, fit_resid_xiony, seed.vec, run_test)
{
  ## randomization
  seed <- seed.vec[arg]
  current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",samplesize))
  dir.create(current_dir)

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
  if (run_test){
    xony_dir <- file.path(current_dir,"xony")
    yonx_dir <- file.path(current_dir,"yonx")

    dir.create(xony_dir)
    dir.create(yonx_dir)

    X <- randomdataset1[,"x"]
    res_yonx <- fit_resid_yonxi(randomdataset1)
    fityonx <- res_yonx$model
    e.y <- res_yonx$residuals

    theta <- est.fun(X,e.y,nrow(randomdataset1))

    write.csv(theta,file.path(yonx_dir,"thetahat.csv"))
    ############################################################xony######################################
    res_xiony <- fit_resid_xiony(randomdataset1)
    fitxiony <- res_xiony$model
    e.x <- res_xiony$residuals

    x_theta <- est.fun(e.x,randomdataset1[,"y"], nrow(randomdataset1))

    write.csv(x_theta,file.path(xony_dir,"thetahat.csv"))
  }

}


verify_setup <- function(i, sampsize, main_dir, subdata_all, fit_resid_yonx, fit_resid_xony, seed.vec, run_test)
{
  pointest(i, sampsize, main_dir, subdata_all, fit_resid_yonx, fit_resid_xony, seed.vec, run_test)
}

do_setup <- function(data, main_dir, sampsize, fit_resid_yonx, fit_resid_xony, run_parallel = T, num_cores = availableCores()-1, nsubsamp = 100, run_test = T){
  colnames(data) <- c("x","y")
  seed.vec <- seq(1, nsubsamp)
  trials <- 1:nsubsamp
  main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
  dir.create(main_dir)
  if (run_parallel){
    all <- parallel::mclapply(trials, verify_setup, sampsize = sampsize, main_dir = main_dir, subdata_all = data, fit_resid_yonx = fit_resid_yonx, fit_resid_xony = fit_resid_xony, seed.vec = seed.vec, run_test = run_test, mc.cores = num_cores)
  }else{
    all <- lapply(trials, verify_setup, sampsize = sampsize, main_dir = main_dir, subdata_all = data, fit_resid_yonx = fit_resid_yonx, fit_resid_xony = fit_resid_xony, seed.vec = seed.vec, run_test = run_test)
  }
}

