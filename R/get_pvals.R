# get_pvals <- function(samplesize, dir, nboot=100, nsubsamp=100){
#   seed.vec <- seq(1, nsubsamp)
#   p_vals_xy <- rep(NA, nsubsamp)
#   p_vals_yx <- rep(NA, nsubsamp)
#   main_dir <- file.path(dir,paste0("samplesize",samplesize))
#   for (s in 1:nsubsamp){
#     seed <- seed.vec[s]
#     p_xony <- rep(NA, nsubsamp)
#     p_yonx <- rep(NA, nsubsamp)
#     for (b in 1:nboot){
#       current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",samplesize))
#       xony_dir <- file.path(current_dir,"xony")
#       yonx_dir <- file.path(current_dir,"yonx")
#       xony_bp <- paste0(xony_dir,"/bp",b,".csv")
#       yonx_bp <- paste0(yonx_dir,"/bp",b,".csv")
#       tryCatch({
#         bp_xony <- read.csv(xony_bp, header = TRUE)[, -1]
#         bp_yonx <- read.csv(yonx_bp, header = TRUE)[, -1]
#         p_xony[b] <- bp_xony
#         p_yonx[b] <- bp_yonx
#       }, error = function(e) {
#         # Handle the error (e.g., print a message)
#         cat("Error reading CSV file:", e$message, "\n")
#         # You may choose to set p_xony[b] and p_yonx[b] to a default value or NA
#         p_xony[b] <- NA
#         p_yonx[b] <- NA
#         nboot = nboot-1
#       })
#     }
#     p_val_xony <- sum(p_xony, na.rm=TRUE)/nboot
#     p_val_yonx <- sum(p_yonx, na.rm=TRUE)/nboot
#     p_vals_xy[s] <- p_val_xony
#     p_vals_yx[s] <- p_val_yonx
#   }
#   dfx <- as.data.frame(p_vals_xy)
#   dfy <- as.data.frame(p_vals_yx)
#   write.csv(dfx,file.path(dir,sprintf("p_valsxy_samp%d.csv", samplesize)))
#   write.csv(dfy,file.path(dir,sprintf("p_valsyx_samp%d.csv", samplesize)))
# }

get_pvals <- function(samplesize, dir, run_parallel = T, nboot = 100, nsubsamp = 100) {
  seed.vec <- seq(1, nsubsamp)
  p_vals_xy <- rep(NA, nsubsamp)
  p_vals_yx <- rep(NA, nsubsamp)
  main_dir <- file.path(dir, paste0("samplesize", samplesize))

  # Function to process each bootstrap sample
  process_bootstrap <- function(b, xony_dir, yonx_dir) {
    xony_bp <- paste0(xony_dir, "/bp", b, ".csv")
    yonx_bp <- paste0(yonx_dir, "/bp", b, ".csv")
    tryCatch({
      bp_xony <- read.csv(xony_bp, header = TRUE)[, -1]
      bp_yonx <- read.csv(yonx_bp, header = TRUE)[, -1]
      list(bp_xony = bp_xony, bp_yonx = bp_yonx)
    }, error = function(e) {
      # Handle the error
      cat("Error reading CSV file:", e$message, "\n")
      list(bp_xony = NA, bp_yonx = NA)
    })
  }

  # Function to process each subsample
  process_subsample <- function(s) {
    seed <- seed.vec[s]
    current_dir <- file.path(main_dir, paste0("dataset_", seed, "_", samplesize))
    xony_dir <- file.path(current_dir, "xony")
    yonx_dir <- file.path(current_dir, "yonx")

    if (run_parallel){
      # Run the bootstrap process in parallel
      bootstrap_results <- parallel::mclapply(1:nboot, process_bootstrap, xony_dir = xony_dir, yonx_dir = yonx_dir, mc.cores = parallel::detectCores() - 1)
    } else{
      bootstrap_results <- lapply(1:nboot, process_bootstrap, xony_dir = xony_dir, yonx_dir = yonx_dir)
    }

    p_xony <- sapply(bootstrap_results, function(res) res$bp_xony)
    p_yonx <- sapply(bootstrap_results, function(res) res$bp_yonx)

    p_val_xony <- sum(p_xony, na.rm = TRUE) / nboot
    p_val_yonx <- sum(p_yonx, na.rm = TRUE) / nboot

    list(p_val_xony = p_val_xony, p_val_yonx = p_val_yonx)
  }

  if (run_parallel){
    # Run the subsample process in parallel
    subsample_results <- parallel::mclapply(1:nsubsamp, process_subsample, mc.cores = parallel::detectCores() - 1)
  }else{
    subsample_results <- lapply(1:nsubsamp, process_subsample)
  }

  p_vals_xy <- sapply(subsample_results, function(res) res$p_val_xony)
  p_vals_yx <- sapply(subsample_results, function(res) res$p_val_yonx)

  dfx <- as.data.frame(p_vals_xy)
  dfy <- as.data.frame(p_vals_yx)

  write.csv(dfx, file.path(dir, sprintf("p_valsxy_samp%d.csv", samplesize)))
  write.csv(dfy, file.path(dir, sprintf("p_valsyx_samp%d.csv", samplesize)))
}
