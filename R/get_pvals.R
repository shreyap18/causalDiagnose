get_pvals <- function(samplesize, dir, nboot=100, nsubsamp=100){
  seed.vec <- seq(1, nsubsamp)
  p_vals_xy <- rep(NA, nsubsamp)
  p_vals_yx <- rep(NA, nsubsamp)
  # main_dir <- getwd()
  # main_dir <- file.path(main_dir, paste0(file))
  main_dir <- file.path(dir,paste0("samplesize",samplesize))
  for (s in 1:nsubsamp){
    seed <- seed.vec[s]
    p_xony <- rep(NA, nsubsamp)
    p_yonx <- rep(NA, nsubsamp)
    for (b in 1:nboot){
      current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",samplesize))
      xony_dir <- file.path(current_dir,"xony")
      yonx_dir <- file.path(current_dir,"yonx")
      xony_bp <- paste0(xony_dir,"/bp",b,".csv")
      yonx_bp <- paste0(yonx_dir,"/bp",b,".csv")
      tryCatch({
        bp_xony <- read.csv(xony_bp, header = TRUE)[, -1]
        bp_yonx <- read.csv(yonx_bp, header = TRUE)[, -1]
        p_xony[b] <- bp_xony
        p_yonx[b] <- bp_yonx
      }, error = function(e) {
        # Handle the error (e.g., print a message)
        cat("Error reading CSV file:", e$message, "\n")
        # You may choose to set p_xony[b] and p_yonx[b] to a default value or NA
        p_xony[b] <- NA
        p_yonx[b] <- NA
        nboot = nboot-1
      })
    }
    p_val_xony <- sum(p_xony, na.rm=TRUE)/nboot
    p_val_yonx <- sum(p_yonx, na.rm=TRUE)/nboot
    p_vals_xy[s] <- p_val_xony
    p_vals_yx[s] <- p_val_yonx
  }
  dfx <- as.data.frame(p_vals_xy)
  dfy <- as.data.frame(p_vals_yx)
  # main_dir <- getwd()
  # main_dir <- file.path(main_dir, paste0(file))
  write.csv(dfx,file.path(dir,sprintf("p_valsxy_samp%d.csv", samplesize)))
  write.csv(dfy,file.path(dir,sprintf("p_valsyx_samp%d.csv", samplesize)))
  # if (num_b == -1){
  #   write.csv(dfx,file.path(dir,sprintf("p_valsxy_samp%d.csv", samplesize)))
  #   write.csv(dfy,file.path(dir,sprintf("p_valsyx_samp%d.csv", samplesize)))
  # }
  # else{
  #   filexy_name = paste0("p_valsxy_samp", sampsize, "_b", num_b, ".csv")
  #   write.csv(dfx,file.path(dir,filexy_name))
  #   fileyx_name = paste0("p_valsyx_samp", sampsize, "_b", num_b, ".csv")
  #   write.csv(dfy,file.path(dir,fileyx_name))
  # }
}
