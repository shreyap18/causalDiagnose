get_cddr_test <- function(sampsize, main_dir, run_parallel = T, nsubsamp=100, alpha = 0.05){
  dfyx <- read.csv(file.path(main_dir,sprintf("p_valsyx_samp%d.csv", sampsize)))[,-1]
  dfxy <- read.csv(file.path(main_dir,sprintf("p_valsxy_samp%d.csv", sampsize)))[,-1]
  reject_yonx_and_xony <- 0
  noreject_yonx_and_xony <- 0
  reject_yonx_noreject_xony <- 0
  reject_xony_noreject_yonx <- 0
  for (s in 1:nsubsamp){
    p_val_xony <- dfxy[s]
    p_val_yonx <- dfyx[s]
    if ((p_val_xony <= alpha) & (p_val_yonx <= alpha)){
      reject_yonx_and_xony <- reject_yonx_and_xony + 1
    }
    if ((p_val_xony > alpha) & (p_val_yonx > alpha)){
      noreject_yonx_and_xony <- noreject_yonx_and_xony + 1
    }
    if ((p_val_xony > alpha) & (p_val_yonx <= alpha)){
      reject_yonx_noreject_xony <- reject_yonx_noreject_xony + 1
    }
    if ((p_val_xony <= alpha) & (p_val_yonx > alpha)){
      reject_xony_noreject_yonx <- reject_xony_noreject_yonx + 1
    }
  }
  df_cddr <- data.frame(c(sampsize, reject_yonx_and_xony/nsubsamp, noreject_yonx_and_xony/nsubsamp, reject_yonx_noreject_xony/nsubsamp, reject_xony_noreject_yonx/nsubsamp))
  write.csv(df_cddr,file.path(main_dir,sprintf("cddr_samp%d.csv", sampsize)))
  cddr_return <- data.frame(samplesizes = sampsize, yonx_and_xony_reject = reject_yonx_and_xony/nsubsamp, yonx_and_xony_noreject = noreject_yonx_and_xony/nsubsamp, yonx_reject_xony_noreject = reject_yonx_noreject_xony/nsubsamp, xony_reject_yonx_noreject = reject_xony_noreject_yonx/nsubsamp)

  return(cddr_return)
}

# get_cddr_test <- function(sampsize, main_dir, nsubsamp = 100, alpha = 0.05) {
#   dfyx <- read.csv(file.path(main_dir, sprintf("p_valsyx_samp%d.csv", sampsize)))[, -1]
#   dfxy <- read.csv(file.path(main_dir, sprintf("p_valsxy_samp%d.csv", sampsize)))[, -1]
#
#   calculate_rejections <- function(s) {
#     p_val_xony <- dfxy[s]
#     p_val_yonx <- dfyx[s]
#     c(
#       yonx_and_xony_reject = (p_val_xony <= alpha) & (p_val_yonx <= alpha),
#       yonx_and_xony_noreject = (p_val_xony > alpha) & (p_val_yonx > alpha),
#       yonx_reject_xony_noreject = (p_val_xony > alpha) & (p_val_yonx <= alpha),
#       xony_reject_yonx_noreject = (p_val_xony <= alpha) & (p_val_yonx > alpha)
#     )
#   }
#
#   # Use mclapply to parallelize the loop
#   rejections <- parallel::mclapply(1:nsubsamp, calculate_rejections, mc.cores = parallel::detectCores() - 1)
#
#   # Summarize the rejections
#   rejections_matrix <- do.call(rbind, rejections)
#   rejection_rates <- colSums(rejections_matrix) / nsubsamp
#
#   # Create and write the result dataframe
#   df_cddr <- data.frame(c(sampsize, rejection_rates))
#   colnames(df_cddr) <- c("sampsize", "yonx_and_xony_reject", "yonx_and_xony_noreject", "yonx_reject_xony_noreject", "xony_reject_yonx_noreject")
#   write.csv(df_cddr, file.path(main_dir, sprintf("cddr_samp%d.csv", sampsize)))
#
#   return(data.frame(sampsize = sampsize, rejection_rates))
# }

