get_cddr_test <- function(sampsize, main_dir, nsubsamp=100, alpha = 0.05){
  # main_dir <- getwd()
  # main_dir <- file.path(main_dir, paste0(file))
  dfyx <- read.csv(file.path(main_dir,sprintf("p_valsyx_samp%d.csv", sampsize)))[,-1]
  dfxy <- read.csv(file.path(main_dir,sprintf("p_valsxy_samp%d.csv", sampsize)))[,-1]

  # if (num_b == -1){
  #   dfyx <- read.csv(file.path(main_dir,sprintf("p_valsyx_samp%d.csv", sampsize)))[,-1]
  #   dfxy <- read.csv(file.path(main_dir,sprintf("p_valsxy_samp%d.csv", sampsize)))[,-1]
  # }
  # else{
  #   fileyx_name = paste0("p_valsyx_samp", sampsize, "_b", num_b, ".csv")
  #   dfyx <- read.csv(file.path(main_dir, fileyx_name))[,-1]
  #   filexy_name = paste0("p_valsxy_samp", sampsize, "_b", num_b, ".csv")
  #   dfxy <- read.csv(file.path(main_dir, filexy_name))[,-1]
  # }
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
  # if (num_b == -1){
  #   write.csv(df_cddr,file.path(main_dir,sprintf("cddr_samp%d.csv", sampsize)))
  # }
  # else{
  #   file_name = paste0("cddr_samp", sampsize, "_b", num_b, ".csv")
  #   write.csv(df_cddr,file.path(main_dir,file_name))
  # }
  cddr_return <- data.frame(sampsize = sampsize, yonx_and_xony_reject = reject_yonx_and_xony/nsubsamp, yonx_and_xony_noreject = noreject_yonx_and_xony/nsubsamp, yonx_reject_xony_noreject = reject_yonx_noreject_xony/nsubsamp, xony_reject_yonx_noreject = reject_xony_noreject_yonx/nsubsamp)

  return(cddr_return)
}
