# combine_cddr_samp <- function(num_b, samplesizes, file, names_col, name_file, name_out, test = T){
#   cddr <- data.frame(matrix(ncol = length(names_col), nrow = 0))
#   colnames(cddr) <- names_col
#   cddr_samp <- NA
#   for (sampsize in samplesizes){
#     main_dir <- getwd()
#     main_dir <- file.path(main_dir,paste0(file))
#     if (num_b == -1){
#       cddr_samp <- read.csv(paste0(main_dir,"/",name_file, sprintf("%d.csv", sampsize)),header = T)[,-1]
#     } else{
#       cddr_samp <- read.csv(paste0(main_dir,"/",name_file, sampsize, "_b", num_b, ".csv"),header = T)[,-1]
#     }
#     cddr <- rbind(cddr, cddr_samp)
#   }
#   colnames(cddr) <- names_col
#   if (num_b == -1){
#     write.csv(cddr,file.path(main_dir,sprintf("%s.csv", name_out)))
#   }
#   else{
#     file_name = paste0(name_out, "_b", num_b, ".csv")
#     write.csv(cddr,file.path(main_dir,file_name))
#   }
#   main_dir <- getwd()
#   main_dir <- file.path(main_dir,paste0(file))
#   files_to_delete <- list.files(main_dir, pattern = name_file)
#   # Check if any files match the pattern
#   if (length(files_to_delete) > 0) {
#     # Loop through the files and remove them
#     for (f in files_to_delete) {
#       print(f)
#       file.remove(file.path(main_dir, f))
#     }
#   } else {
#     print("No files found matching the pattern.")
#   }
#   return(cddr)
# }
# run_procedure(subdata, main_dir, c(4,5,6), yonx, xiony, T)
run_procedure <- function(data, dir, sampsizes, yonx, xiony, names_col_cddr, name_files, name_out, run_lingam = F, clear_files = T, nsubsamp = 100, early_stop = T){
  early_stop_iter = 0
  names_col_pval <- 1:nsubsamp
  cddr <- data.frame(matrix(ncol = length(names_col_cddr), nrow = 0))
  colnames(cddr) <- names_col_cddr
  cddr_samp <- NA

  pvalxy <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
  colnames(pvalxy) <- names_col_pval
  pvalxy_samp <- NA

  pvalyx <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
  colnames(pvalyx) <- names_col_pval
  pvalyx_samp <- NA
  for (sampsize in sampsizes){
    cddr_samp <- run_procedure_samp(data, dir, sampsize, yonx, xiony, run_lingam, clear_files, nsubsamp)
    pvalxy_samp <- read.csv(paste0(dir,"/",name_files[2], sprintf("%d.csv", sampsize)),header = T)[,-1]
    pvalyx_samp <- read.csv(paste0(dir,"/",name_files[3], sprintf("%d.csv", sampsize)),header = T)[,-1]
    cddr <- rbind(cddr, cddr_samp)
    pvalxy <- rbind(pvalxy, pvalxy_samp)
    pvalyx <- rbind(pvalyx, pvalyx_samp)
    if (cddr_samp$yonx_and_xony_reject == 1){
      early_stop_iter = early_stop_iter + 1
      if (early_stop_iter > 1){
        break
      }
    }
  }
  colnames(cddr) <- names_col_cddr
  write.csv(cddr,file.path(dir,sprintf("%s.csv", name_out[1])))

  colnames(pvalxy) <- names_col_pval
  write.csv(pvalxy,file.path(dir,sprintf("%s.csv", name_out[2])))

  colnames(pvalyx) <- names_col_pval
  write.csv(pvalyx,file.path(dir,sprintf("%s.csv", name_out[3])))

  if (clear_files){
    for (name_file in name_files){
      files_to_delete <- list.files(dir, pattern = name_file)
      if (length(files_to_delete) > 0) {
        # Loop through the files and remove them
        for (f in files_to_delete) {
          print(f)
          file.remove(file.path(main_dir, f))
        }
      } else {
        print("No files found matching the pattern.")
      }
    }
  }
  return(cddr)
}
