#' Get estimated causal discovery outcome rates as a function of subsample size
#'
#' @param data A dataframe of size N with two columns: x and y, where x->y is the hypothesized direction
#' @param dir A string of the directory where want to save the csv file of estimated causal discovery outcome rates for each subsample size
#' @param sampsizes A vector of subsample sizes where the largest subsample size is less than equal to N
#' @param names_col_cddr A vector of names for each of the columns in the estimated causal discovery outcome rates dataframe
#' @param name_files A vector of names for the saved files for each subsample size (just the root name, such as cddr_samp)
#' @param name_out A vector of names for the output files corresponding to the estimated causal discovery outcome rates dataframe as well as the pvalues
#' @param fit_resid_yonx A function that takes in a dataframe with variable x, fits a model of choice on the data (e.g. linear regression) where y is the outcome and x is the predictor, and function outputs a list with two components: model, which is the model object, and residuals, the residuals after fitting the model on the data, default is a function that returns NA as this would only be relevant for test-based method
#' @param predict_yonx A function that takes in a model object (where y is the outcome and x is the predictor) and newdata with two variables x and y, gets predictions based on the model for the newdata and returns these predictions, default is a function that returns NA as this would only be relevant for test-based method
#' @param fit_resid_xony A function that takes in a dataframe with variable y, fits a model of choice on the data (e.g. linear regression) where x is the outcome and y is the predictor, and function outputs a list with two components: model, which is the model object, and residuals, the residuals after fitting the model on the data, default is a function that returns NA as this would only be relevant for test-based method
#' @param predict_xony A function that takes in a model object (where x is the outcome and y is the predictor) and newdata with two variables x and y, gets predictions based on the model for the newdata and returns these predictions, default is a function that returns NA as this would only be relevant for test-based method
#' @param run_parallel A logical value on whether to run items using parallel processing, default is True
#' @param num_cores A numeric value for the number of cores, default is availableCores()-1
#' @param run_causal_method A function that takes in a dataframe with two variables, x and y and returns the estimated causal ordering as a vector (e.g. c(1,2) for x->y or c(2,1) for y->x), default is a function that returns NA as this is only relevant for deterministic methods (not the test-based method)
#' @param run_lingam A logical value on whether to run LiNGAM method, default is True
#' @param run_test A logical value on whether to run test-based method, default is True
#' @param clear_files A logical value on whether to clear the files generated in the bootstrap procedure for each subsample size, default is True
#' @param nsubsamp A numeric value for S, the number of subsamples, default is 100
#' @param nboot A numeric value for the number of bootstrap repetitions, default is 100
#' @param alpha A numeric value for the alpha level (probability of type 1 error), default is 0.05
#' @param early_stop A logical value on whether to stop the method early for the test-based method if the rate of rejecting both directions converges to 1, default is True
#' @param early_stop_count A numerical value on what the threshold is to stop early (e.g after 3 subsample sizes where the rate of rejecting in both directions is 1), default is 2
#' @param early_stop_thresh A numerical value on what the numerical threshold is to stop early (e.g stop if the rate of rejecting in both directions is 1), default is 1
#' @param fill_rest_subsamp A logical value on whether to fill the rest of the subsamples with early_stop_thresh (e.g 1) when early stopping is used, default is False
#'
#' @return A dataframe of the estimated causal discovery outcome rates for each subsample size considered. Also locally saves these probabilities as well as the p-values into the corresponding name_out files
#'
#' @importFrom stats coef lm predict residuals sd
#' @importFrom utils read.csv write.csv
#' @importFrom parallelly availableCores
#'
#' @export
#'
#' @examples
#'
#' sampsizes <- c(seq(20, 40, 10))
#' fit_resid_yonx <- function(data) {
#'   model <- lm(formula("y~x"), data = data)
#'   resids <- residuals(model)
#'   model_obs <- list(model = model, residuals = resids)
#'   return(model_obs)
#'   }
#' predict_yonx <- function(model, data) {
#'   preds <- predict(model,newdata = data)
#'   return(preds)
#'   }
#' fit_resid_xony <- function(data) {
#'   model <- lm(formula("x~y"), data = data)
#'   resids <- residuals(model)
#'   model_obs <- list(model = model, residuals = resids)
#'   return(model_obs)
#'   }
#' predict_xony <- function(model, data) {
#'   preds <- predict(model,newdata = data)
#'   return(preds)
#'   }
#' names_col_cddr <- c("samplesizes", "yonx_and_xony_reject",
#' "yonx_and_xony_noreject", "yonx_reject_xony_noreject",
#' "xony_reject_yonx_noreject")
#' main_dir <- "~/Documents/cddr_paper/package/test"
#' name_files <- c("cddr_samp", "p_valsxy_samp", "p_valsyx_samp")
#' name_out <- c("cddr_test", "p_valsxy", "p_valsyx")
#' cddr <- run_procedure(pop_cal, main_dir, sampsizes, names_col_cddr, name_files,
#' name_out, fit_resid_yonx, predict_yonx, fit_resid_xony, predict_xony)
#'
#'
#' sampsizes <- c(seq(20, 40, 10))
#' names_col_cddr <- c("samplesizes", "order_right", "order_left")
#' main_dir <- "~/Documents/cddr_paper/package/test"
#' name_files <- c("cddr_deter_samp")
#' name_out <- c("cddr_lingam")
#' cddr_lingam <- run_procedure(pop_cal, main_dir, sampsizes, names_col_cddr,
#' name_files, name_out, run_test = FALSE, early_stop_thresh = 0.95)
#'
#'
#' sampsizes <- c(seq(20, 60, 10))
#' names_col_cddr <- c("samplesizes", "order_right", "order_left")
#' main_dir <- "~/Documents/cddr_paper/package/test"
#' name_files <- c("cddr_deter_samp")
#' name_out <- c("cddr_random")
#' run_causal_method <- function(data){
#'   p <- rbinom(1, 1, p = 0.6)
#'     if (p == 0){
#'       return(c(1,2))}
#'    else{
#'       return(c(2,1))
#'       }}
#' cddr_random <- run_procedure(pop_cal, main_dir, sampsizes, names_col_cddr,name_files,
#' name_out, run_test = FALSE, run_lingam = FALSE, run_causal_method = run_causal_method,
#' early_stop_thresh = 0.95)

run_procedure <- function(data, dir, sampsizes, names_col_cddr, name_files, name_out, fit_resid_yonx = function(x) {NA}, predict_yonx = function(x) {NA}, fit_resid_xony = function(x) {NA}, predict_xony = function(x) {NA}, run_parallel = T, num_cores = availableCores()-1, run_causal_method = function(x) {NA}, run_lingam = T, run_test = T, clear_files = T, nsubsamp = 100, nboot = 100, alpha = 0.05, early_stop = T, early_stop_count = 2, early_stop_thresh = 1, fill_rest_subsamp = F){
  early_stop_iter = 0
  cddr <- data.frame(matrix(ncol = length(names_col_cddr), nrow = 0))
  colnames(cddr) <- names_col_cddr
  cddr_samp <- NA

  if (run_test){
    names_col_pval <- 1:nsubsamp
    pvalxy <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
    colnames(pvalxy) <- names_col_pval
    pvalxy_samp <- NA

    pvalyx <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
    colnames(pvalyx) <- names_col_pval
    pvalyx_samp <- NA
  }

  for (s in 1:length(sampsizes)){
    sampsize <- sampsizes[s]
    cddr_samp <- run_procedure_samp(data, dir, sampsize, fit_resid_yonx, predict_yonx, fit_resid_xony, predict_xony, run_parallel, num_cores, run_causal_method, run_lingam, run_test, clear_files, nsubsamp, nboot, alpha)
    colnames(cddr_samp) <- names_col_cddr
    cddr <- rbind(cddr, cddr_samp)
    if (run_test){
      pvalxy_samp <- read.csv(paste0(dir,"/",name_files[2], sprintf("%d.csv", sampsize)),header = T)[,-1]
      pvalyx_samp <- read.csv(paste0(dir,"/",name_files[3], sprintf("%d.csv", sampsize)),header = T)[,-1]
      pvalxy <- rbind(pvalxy, pvalxy_samp)
      pvalyx <- rbind(pvalyx, pvalyx_samp)
      if (cddr_samp$yonx_and_xony_reject >= early_stop_thresh){
        early_stop_iter = early_stop_iter + 1
        if (early_stop_iter > early_stop_count){
          if (fill_rest_subsamp){
            remain_s <- length(sampsizes)-s
            cddr_rest <- data.frame(
              samplesizes = sampsizes[(s + 1):length(sampsizes)],
              yonx_and_xony_reject = rep(early_stop_thresh, remain_s),
              yonx_and_xony_noreject = rep(1-early_stop_thresh, remain_s),
              yonx_reject_xony_noreject = rep(1-early_stop_thresh, remain_s),
              xony_reject_yonx_noreject = rep(1-early_stop_thresh, remain_s))
            colnames(cddr_rest) <- names_col_cddr
            cddr <- rbind(cddr, cddr_rest)
          }
          break
        }
      }
    } else{
      if (cddr_samp$order_right >= early_stop_thresh){
        early_stop_iter = early_stop_iter + 1
        if (early_stop_iter > early_stop_count){
          if (fill_rest_subsamp){
            remain_s <- length(sampsizes)-s
            cddr_rest <- data.frame(
              samplesizes = sampsizes[(s + 1):length(sampsizes)],
              order_right = rep(early_stop_thresh, remain_s),
              order_right = rep(1-early_stop_thresh, remain_s))
            colnames(cddr_rest) <- names_col_cddr
            cddr <- rbind(cddr, cddr_rest)
          }
          break
        }
      }
    }
  }
  colnames(cddr) <- names_col_cddr
  write.csv(cddr,file.path(dir,sprintf("%s.csv", name_out[1])))
  if (run_test){
    colnames(pvalxy) <- names_col_pval
    write.csv(pvalxy,file.path(dir,sprintf("%s.csv", name_out[2])))
    colnames(pvalyx) <- names_col_pval
    write.csv(pvalyx,file.path(dir,sprintf("%s.csv", name_out[3])))
  }

  if (clear_files){
    for (name_file in name_files){
      files_to_delete <- list.files(dir, pattern = name_file)
      if (length(files_to_delete) > 0) {
        # Loop through the files and remove them
        for (f in files_to_delete) {
          # print(f)
          file.remove(file.path(dir, f))
        }
      } else {
        # print("No files found matching the pattern.")
      }
    }
  }
  return(cddr)
}

# run_procedure <- function(data, dir, sampsizes, yonx, xiony, names_col_cddr, name_files, name_out, run_lingam = FALSE, clear_files = TRUE, nsubsamp = 100, early_stop = TRUE) {
#   names_col_pval <- 1:nsubsamp
#   cddr <- data.frame(matrix(ncol = length(names_col_cddr), nrow = 0))
#   colnames(cddr) <- names_col_cddr
#
#   pvalxy <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
#   colnames(pvalxy) <- names_col_pval
#
#   pvalyx <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
#   colnames(pvalyx) <- names_col_pval
#
#   # Define the function to process each sample size
#   process_sampsize <- function(sampsize) {
#     cddr_samp <- run_procedure_samp(data, dir, sampsize, yonx, xiony, run_lingam, clear_files, nsubsamp)
#     pvalxy_samp <- read.csv(paste0(dir, "/", name_files[2], sprintf("%d.csv", sampsize)), header = TRUE)[, -1]
#     pvalyx_samp <- read.csv(paste0(dir, "/", name_files[3], sprintf("%d.csv", sampsize)), header = TRUE)[, -1]
#
#     list(cddr_samp = cddr_samp, pvalxy_samp = pvalxy_samp, pvalyx_samp = pvalyx_samp, early_stop = cddr_samp$yonx_and_xony_reject)
#   }
#
#   # Run the process_sampsize function in parallel
#   results <- parallel::mclapply(sampsizes, process_sampsize, mc.cores = parallel::detectCores() - 1)
#
#   # Combine the results and check for early stopping
#   for (result in results) {
#     if (is.null(result)) {
#       break
#     }
#
#     cddr <- rbind(cddr, result$cddr_samp)
#     pvalxy <- rbind(pvalxy, result$pvalxy_samp)
#     pvalyx <- rbind(pvalyx, result$pvalyx_samp)
#   }
#
#   # Write results to CSV files
#   colnames(cddr) <- names_col_cddr
#   write.csv(cddr, file.path(dir, sprintf("%s.csv", name_out[1])))
#
#   colnames(pvalxy) <- names_col_pval
#   write.csv(pvalxy, file.path(dir, sprintf("%s.csv", name_out[2])))
#
#   colnames(pvalyx) <- names_col_pval
#   write.csv(pvalyx, file.path(dir, sprintf("%s.csv", name_out[3])))
#
#   # Clear files if needed
#   if (clear_files) {
#     for (name_file in name_files) {
#       files_to_delete <- list.files(dir, pattern = name_file)
#       if (length(files_to_delete) > 0) {
#         for (f in files_to_delete) {
#           print(f)
#           file.remove(file.path(dir, f))
#         }
#       } else {
#         print("No files found matching the pattern.")
#       }
#     }
#   }
#
#   return(cddr)
# }

# ## parallel version with early stopping
# run_procedure <- function(data, dir, sampsizes, yonx, xiony, names_col_cddr, name_files, name_out, run_lingam = FALSE, clear_files = TRUE, nsubsamp = 100, run_early_stop = TRUE) {
#   names_col_pval <- 1:nsubsamp
#   cddr <- data.frame(matrix(ncol = length(names_col_cddr), nrow = 0))
#   colnames(cddr) <- names_col_cddr
#
#   pvalxy <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
#   colnames(pvalxy) <- names_col_pval
#
#   pvalyx <- data.frame(matrix(ncol = length(names_col_pval), nrow = 0))
#   colnames(pvalyx) <- names_col_pval
#
#   # Define the function to process each sample size
#   process_sampsize <- function(sampsize) {
#     cddr_samp <- run_procedure_samp(data, dir, sampsize, yonx, xiony, run_lingam, clear_files, nsubsamp)
#     pvalxy_samp <- read.csv(paste0(dir, "/", name_files[2], sprintf("%d.csv", sampsize)), header = TRUE)[, -1]
#     pvalyx_samp <- read.csv(paste0(dir, "/", name_files[3], sprintf("%d.csv", sampsize)), header = TRUE)[, -1]
#
#     list(cddr_samp = cddr_samp, pvalxy_samp = pvalxy_samp, pvalyx_samp = pvalyx_samp, early_stop = cddr_samp$yonx_and_xony_reject)
#   }
#
#   # Launch parallel processes
#   results <- list()
#   for (i in seq_along(sampsizes)) {
#     sampsize <- sampsizes[i]
#     results[[i]] <- parallel::mcparallel(process_sampsize(sampsize))
#   }
#
#   # Collect results and check for early stopping
#   for (i in seq_along(results)) {
#     result <- parallel::mccollect(results[[i]])[[1]]
#
#     if (is.null(result)) {
#       break
#     }
#
#     cddr <- rbind(cddr, result$cddr_samp)
#     pvalxy <- rbind(pvalxy, result$pvalxy_samp)
#     pvalyx <- rbind(pvalyx, result$pvalyx_samp)
#
#     if (result$early_stop == 1 && run_early_stop) {
#       break
#     }
#   }
#
#   # Write results to CSV files
#   colnames(cddr) <- names_col_cddr
#   write.csv(cddr, file.path(dir, sprintf("%s.csv", name_out[1])))
#
#   colnames(pvalxy) <- names_col_pval
#   write.csv(pvalxy, file.path(dir, sprintf("%s.csv", name_out[2])))
#
#   colnames(pvalyx) <- names_col_pval
#   write.csv(pvalyx, file.path(dir, sprintf("%s.csv", name_out[3])))
#
#   # Clear files if needed
#   if (clear_files) {
#     for (name_file in name_files) {
#       files_to_delete <- list.files(dir, pattern = name_file)
#       if (length(files_to_delete) > 0) {
#         for (f in files_to_delete) {
#           print(f)
#           file.remove(file.path(dir, f))
#         }
#       } else {
#         print("No files found matching the pattern.")
#       }
#     }
#   }
#
#   return(cddr)
# }

