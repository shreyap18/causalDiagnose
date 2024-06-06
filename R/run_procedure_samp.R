#' @importFrom parallelly availableCores

run_procedure_samp <- function(data, dir, sampsize, fit_resid_yonx = function(x) {NA}, predict_yonx = function(x) {NA}, fit_resid_xony = function(x) {NA}, predict_xony = function(x) {NA}, run_parallel = T, num_cores = availableCores()-1, run_causal_method = function(x) {NA}, run_lingam = T, run_test = T, clear_files = T, nsubsamp = 100, nboot = 100, alpha = 0.05){
  setup <- do_setup(data, dir, sampsize, fit_resid_yonx, fit_resid_xony, run_parallel, num_cores, nsubsamp, run_test)
  if (run_test){
    boot <- do_bootstrap(dir, sampsize, fit_resid_xony, predict_xony, fit_resid_yonx, predict_yonx, run_parallel, num_cores, nsubsamp, nboot)
    pvals <- get_pvals(sampsize, dir, run_parallel, num_cores)
    cddr_samp <- get_cddr_test(sampsize, dir, nsubsamp, alpha)
  } else{
    cddr_samp <- get_cddr_deterministic(sampsize, dir, run_causal_method, run_lingam, nsubsamp)
  }
  if (clear_files){
    main_dir <- file.path(dir,paste0("samplesize",sampsize))
    unlink(main_dir, recursive = TRUE)
  }
  return(cddr_samp)
}
