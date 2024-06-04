run_procedure_samp <- function(data, dir, sampsize, fit_resid_yonx, predict_yonx, fit_resid_xony, predict_xony, run_parallel = T, run_lingam = F, clear_files = T, nsubsamp = 100){
  setup <- do_setup(data, dir, sampsize, fit_resid_yonx, fit_resid_xony, run_parallel, nsubsamp)
  boot <- do_bootstrap(dir, sampsize, fit_resid_xony, predict_xony, fit_resid_yonx, predict_yonx, run_parallel)
  pvals <- get_pvals(sampsize, dir, run_parallel)
  cddr_test_samp <- get_cddr_test(sampsize, dir, run_parallel)
  if (run_lingam){
    cddr_lingam_samp <- get_cddr_lingam(sampsize, dir)
  }
  if (clear_files){
    main_dir <- file.path(dir,paste0("samplesize",sampsize))
    unlink(main_dir, recursive = TRUE)
  }
  return(cddr_test_samp)
}
