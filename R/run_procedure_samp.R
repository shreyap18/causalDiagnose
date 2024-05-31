run_procedure_samp <- function(data, dir, sampsize, yonx, xiony, run_lingam = F, clear_files = T, nsubsamp = 100){
  setup <- do_setup(data, dir, sampsize, yonx, xiony, nsubsamp)
  boot <- do_bootstrap(dir, sampsize, xiony, yonx)
  pvals <- get_pvals(sampsize, dir)
  cddr_test_samp <- get_cddr_test(sampsize, dir)
  if (run_lingam){
    cddr_lingam_samp <- get_cddr_lingam(sampsize, dir)
  }
  if (clear_files){
    main_dir <- file.path(dir,paste0("samplesize",sampsize))
    unlink(main_dir, recursive = TRUE)
  }
  return(cddr_test_samp)
}
