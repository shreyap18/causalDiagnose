#' @importFrom stats qnorm
#' @importFrom grDevices colors
#' @importFrom ggplot2 .data

calc_ci <- function(data, outcome_column, dataset_name, nsubsamp = 100, ci_level = 0.95) {
  z_value <- qnorm((1 + ci_level) / 2)
  data_means <- data[[outcome_column]]
  sd_hat <- sqrt((data_means * (1 - data_means)) / nsubsamp)
  lower_bounds <- data_means - z_value*sd_hat
  upper_bounds <- data_means + z_value *sd_hat
  data[sprintf("%s_ci_lower", dataset_name)] = lower_bounds
  data[sprintf("%s_ci_upper", dataset_name)] = upper_bounds
  return(data)
}

#' CDDR Diagnostic: the plotted comparison of the probability of each causal discovery outcome as a function of sample size less than dataset size N.
#'
#' @param cddr A dataframe of the estimated causal discovery outcome rates for each subsample size considered, obtained by running run_procedure
#' @param outcome_columns A vector the names corresponding to the outcome rates in the cddr dataframe
#' @param dataset_names A vector of names corresponding to what one would want to label the possible causal discovery outcome rates
#' @param is_test A logical value on whether using the test-based method, default is True
#' @param nsubsamp A numeric value for S, the number of subsamples, default is 100
#' @param ci_level A numerical value corresponding to the confidence level, default is 0.95
#'
#' @return a ggplot plot object that plots the CDDR Diagnostic
#' @export
#'
#' @examples
#' "Gets the CDDR diagnostic for test-based approach example"
#'
#' cddr <- read.csv("~/Documents/cddr_paper/package/test/cddr_test_example.csv")[,-1]
#' outcome_columns <- c("yonx_and_xony_reject", "yonx_and_xony_noreject",
#' "yonx_reject_xony_noreject", "xony_reject_yonx_noreject")
#' dataset_names <- c("p11", "p00", "p01", "p10")
#' plots <- get_diagnose_plot(cddr, outcome_columns, dataset_names)
#'
#' "Gets the CDDR diagnostic for LiNGAM example"
#' cddr_lingam <- read.csv("~/Documents/cddr_paper/package/test/cddr_lingam_example.csv")[,-1]
#' outcome_columns <- c("order_right", "order_left")
#' dataset_names <- c("d1", "d2")
#' plots <- get_diagnose_plot(cddr_lingam, outcome_columns, dataset_names, is_test = FALSE)
#'
#' "Gets the CDDR diagnostic for example causal discovery method that randomly
#' determines a direction with 0.5 probability"
#'
#' cddr_random <- read.csv("~/Documents/cddr_paper/package/test/cddr_random.csv")[,-1]
#' outcome_columns <- c("order_right", "order_left")
#' dataset_names <- c("d1", "d2")
#' plots <- get_diagnose_plot(cddr_random, outcome_columns, dataset_names, is_test = FALSE)

get_diagnose_plot <- function(cddr, outcome_columns, dataset_names, is_test = T, nsubsamp = 100, ci_level = 0.95){
  for (i in 1:length(outcome_columns)){
    cddr <- calc_ci(cddr, outcome_columns[i], dataset_names[i])
  }
  if (is_test){
    colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
    cddr_diagnose_plot <- ggplot2::ggplot(cddr) +
      ggplot2::theme_bw() +
      ggplot2::geom_line(ggplot2::aes(x=.data$samplesizes, y=.data$yonx_and_xony_reject, color = "both reject" )) +
      ggplot2::geom_line(ggplot2::aes(x=.data$samplesizes, y=.data$yonx_and_xony_noreject, color = "fail to reject both")) +
      ggplot2::geom_line(ggplot2::aes(x=.data$samplesizes, y=.data$yonx_reject_xony_noreject, color = "reject only X -> Y")) +
      ggplot2::geom_line(ggplot2::aes(x=.data$samplesizes, y=.data$xony_reject_yonx_noreject, color = "reject only Y -> X")) +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data$samplesizes, ymin = .data$p11_ci_lower, ymax = .data$p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data$samplesizes, ymin = .data$p00_ci_lower, ymax = .data$p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data$samplesizes, ymin = .data$p01_ci_lower, ymax = .data$p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data$samplesizes, ymin = .data$p10_ci_lower, ymax = .data$p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
      ggplot2::labs(
        x = "Subsample Size",
        y = "Causal Outcome Rate",
        color = "Test-based Approach CDDR\nDiagnostic Colors") +
      ggplot2::ylim(-0.05,1.05) +
      ggplot2::scale_color_manual(values = colors)

    return(cddr_diagnose_plot)
  } else {
    colors <- c("Choose X->Y" = "#CC6600", "Choose Y->X" = "#6699CC")
    cddr_diagnose_plot <- ggplot2::ggplot(cddr) +
      ggplot2::theme_bw() +
      ggplot2::geom_line(ggplot2::aes(x=.data$samplesizes, y=.data$order_right, color = "Choose X->Y")) +
      ggplot2::geom_line(ggplot2::aes(x=.data$samplesizes, y=.data$order_left, color = "Choose Y->X")) +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data$samplesizes, ymin = .data$d1_ci_lower, ymax = .data$d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data$samplesizes, ymin = .data$d2_ci_lower, ymax = .data$d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
      ggplot2::labs(
        x = "Subsample Size",
        y = "Causal Outcome Rate",
        color = "Deterministic Approach\nCDDR Diagnostic Colors") +
      ggplot2::ylim(-0.05,1.05) +
      ggplot2::scale_color_manual(values = colors)
    return(cddr_diagnose_plot)
  }

}
