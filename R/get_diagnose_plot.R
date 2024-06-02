#' @importFrom stats qnorm
#' @importFrom grDevices colors

calc_ci <- function(data, outcome_column, dataset_name, S = 100, ci_level = 0.95) {
  z_value <- qnorm((1 + ci_level) / 2)
  data_means <- data[[outcome_column]]
  sd_hat <- sqrt((data_means * (1 - data_means)) / S)
  lower_bounds <- data_means - z_value*sd_hat
  upper_bounds <- data_means + z_value *sd_hat
  data[sprintf("%s_ci_lower", dataset_name)] = lower_bounds
  data[sprintf("%s_ci_upper", dataset_name)] = upper_bounds
  return(data)
}

get_diagnose_plot <- function(cddr, outcome_columns, dataset_names, is_test = T, S = 100, ci_level = 0.95){
  for (i in 1:length(outcome_columns)){
    cddr <- calc_ci(cddr, outcome_columns[i], dataset_names[i])
  }
  if (is_test){
    # colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
    cddr_diagnose_plot <- ggplot2::ggplot(cddr) +
      ggplot2::theme_bw() +
      ggplot2::geom_line(ggplot2::aes_string(x="samplesizes", y="yonx_and_xony_reject"), color = "#8046A0") +
      ggplot2::geom_line(ggplot2::aes_string(x="samplesizes", y="yonx_and_xony_noreject"), color = "#007A5E") +
      ggplot2::geom_line(ggplot2::aes_string(x="samplesizes", y="yonx_reject_xony_noreject"), color = "#6699CC") +
      ggplot2::geom_line(ggplot2::aes_string(x="samplesizes", y="xony_reject_yonx_noreject"), color = "#CC6600") +
      ggplot2::geom_ribbon(ggplot2::aes_string(x = "samplesizes", ymin = "p11_ci_lower", ymax = "p11_ci_upper"), alpha = 0.3, fill = "#8046A0") +
      ggplot2::geom_ribbon(ggplot2::aes_string(x = "samplesizes", ymin = "p00_ci_lower", ymax = "p00_ci_upper"), alpha = 0.3, fill = "#007A5E") +
      ggplot2::geom_ribbon(ggplot2::aes_string(x = "samplesizes", ymin = "p01_ci_lower", ymax = "p01_ci_upper"), alpha = 0.3, fill = "#6699CC") +
      ggplot2::geom_ribbon(ggplot2::aes_string(x = "samplesizes", ymin = "p10_ci_lower", ymax = "p10_ci_upper"), alpha = 0.3, fill = "#CC6600") +
      ggplot2::labs(
        x = "Subsample Size",
        y = "Causal Outcome Rate",
        color = "Test-based Approach CDDR\nDiagnostic Colors") +
      ggplot2::ylim(-0.05,1.05) +
      ggplot2::scale_color_manual(values = colors)

    return(cddr_diagnose_plot)
  } else {
    # colors <- c("Choose X->Y" = "#CC6600", "Choose Y->X" = "#6699CC")
    cddr_diagnose_plot <- ggplot2::ggplot(cddr) +
      ggplot2::theme_bw() +
      ggplot2::geom_line(ggplot2::aes_string(x="samplesizes", y="order_right"), color = "#CC6600") +
      ggplot2::geom_line(ggplot2::aes_string(x="samplesizes", y="order_left"), color = "#6699CC") +
      ggplot2::geom_ribbon(ggplot2::aes_string(x = "samplesizes", ymin = "d1_ci_lower", ymax = "d1_ci_upper"), alpha = 0.4, fill = "#CC6600") +
      ggplot2::geom_ribbon(ggplot2::aes_string(x = "samplesizes", ymin = "d2_ci_lower", ymax = "d2_ci_upper"), alpha = 0.4, fill = "#6699CC") +
      ggplot2::labs(
        x = "Subsample Size",
        y = "Causal Outcome Rate",
        color = "Lingam CDDR Diagnostic Colors") +
      ggplot2::ylim(-0.05,1.05) +
      ggplot2::scale_color_manual(values = colors)
    return(cddr_diagnose_plot)
  }

}
