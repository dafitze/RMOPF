#' @import dplyr
#' @import ggplot2
#' @export

plot_pf <- function(df, mu = 0.0) {
  if (length(df$time) == 1){
    df |>
      group_by(vpn, stimulus) |>
      summarise(mean_response = mean(response)) |>
      ggplot(aes(stimulus, mean_response)) +
      geom_point() +
      geom_smooth(
        method = "glm", se = FALSE,
        method.args = list(family = "binomial")
      ) +
      geom_vline(xintercept = mu, linetype = "dashed") +
      scale_color_viridis_d(begin = 0, end = 0.8) +
      facet_wrap(~vpn) +
      lims(y = c(0,1)) +
      theme_clean()
  } else {
    df |>
      group_by(vpn, time, stimulus) |>
      summarise(mean_response = mean(response)) |>
      ggplot(aes(stimulus, mean_response, color = time)) +
      geom_point() +
      geom_smooth(
        method = "glm", se = FALSE,
        method.args = list(family = "binomial")
      ) +
      geom_vline(xintercept = mu, linetype = "dashed") +
      scale_color_viridis_d(begin = 0, end = 0.8) +
      facet_wrap(~vpn) +
      lims(y = c(0,1)) +
      theme_clean()
  }
}
