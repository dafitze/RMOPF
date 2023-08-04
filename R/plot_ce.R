#' @import brms
#' @import dplyr
#' @export

plot_ce = function(fit, plot_data = NA, index = 1, title = "Title"){
  if (is_tibble(plot_data)){

    d_summary = plot_data |>
      group_by(time, stimulus) %>%
      summarise(mean_response = mean(response)) %>%
      mutate(lower__ = 0,
             upper__ = 0,
             effect2__ = time)

    plot(conditional_effects(fit), plot = F)[[index]] +
      geom_point(data = d_summary, aes(stimulus, mean_response), size = 3, alpha = 1.0) +
      lims(x = c(-2,2), y = c(0,1)) +
      theme_clean() +
      ggtitle(title)
  }
  else {
    plot(conditional_effects(fit), plot = F)[[index]] +
      lims(x = c(-2,2), y = c(0,1)) +
      theme_clean() +
      ggtitle(title)
  }

}
