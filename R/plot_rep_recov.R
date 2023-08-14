#' @import dplyr
#' @import ggplot2
#' @export

plot_rep_recov = function(rep_df){
  tmp_sim = rep_df |>
    select(param = pars_param, pars_sim) |>
    unique()

  rep_df$chains |>
    bind_rows(.id = "id") |>
    select(c(id, tmp_sim$param)) |>
    pivot_longer(cols = -id, names_to = "param", values_to = "value") |>
    ggplot(aes(x = value, y = id, color = id)) +
    stat_halfeye(.width = c(.51,.93), geom = "pointinterval", alpha = 0.6, show.legend = F) +
    geom_vline(data = tmp_sim, aes(xintercept = pars_sim)) +
    facet_grid(~param, scales = "free") +
    labs(x = '',
         y = '') +
    theme_clean() +
    theme(
      # axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank())
}
