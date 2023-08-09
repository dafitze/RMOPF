#' @import dplyr
#' @import ggplot2
#' @export

plot_chains = function(chains, plot_data = NA, color = 'orange', title = 'Title', show_pointinterval = F){
  if (!is_tibble(chains)){
    tmp = bind_rows(chains, .id = "id")
  } else if (is_tibble(chains)){
    tmp = chains %>% mutate(id = "one")
  }

  tmp %>%
    pivot_longer(cols = -id, names_to = "param", values_to = "value") %>%
    ggplot() +
    stat_halfeye(mapping = aes(x = value, y = 0, fill = id), .width = c(.51,.93), geom = "slab", alpha = 0.6) +
    {if(show_pointinterval) stat_halfeye(mapping = aes(x = value, y = 0, color = id), .width = c(.51,.93), fill = color, geom = "pointinterval")} +
    {if(!is.na(plot_data)[1])geom_point(data = get_pars(select(tmp, -id), plot_data),
                                        mapping = aes(x = sim, y = 0, color = "Simulation"),
                                        shape = 124,
                                        size = 8) } +
    facet_wrap(~param, ncol = 2, scales = "free") +
    theme_clean() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title = element_blank(),
          # legend.position = "bottom",
          strip.text.x = element_text(size = 15, hjust = 0)) +
    labs(x = "",
         y = "") +
    ggtitle(title)
}

