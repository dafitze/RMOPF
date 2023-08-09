#' @import dplyr
#' @export

get_sim_param = function(simulated_data, chains){
  pars_sim = d_sim |>
    select(colnames(chains)) |>
    unique() |>
    pivot_longer(cols = everything(), names_to = 'param', values_to = 'sim')
  pars_sim
}
