#' @import dplyr
#' @export

get_pars = function(chains, simulated_data, width = 0.95){
  # get simulation pars
  pars_sim = get_sim_param(simulated_data, chains)

  pars = chains |>
    pivot_longer(cols = everything(), names_to = "param", values_to = "value") |>
    group_by(param) |>
    mean_qi(.width = width) |>
    select(param, value, .lower, .upper, .width) |>
    mutate(fit = round(value, digits = 3),
           .lower = round(.lower, digits = 3),
           .upper = round(.upper, digits = 3)) |>
    left_join(pars_sim) |>
    select(param, sim, fit, .lower, .upper, .width)
}
