#' @import dplyr
#' @export

repeat_simulation = function(reps = 2,
                             b0_pre = 0.0,
                             b0_post = 0.0,
                             b0_pre_sigma = 0.0,
                             b0_post_sigma = 0.0,

                             b1_pre = 0.0,
                             b1_post = 0.0,
                             b1_pre_sigma = 0.0,
                             b1_post_sigma = 0.0,

                             lapse_pre = 0.0,
                             lapse_post = 0.0,
                             lapse_pre_sigma = 0.0,
                             lapse_post_sigma = 0.0,

                             guess_pre = 0.0,
                             guess_post = 0.0,
                             guess_pre_sigma = 0.0,
                             guess_post_sigma = 0.0,

                             rho = 0.0,

                             n_vpn = 2,
                             n_trials = 2,
                             time = c("pre","post"),
                             stimulus = c(-2, -1,1,2)){
  tmp = tibble(idx = 1:reps,
               b0_pre, b0_post, b0_pre_sigma, b0_post_sigma,
               b1_pre, b1_post, b1_pre_sigma, b1_post_sigma,
               lapse_pre, lapse_post, lapse_pre_sigma, lapse_post_sigma,
               guess_pre, guess_post, guess_pre_sigma, guess_post_sigma,
               rho,
               n_vpn,
               n_trials,
               time = list(time),
               stimulus = list(stimulus)
  ) |>
    mutate(sim_dat = pmap(list(b0_pre, b0_post, b0_pre_sigma, b0_post_sigma,
                               b1_pre, b1_post, b1_pre_sigma, b1_post_sigma,
                               lapse_pre, lapse_post, lapse_pre_sigma, lapse_post_sigma,
                               guess_pre, guess_post, guess_pre_sigma, guess_post_sigma,
                               rho,
                               n_vpn,
                               n_trials,
                               time,
                               stimulus),
                          cell_mean_simulation)) |>
    select(idx, sim_dat)
  tmp
}
