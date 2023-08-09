#' @import tibble
#' @import dplyr
#' @import tidyr
#' @export

cell_mean_simulation = function(b0_pre = 0.0,
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

  if (n_vpn == 1){                                                  # SIMULATE DATA FOR ONE SUBJECT
    vpn_param = tibble(vpn = 1,
                     b0_pre_j = b0_pre,
                     b0_post_j = b0_post,
                     b1_pre_j = b1_pre,
                     b1_post_j = b1_post,
                     lapse_pre_j = lapse_pre,
                     lapse_post_j = lapse_post,
                     guess_pre_j = guess_pre,
                     guess_post_j = guess_post)

  } else {                                                          # SIMULATE DATA FOR MULTIPLE SUBJECTS
    mus = c(b0_pre, b0_post, b1_pre, b1_post, lapse_pre, lapse_post, guess_pre, guess_post)#, b0_lapse, b1_lapse, b0_guess, b1_guess)                             #   means
    sigmas = c(b0_pre_sigma, b0_post_sigma, b1_pre_sigma, b1_post_sigma, lapse_pre_sigma, lapse_post_sigma, guess_pre_sigma, guess_post_sigma)  #   standard deviations
    rho_ma = matrix(c(1, rho, rho, rho, rho, rho, rho, rho,              #   correlation matrix
                      rho, 1, rho, rho, rho, rho, rho, rho,
                      rho, rho, 1, rho, rho, rho, rho, rho,
                      rho, rho, rho, 1, rho, rho, rho, rho,
                      rho, rho, rho, rho, 1, rho, rho, rho,
                      rho, rho, rho, rho, rho, 1, rho, rho,
                      rho, rho, rho, rho, rho, rho, 1, rho,
                      rho, rho, rho, rho, rho, rho, rho, 1), nrow = 8)
    SD = diag(sigmas) %*% rho_ma %*% diag(sigmas)                   #   now matrix multiply to get covariance matrix

    vpn_param = MASS::mvrnorm(n_vpn, mus, SD, tol = 2) |>           #   draw param
      tibble::as_tibble(.name_repair = "unique") |>
      purrr::set_names("b0_pre_j", "b0_post_j", "b1_pre_j", "b1_post_j", "lapse_pre_j", "lapse_post_j", "guess_pre_j", "guess_post_j") |>
      mutate(vpn = 1:n_vpn, .before = "b0_pre_j")
  }

  # simulate
  df = vpn_param |>                                                # THE SAME FOR ONE OR MULTIPLE
    expand_grid(time,
                stimulus,
                rep = seq(1, n_trials)) |>
    mutate(
      time = factor(time, levels = c("pre", "post")),
      b0_time = case_when(time == "pre" ~ b0_pre_j,
                          time == "post" ~ b0_post_j),
      b1_time = case_when(time == "pre" ~ b1_pre_j,
                          time == "post" ~ b1_post_j),
      lapse_time = case_when(time == "pre" ~ lapse_pre_j,
                             time == "post" ~ lapse_post_j),
      guess_time = case_when(time =="pre" ~ guess_pre_j,
                             time == "post" ~ guess_post_j),
      linpred = b0_time + (b1_time * stimulus),
      theta = guess_time + (1 - guess_time - lapse_time) * plogis(linpred),
      response = rbinom(n = length(theta), size = 1, prob = theta),
      b0_pre = b0_pre,
      b0_post = b0_post,
      b0_pre_sigma = b0_pre_sigma,
      b0_post_sigma = b0_post_sigma,

      b1_pre = b1_pre,
      b1_post = b1_post,
      b1_pre_sigma = b1_pre_sigma,
      b1_post_sigma = b1_post_sigma,

      lapse_pre = lapse_pre,
      lapse_post = lapse_post,
      lapse_pre_sigma = lapse_pre_sigma,
      lapse_post_sigma = lapse_post_sigma,

      guess_pre = guess_pre,
      guess_post = guess_post,
      guess_pre_sigma = guess_pre_sigma,
      guess_post_sigma = guess_post_sigma)
  df
}

simulate_data = function(b0 = 0.0,
                         b0_sigma = 0.0,
                         b1 = 0.0,
                         b1_sigma = 0.0,
                         b2 = 0.0,
                         b2_sigma = 0.0,
                         b3 = 0.0,
                         b3_sigma = 0.0,
                         b0_guess = 0.0,
                         b0_guess_sigma = 0.0,
                         b1_guess = 0.00,
                         b1_guess_sigma = 0.00,
                         b0_lapse = 0.0,
                         b0_lapse_sigma = 0.0,
                         b1_lapse = 0.00,
                         b1_lapse_sigma = 0.00,
                         rho = 0.0,
                         n_vpn = 2,
                         n_trials = 20,
                         time = c("pre","post"),
                         stimulus = c(-2, -1,1,2)){


  if (n_vpn == 1){                                                  # SIMULATE DATA FOR ONE SUBJECT
    vpn_param = tibble(vpn = 1,
                       b0_j = b0,
                       b1_j = b1,
                       b2_j = b2,
                       b3_j = b3,
                       b0_lapse_j = b0_lapse,
                       b1_lapse_j = b1_lapse,
                       b0_guess_j = b0_guess,
                       b1_guess_j = b1_guess)
  } else {                                                          # SIMULATE DATA FOR MULTIPLE SUBJECTS
    mus = c(b0, b1, b2, b3, b0_lapse, b1_lapse, b0_guess, b1_guess)                             #   means
    sigmas = c(b0_sigma, b1_sigma, b2_sigma, b3_sigma, b0_lapse_sigma, b1_lapse_sigma, b0_guess_sigma, b1_guess_sigma)  #   standard deviations
    rho_ma = matrix(c(1, rho, rho, rho, rho, rho, rho, rho,              #   correlation matrix
                      rho, 1, rho, rho, rho, rho, rho, rho,
                      rho, rho, 1, rho, rho, rho, rho, rho,
                      rho, rho, rho, 1, rho, rho, rho, rho,
                      rho, rho, rho, rho, 1, rho, rho, rho,
                      rho, rho, rho, rho, rho, 1, rho, rho,
                      rho, rho, rho, rho, rho, rho, 1, rho,
                      rho, rho, rho, rho, rho, rho, rho, 1), nrow = 8)
    SD = diag(sigmas) %*% rho_ma %*% diag(sigmas)                   #   now matrix multiply to get covariance matrix

    vpn_param = MASS::mvrnorm(n_vpn, mus, SD, tol = 2) |>           #   draw param
      tibble::as_tibble(.name_repair = "unique") |>
      purrr::set_names("b0_j", "b1_j", "b2_j", "b3_j", "b0_lapse_j", "b1_lapse_j", "b0_guess_j", "b1_guess_j") |>
      mutate(vpn = 1:n_vpn, .before = "b0_j")
  }

  df = vpn_param |>                                                # THE SAME FOR ONE OR MULTIPLE
    expand_grid(time,
                stimulus,
                rep = seq(1, n_trials)) |>
    mutate(
      time = factor(time, levels = c("pre", "post")),
      time_num = case_when(time == "pre" ~ 0,
                           time == "post" ~ 1),
      guess = b0_guess_j + (b1_guess_j * time_num),
      lapse = b0_lapse_j + (b1_lapse_j * time_num),
      linpred = b0_j + (b1_j * stimulus) + (b2_j * time_num) + (b3_j * stimulus * time_num),
      theta = guess + (1 - guess - lapse) * plogis(linpred),
      response = rbinom(n = length(theta), size = 1, prob = theta),
      b0 = b0,
      b0_pre = b0,
      b0_post = b0 + b2,
      b0_sigma = b0_sigma,
      b1 = b1,
      b1_sigma = b1_sigma,
      b2 = b2,
      b2_sigma = b2_sigma,
      b3 = b3,
      b3_sigma = b3_sigma,
      b1_pre = b1,
      b1_pre_sigma = b1_sigma,
      b1_post = b1 + b3,
      b1_post_sigma = b1_sigma,
      b0_guess = b0_guess,
      b0_guess_sigma = b0_guess_sigma,
      b1_guess = b1_guess,
      b1_guess_sigma = b1_guess_sigma,
      guess_pre = b0_guess,
      guess_post = b0_guess + b1_guess,
      b0_lapse = b0_lapse,
      b0_lapse_sigma = b0_lapse_sigma,
      b1_lapse = b1_lapse,
      b1_lapse_sigma = b1_lapse_sigma,
      lapse_pre = b0_lapse,
      lapse_post = b0_lapse + b1_lapse,
      rho = rho
    )
  # |>
  # arrange(vpn, stimulus)
}
