#' @import tidybayes
#' @export
get_chains = function(fit, type = c("logreg_one", "logreg_one_nl", "logreg_one_ranef", "logreg_prepost", "logreg_prepost_ranef", "wichmann_one", "wichmann_one_ranef", "wichmann_prepost")){
  type = match.arg(type)
  if (type == "logreg_one"){                     # LOGREG ONE
    chains = fit %>%
      spread_draws(
        b_Intercept,
        b_stimulus
      ) |>
      mutate(
        b0 = b_Intercept,
        b1 = b_stimulus,
      ) |>
      select(b0, b1)
  } else if ( type == "logreg_one_nl"){         # LOGREG ONE NL
    chains = fit |>
      spread_draws(
        b_a_Intercept,
        b_s_stimulus
      ) |>
      mutate(
        b0 = b_a_Intercept,
        b1 = b_s_stimulus
      ) |>
      select(b0, b1)
  } else if ( type == "logreg_one_ranef"){     # LOGREG ONE RANEF
    chains = fit %>%
      spread_draws(
        b_Intercept,
        b_stimulus,
        sd_vpn__Intercept,
        sd_vpn__stimulus,
        # cor_vpn__Intercept__stimulus
      ) |>
      mutate(
        b0 = b_Intercept,
        b1 = b_stimulus,
        b0_sigma = sd_vpn__Intercept,
        b1_sigma = sd_vpn__stimulus,
        # cor_b0_b1 = cor_vpn__Intercept__stimulus
      ) |>
      select(b0, b1, b0_sigma, b1_sigma)#, cor_b0_b1)
  } else if ( type == "logreg_prepost"){       # LOGREG PRE/POST
    chains = fit |>
      spread_draws(
        b_Intercept,
        `b_timepre:stimulus`,
        `b_timepost:stimulus`
      ) |>
      mutate(
        b0 = b_Intercept,
        b1_pre = `b_timepre:stimulus`,
        b1_post = `b_timepost:stimulus`
      ) |>
      select(b0, b1_pre, b1_post)
  } else if ( type == "logreg_prepost_ranef"){
    chains = fit %>%
      spread_draws(
        b_Intercept,
        `b_timepre:stimulus`,
        `b_timepost:stimulus`,
        sd_vpn__Intercept,
        `sd_vpn__timepre:stimulus`,
        `sd_vpn__timepost:stimulus`,
        # `cor_vpn__Intercept__timepre:stimulus`,
        # `cor_vpn__Intercept__timepost:stimulus`,
        # `cor_vpn__timepre:stimulus__timepost:stimulus`
      ) %>%
      mutate(
        b0 = b_Intercept,
        b1_pre = `b_timepre:stimulus`,
        b1_post = `b_timepost:stimulus`,
        b0_sigma = sd_vpn__Intercept,
        b1_pre_sigma = `sd_vpn__timepre:stimulus`,
        b1_post_sigma = `sd_vpn__timepost:stimulus`,
        # cor_b0_b1_pre = `cor_vpn__Intercept__timepre:stimulus`,
        # cor_b0_b1_post = `cor_vpn__Intercept__timepost:stimulus`,
        # cor_b1_prepost = `cor_vpn__timepre:stimulus__timepost:stimulus`,
      ) %>%
      select(b0, b1_pre, b1_post, b0_sigma, b1_pre_sigma, b1_post_sigma)#, cor_b0_b1_pre, cor_b0_b1_post, cor_b1_prepost, b1_diff)
  } else if ( type == "wichmann_one"){
    chains = fit %>%
      spread_draws(
        b_eta_Intercept,
        b_eta_stimulus,
        b_guess_Intercept,
        b_lapse_Intercept
      ) %>%
      mutate(
        b0 = b_eta_Intercept,
        b1 = b_eta_stimulus,
        guess = b_guess_Intercept,
        lapse = b_lapse_Intercept
      ) %>%
      select(b0, b1, guess, lapse)
  } else if ( type == "wichmann_one_ranef"){
    chains = fit %>%
      spread_draws(
        b_eta_Intercept,
        b_eta_stimulus,
        b_guess_Intercept,
        b_lapse_Intercept,
        sd_vpn__eta_Intercept,
        sd_vpn__eta_stimulus,
        sd_vpn__guess_Intercept,
        sd_vpn__lapse_Intercept,
        # cor_vpn__eta_Intercept__eta_stimulus
      ) %>%
      mutate(
        b0 = b_eta_Intercept,
        b0_sigma = sd_vpn__eta_Intercept,
        b1 = b_eta_stimulus,
        b1_sigma = sd_vpn__eta_stimulus,
        b0_guess = b_guess_Intercept,
        b0_guess_sigma = sd_vpn__guess_Intercept,
        b0_lapse = b_lapse_Intercept,
        b0_lapse_sigma = sd_vpn__lapse_Intercept,
        # rho = cor_vpn__eta_Intercept__eta_stimulus
      ) %>%
      select(b0, b0_sigma, b1, b1_sigma, b0_guess, b0_guess_sigma, b0_lapse, b0_lapse_sigma)#, rho)
  } else if ( type == "wichmann_prepost"){
    chains = fit %>%
      spread_draws(
        b_eta_Intercept,
        `b_eta_timepre:stimulus`,
        `b_eta_timepost:stimulus`,
        b_guess_Intercept,
        b_guess_timepost,
        b_lapse_Intercept,
        b_lapse_timepost
      ) %>%
      mutate(
        b0 = b_eta_Intercept,
        b1_pre = `b_eta_timepre:stimulus`,
        b1_post = `b_eta_timepost:stimulus`,
        b3 = `b_eta_timepost:stimulus`,
        b0_guess = b_guess_Intercept,
        b2_guess = b_guess_timepost,
        b0_lapse = b_lapse_Intercept,
        b2_lapse = b_lapse_timepost,
        lapse_pre = b0_lapse,
        lapse_post = b0_lapse + b2_lapse,
        guess_pre = b0_guess,
        guess_post = b0_guess + b2_guess
      ) %>%
      select(b0, b1_pre, b1_post, guess_pre, guess_post, lapse_pre, lapse_post)
  }
  chains
}
