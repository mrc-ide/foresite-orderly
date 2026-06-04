# summary ----------------------------------------------------------------------
## x malariasimulation output
summary_function_pf <- function(x){
  prev <- x |>
    postie::drop_burnin(
      burnin = 5 * 365 # TODO: Check burnin must align with calibration burnin!
    ) |>
    postie::get_prevalence(
    ) |>
    dplyr::summarise(
      prevalence_2_10 = mean(lm_prevalence_2_10),
      .by = "year"
    ) |>
    dplyr::filter(year %in% 2010:2024) |>
    dplyr::pull(prevalence_2_10)
  
  return(prev)
}

summary_function_pv <- function(x){
  prev <- x |>
    postie::drop_burnin(
      burnin = 5 * 365 # TODO: Check burnin must align with calibration burnin!
    ) |>
    postie::get_prevalence(
    ) |>
    dplyr::summarise(
      prevalence_1_100 = mean(lm_prevalence_1_100),
      .by = "year"
    ) |>
    dplyr::filter(year %in% 2010:2024) |>
    dplyr::pull(prevalence_1_100)
  return(prev)
}
# ------------------------------------------------------------------------------

calibrate_site <- function(
    x, site,
    human_population,
    diagnostic_burnin,
    max_attempts
){
  # browser()
  print(x)
  sub_site <- site::subset_site(site, x)
  parasite <- ifelse(sub_site$eir$sp == "pf", "falciparum", "vivax")
  
  # Define the target prevalence to fit to
  if(parasite == "falciparum"){
    prevalence <- sub_site$prevalence$pfpr 
    summary_function <- summary_function_pf
    scaler <- 0.215
  } else {
    prevalence <- sub_site$prevalence$pvpr 
    summary_function <- summary_function_pv
    scaler <- 0.003
  }
  target <- prevalence[sub_site$prevalence$year %in% 2010:2024]
  
  # Add ITN input dist
  sub_site$interventions$itn$implementation$itn_input_dist <- site::site_usage_to_model_distribution(
    usage = sub_site$interventions$itn$use$itn_use,
    usage_year = sub_site$interventions$itn$use$year,
    usage_day_of_year = sub_site$interventions$itn$use$usage_day_of_year,
    distribution_year = sub_site$interventions$itn$implementation$year,
    distribution_day_of_year = sub_site$interventions$itn$implementation$distribution_day_of_year,
    distribution_lower = sub_site$interventions$itn$implementation$distribution_lower,
    distribution_upper = sub_site$interventions$itn$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = sub_site$interventions$itn$retention_half_life
  ) 
  
  # Set parameters
  calibration_burnin <- 5
  p <- site::site_parameters(
    interventions = sub_site$interventions,
    demography = sub_site$demography,
    vectors = sub_site$vectors,
    seasonality = sub_site$seasonality,
    overrides = list(
      human_population = human_population[1]
    ),
    parasite = parasite,
    start_year = 2000 - calibration_burnin,
    end_year = 2026
  )
  
  calibration <- x$eir
  if(is.na(calibration)){
    tryCatch(
      {
        calibration <- cali::calibrate(
          parameters = p,
          target = target,
          summary_function = summary_function,
          eq_prevalence = min(max(prevalence), 0.85),
          eq_ft = sub_site$interventions$treatment$implementation$tx_cov[1],
          human_population = human_population,
          max_attempts = max_attempts,
          eir_limits = c(0.00001, 1500)
        )
      },
      error = function(e) {
        # Handle the error here
        message("Site: ", paste(x[-length(x)], collapse = " "), " has failed calibration")
        message("An error occurred: ", e)
        calibration <- NA
      }
    )
  }
  
  prev <- NULL
  epi <- NULL
  
  if(!is.na(calibration)){
    p <- site::site_parameters(
      interventions = sub_site$interventions,
      demography = sub_site$demography,
      vectors = sub_site$vectors,
      seasonality = sub_site$seasonality,
      overrides = list(
        human_population = 50000
      ),
      parasite = parasite,
      start_year = 2000 - diagnostic_burnin,
      end_year = 2026,
      eir = calibration
    )
    
    s <- malariasimulation::run_simulation(timesteps = p$timesteps, parameters = p)
    browser()
    prev <- s |>
      postie::drop_burnin(
        burnin = 365 * diagnostic_burnin
      ) |>
      postie::get_prevalence(
      ) |>
      dplyr::bind_cols(
        dplyr::select(
          sub_site$eir,
          -c("eir")
        )
      )
    
    epi <- s |>
      postie::drop_burnin(
        burnin = 365 * diagnostic_burnin
      ) |>
      postie::get_rates(
        scaler = scaler
      ) |>
      dplyr::bind_cols(
        dplyr::select(
          sub_site$eir,
          -c("eir")
        )
      )
  }
  
  x$eir <- calibration
  out <-  list(
    eir_fit = x,
    epi = epi,
    prev = prev
  )
  return(out)
}