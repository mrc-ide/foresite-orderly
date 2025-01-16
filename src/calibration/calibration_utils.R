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
    dplyr::filter(year %in% 2014:2018) |>
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
    dplyr::filter(year %in% 2014:2018) |>
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
  print(x)
  sub_site <- site::subset_site(site, x)
  species <- x$sp
  
  # Define the target prevalence to fit to
  if(species == "pf"){
    prevalence <- sub_site$prevalence$pfpr 
    summary_function <- summary_function_pf
    scaler <- 0.215
  } else {
    prevalence <- sub_site$prevalence$pvpr 
    summary_function <- summary_function_pv
    scaler <- 0.003
  }
  target <- prevalence[sub_site$prevalence$year %in% 2014:2018]
  
  # Set parameters
  calibration_burnin <- 5
  p <- site::site_parameters(
    interventions = sub_site$interventions,
    demography = sub_site$demography,
    vectors = sub_site$vectors$vector_species,
    seasonality = sub_site$seasonality$seasonality_parameters,
    overrides = list(
      human_population = human_population[1]
    ),
    species  = species,
    burnin = calibration_burnin
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
          eq_ft = sub_site$interventions$tx_cov[1],
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
      vectors = sub_site$vectors$vector_species,
      seasonality = sub_site$seasonality$seasonality_parameters,
      overrides = list(
        human_population = 50000
      ),
      species  = species,
      burnin = diagnostic_burnin,
      eir = calibration
    )
    
    s <- malariasimulation::run_simulation(timesteps = p$timesteps, parameters = p)
    
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
      ) |>
      dplyr::rename(
        prevalence_2_10 = lm_prevalence_2_10,
        prevalence_1_100 = lm_prevalence_1_100
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