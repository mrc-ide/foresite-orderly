# summary ----------------------------------------------------------------------
## x malariasimulation output
summary_function_pf <- function(x){
  prev <- x |>
    postie::drop_burnin(
    burnin = 5 * 365
  ) |>
    postie::get_prevalence(
    )
  # Estimate annual prevalence
  year <- 2000:2024
  pfpr <- tapply(prev$prevalence_2_10, rep(year, each = 365), mean)
  # extract years
  pfpr_subset <- pfpr[year %in% 2014:2018]
  return(pfpr_subset)
}
summary_function_pv <- function(x){
  prev <- x |>
    postie::drop_burnin(
      burnin = 5 * 365
    ) |>
    postie::get_prevalence(
    )
  # Estimate annual prevalence
  year <- 2000:2024
  pvpr <- tapply(prev$prevalence_1_100, rep(year, each = 365), mean)
  # extract years
  pvpr_subset <- pvpr[year %in% 2014:2018]
  return(pvpr_subset)
}
# ------------------------------------------------------------------------------

calibrate_site <- function(
    x, site,
    human_population = c(1000, 30000, 50000),
    diagnostic_burnin = 20,
    max_attempts = 10
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
  
  calibration <- cali::calibrate(
    parameters = p,
    target = target,
    summary_function = summary_function, #TODO adjust summary function for sp
    eq_prevalence = max(prevalence),
    eq_ft = sub_site$interventions$tx_cov[1],
    human_population = human_population,
    max_attempts = max_attempts
  )

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
  
  return(
    list(
      eir_fit = calibration,
      epi = epi,
      prev = prev
    )
  )
}