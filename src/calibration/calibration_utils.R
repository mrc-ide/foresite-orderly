# Starting EIR guess -----------------------------------------------------------
# Estimate equilibrium pfpr prevalence for a given eir and treatment coverage
pfpr_2_10 <- function(eir, ft = 0.1) {
  eq <- malariaEquilibrium::human_equilibrium(
    eir,
    ft = ft,
    p = malariaEquilibrium::load_parameter_set("Jamie_parameters.rds"),
    age = 0:100
  )
  sum(eq$states[3:11, 'pos_M']) / sum(eq$states[3:11, 'prop'])
}

# Estimate equilibrium EIR that results in a specified prevalence given treatment coverage
get_eq_eir <- function(eq_pfpr_2_10, ft = 0.1){
  if(eq_pfpr_2_10 <= 0 | eq_pfpr_2_10 > 0.9){
    stop("eq_pfpr_2_10 must be between 0 and 0.9")
  }
  if(ft <= 0 | ft > 1){
    stop("ft must be between 0 and 1")
  }
  opt_f <- function(eir, eq_pfpr_2_10, ft){
    pfpr_2_10(eir, ft) - eq_pfpr_2_10
  }
  opt <- uniroot(
    f = opt_f,
    lower = 0.0001,
    upper = 200,
    eq_pfpr_2_10 = eq_pfpr_2_10,
    ft = ft,
    extendInt = "upX"
  )
  opt$root
}
# ------------------------------------------------------------------------------

# Proposal ---------------------------------------------------------------------
to_real <- function(x, limits){
  log(x - limits[1]) - log(limits[2] - x)
}

from_real <- function(x, limits){
  (limits[2] * exp(x) + limits[1]) / (1 + exp(x))
}

proposal <- function(x, limits, direction, step = 0.5) {
  r <- to_real(x, limits)
  r_prop <- ifelse(direction ==  "decrease", r - step, r + step)
  x <- from_real(r_prop, limits)
  return(x)
}
# ------------------------------------------------------------------------------

# Evaluation -------------------------------------------------------------------
check_elimination <- function(output, target){
  any(output == 0 & target != 0)
}

check_fit <- function(eir, parameters, target, output_f, return_raw = FALSE){
  parameters <- parameters |>
    malariasimulation::set_equilibrium(
      init_EIR = eir
    )
  simulation <- malariasimulation::run_simulation(
    timesteps = parameters$timesteps,
    parameters = parameters
  )
  if(return_raw){
    return(simulation)
  }
  output <- output_f(simulation)
  elimination <- check_elimination(output, target)
  objective <- ifelse(elimination, NA, sum(output - target))
  return(objective)
}

check_stochasticity <- function(direction, fit1, fit2){
  # Check 1, if fit2 has not gone in the same direction as eir2, then we are
  # within stochastic uncertainty that fit1 and fit2 are the same and can
  # stop searching further.
  if(
    (direction == "decrease" & fit2 > fit) |
    (direction == "increase" & fit2 < fit1)
  ) {
    break
  }
}

linear_interpolate <- function(eir1, eir2, fit1, fit2){
  b <- (eir2 - eir1) / (fit2 - fit1)
  eir1 - b * fit1
}
# ------------------------------------------------------------------------------

# Reporting --------------------------------------------------------------------
print_update <- function(eir1 = NA, fit1 = NA, eir2 = NA, fit2 = NA, attempts = 0){
  message(
    "EIR1: ", round(eir1, 3),
    "\nFit1: ", round(fit1, 3),
    "\nEIR2: ", round(eir2, 3),
    "\nFit2: ", round(fit2, 3),
    "\nAttempts: ", attempts,
    "\n"
  )
}
# ------------------------------------------------------------------------------

# summary ----------------------------------------------------------------------
## x malariasimulation output
output <- function(x){
  # Estimate annual prevalence
  x$pfpr <- x$n_detect_730_3649 / x$n_730_3649
  year <- 2000:2024
  pfpr <- tapply(x$pfpr, rep(year, each = 365), mean)
  # extract years
  pfpr_subset <- pfpr[year %in% 2014:2018]
  return(pfpr_subset)
}
# ------------------------------------------------------------------------------

calibrate <- function(x, site, human_population = c(10000, 30000, 50000), burnin = 0, max_attempts = 10, eir_limits = c(0, 1000)){
  #   browser()
  print(x)
  sub_site <- site::subset_site(site, x)
  species <- x$sp
  
  # Define the target prevalence to fit to
  if(species == "pf"){
    prevalence <- sub_site$prevalence$pfpr 
  } else {
    prevalence <- sub_site$prevalence$pvpr 
  }
  target <- prevalence[sub_site$prevalence$year %in% 2014:2018]
  
  # Set parameters
  p <- site::site_parameters(
    interventions = sub_site$interventions,
    demography = sub_site$demography,
    vectors = sub_site$vectors$vector_species,
    seasonality = sub_site$seasonality$seasonality_parameters,
    overrides = list(
      human_population = human_population[1]
    ),
    species  = species,
    burnin = burnin
  )
  
  # Estimate starting guess EIR: eir1
  eir1 <- get_eq_eir(
    eq_pfpr_2_10 = prevalence[1],
    ft = sub_site$interventions$tx_cov[1]
  )
  
  # Get fit of eir1
  fit1 <- NA
  min_eir <- 0
  attempts <- 0
  while(is.na(fit1)){
    fit1 <- check_fit(eir = eir1, parameters = p, target = target, output_f = output)
    attempts <- attempts + 1
    print_update(eir1 = eir1, fit1 = fit1, attempts = attempts)
    
    # In the case that our starting EIR leads to unwanted elimination:
    ## First approach is to increasing human population if available, then
    ## Second approach is to increase eir, and can set a lower bound on future eir proposals
    if(is.na(fit1)){
      if(p$human_population < max(human_population)){
        message("Increasing human population due to elimination\n")
        p$human_population <- human_population[which(human_population == p$human_population) + 1]
      } else {
        eir1 <- proposal(x = eir1, limits = eir_limits, direction = "increase")
        min_eir <- eir1
      }
    }
  }
  
  # eir2
  # To estimate the eir2, we must first determine if we move up or down from
  # eir1
  direction <- ifelse(fit1 > 0, "decrease", "increase")
  if(direction == "decrease"){
    eir_limits[1] <- min_eir
  }
  eir2 <- eir1
  repeat{
    # Propose new EIR
    eir2 <- proposal(x = eir2, limits = eir_limits, direction = direction)
    
    # eir1 and eir2 are within a tolerance
    if(abs(eir1 - eir2) < 0.1){
      x$eir <- mean(c(eir1, eir2))
      message("Terminating due to eir tolerance")
      break
    }
    
    fit2 <- check_fit(eir = eir2, parameters = p, target = target, output_f = output)
    print_update(eir1 = eir1, fit1 = fit1, eir2 = eir2, fit2 = fit2, attempts = attempts)
    
    if(!is.na(fit2)){
      # If we haven't moved far enough, we can update eir1 and fit1  
      if((fit2 > 0 & direction == "decrease") | (fit2 < 0  & direction == "increase")){
        eir1 <- eir2
        fit1 <- fit2
      } else {
        # fit1 and fit2 are stochastically equivalent
        if((direction == "decrease" & fit2 > fit1) | (direction == "increase" & fit2 < fit1)) {
          x$eir <- mean(c(eir1, eir2))
          message("Terminating due to stochastic equivalence")
          break
        }
        if(direction == "decrease"){
          x$eir <- linear_interpolate(eir1 = eir1, eir2 = eir2, fit1 = fit1, fit2 = fit2)
        } else {
          x$eir <- linear_interpolate(eir1 = eir2, eir2 = eir1, fit1 = fit2, fit2 = fit1)
        }
        message("Terminating due to success")
        break
      }
    }
    
    # If upon reducing EIR we get elimination
    if(is.na(fit2) && direction == "decrease"){
      # Update_min_eir
      eir_limits[1] <- eir2
      # Reset eir2
      eir2 <- eir1
    }
    
    attempts <- attempts + 1
    if(attempts == max_attempts){
      x$eir <- mean(c(eir1, eir2))
      message("Terminating due to maximum attempts reached")
      break
    }
  }
  
  # TODO: This should be with a full burn in to assess clinical inc
  
  diagnostic_run <- check_fit(
    eir = x$eir,
    parameters = p,
    target = NULL,
    output_f = NULL,
    return_raw = TRUE
  )
  
  prev <- postie::get_prevalence(
    diagnostic_run
  ) |>
    dplyr::bind_cols(
      dplyr::select(
        sub_site$eir,
        -c("eir")
      )
    )
  
  scaler <- ifelse(species == "pf", 0.215, 0.003)
  epi <- postie::get_rates(
    diagnostic_run,
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
      eir_fit = x,
      epi = epi,
      prev = prev
    )
  )
}