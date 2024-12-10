
# Seasonality ------------------------------------------------------------------
middle_days <- data.frame(
  month = 1:12,
  month_name = c(
    "Jan", "Feb", "Mar", "Apr", "May", 
    "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  ),
  t = c(16, 45.5, 75, 105.5, 136, 166.5, 197, 228, 258.5, 289, 319.5, 350)
)

fit_fourier_df <- function(rainfall, t){
  out <- umbrella::fit_fourier(rainfall, t, 0)
  list(
    data.frame(
      coefficient = names(out$coefficients),
      value = out$coefficients
    )
  )
}
# ------------------------------------------------------------------------------

# Interventions ----------------------------------------------------------------
net_loss_match_objective <- function(mean_retention, half_life, years = 3){
  t <- 1:(365 * years)
  ex <- netz::net_loss_exp(t, mean_retention)
  map <- netz::net_loss_map(t, half_life)
  sum((ex - map)^2)
}
# ------------------------------------------------------------------------------

# Checks -----------------------------------------------------------------------
check_params <- function(site){
  eirs <- base::split(site$eir, 1:nrow(site$eir))
  for(i in 1:length(eirs)){
    x <- eirs[[i]]
    sub_site <- site::subset_site(site, x)
    species <- x$sp
    calibration_burnin <- 5
    tryCatch(
      {
        p <- site::site_parameters(
          interventions = sub_site$interventions,
          demography = sub_site$demography,
          vectors = sub_site$vectors$vector_species,
          seasonality = sub_site$seasonality$seasonality_parameters,
          overrides = list(
            human_population = 1000
          ),
          species  = species,
          burnin = calibration_burnin
        )
      },
      error = function(e) {
        # Handle the error here
        message("Site: ", paste(x[-length(x)], collapse = " "), " cannot produce valid parameter list")
        message("An error occurred: ", e)
        stop("Parameter fail")
      }
    )
  }
}
# ------------------------------------------------------------------------------