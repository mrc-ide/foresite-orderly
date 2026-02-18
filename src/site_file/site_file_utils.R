
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
    
    usage_timesteps <- site::calendar_to_timestep(
      year = sub_site$interventions$itn$use$year,
      day_of_year = sub_site$interventions$itn$use$usage_day_of_year,
      start_year = min(sub_site$interventions$itn$use$year)
    )
    distribution_timesteps <- site::calendar_to_timestep(
      year = sub_site$interventions$itn$implementation$year,
      day_of_year = sub_site$interventions$itn$implementation$distribution_day_of_year,
      start_year = min(sub_site$interventions$itn$implementation$year)
    )
    
    sub_site$interventions$itn$implementation$itn_input_dist <- netz::usage_to_model_distribution(
      usage = sub_site$interventions$itn$use$itn_use,
      usage_timesteps = usage_timesteps,
      distribution_timesteps = distribution_timesteps,
      distribution_lower = sub_site$interventions$itn$implementation$distribution_lower,
      distribution_upper = sub_site$interventions$itn$implementation$distribution_upper,
      net_loss_function = netz::net_loss_map,
      half_life = sub_site$interventions$itn$retention_half_life
    )
    
    sub_site$interventions$itn$use$expected_use <- netz::model_distribution_to_usage(
      distribution = sub_site$interventions$itn$implementation$itn_input_dist,
      usage_timesteps = usage_timesteps,
      distribution_timesteps = distribution_timesteps,
      net_loss_function = netz::net_loss_map,
      half_life = sub_site$interventions$itn$retention_half_life
    )
    
    parasite <- ifelse(x$sp == "pf", "falciparum", "vivax")
    start_year <- 1995
    end_year <- 2026
    tryCatch(
      {
        p <- site::site_parameters(
          interventions = sub_site$interventions,
          demography = sub_site$demography,
          vectors = sub_site$vectors,
          seasonality = sub_site$seasonality,
          overrides = list(
            human_population = 1000
          ),
          parasite  = parasite,
          start_year = start_year,
          end_year = end_year
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