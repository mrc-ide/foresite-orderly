# Adjust one country-year's UN mortality rates so that a fixed-size model
# population reproduces the observed (target) age distribution at equilibrium.
# Expects a single iso3c x year group as `x`.
adjust_rates <- function(x, save_output = FALSE){
  name <- paste(x[1, c("iso3c", "year")], collapse = "_")

  # Re-estimate mortality rates to match the target age distribution
  x$adjusted_mortality_rates <- peeps::estimate_mortality_rates(
    target_age_distribution = x$population_proportion,
    starting_mortality_rates = x$qx
  )

  # Equilibrium age distribution implied by the adjusted rates (kept for checks)
  x$fitted_age_dist <- peeps::equilibrium_age_distribution(x$adjusted_mortality_rates)

  if(save_output){
    saveRDS(x, name)
  } else {
    return(x)
  }
}
