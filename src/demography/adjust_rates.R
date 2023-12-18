adjust_rates <- function(x, save_output = FALSE){
  name <- paste(x[1, c("iso3c", "year")], collapse = "_")
  
  x$adjusted_mortality_rates <- peeps::estimate_mortality_rates(
    target_age_distribution = x$population_proportion,
    starting_mortality_rates = x$qx
  )
  
  x$fitted_age_dist <- peeps::equilibrium_age_distribution(x$adjusted_mortality_rates)
 
  if(save_output){
    saveRDS(x, name)
  } else {
    return(x)
  }
}