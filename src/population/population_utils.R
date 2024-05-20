# Define logistic function
logistic_function <- function(year, B, C) {
  1 / (1 + exp(-B * (year - C)))
}

extrapolate_logistic <- function(year, proportion_urban, threshold_year) {
  # Filter data for fitting
  data <- data.frame(
    year = year[year <= threshold_year & year >= 2030],
    proportion_urban = proportion_urban[year <= threshold_year & year >= 2030]
  )
  
  # Improved initial guesses
  initial_B <- 0.01
  initial_C <- 2050
  
  # Set bounds for parameters
  max_pu <- max(proportion_urban, na.rm = TRUE)
  if(max_pu > 0.65){
    cup <- 1950
  } else {
    cup <- 2020
  }
  lower_bounds <- c(B = 0.001, C = cup)
  upper_bounds <- c(B = 0.3, C = 2100)
  
  # Fit a polynomial model with error handling
  fit <- 
    nls(
      proportion_urban ~ logistic_function(year, B, C), 
      data = data,
      start = list(B = initial_B, C = initial_C),
      lower = lower_bounds,
      upper = upper_bounds,
      algorithm = "port"  # Use the 'port' algorithm for bounded parameters,
      
    )
  
  # Create a sequence for prediction
  predictions <- predict(fit, newdata = data.frame(year = year[year > threshold_year]))
  
  out <- c(proportion_urban[year <= threshold_year], predictions)
  
  return(out)
}