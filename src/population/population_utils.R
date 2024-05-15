logistic_function <- function(year, B, C) {
  year <- year - 2030
  0.8 / (1 + exp(-B * (year - C)))
}

# extrapolate_logistic <- function(year, proportion_urban, threshold_year) {
#   
#   data <- data.frame(
#     year = year[year <= threshold_year & year >= 2030],
#     proportion_urban = proportion_urban[year <= threshold_year & year >= 2030]
#   )
#   # Fit a polynomial model
#   fit <- nls(
#     proportion_urban ~ logistic_function(year, B, C), 
#     data = data,
#     start = list(B = 0.01, C = mean(data$proportion_urban))
#   )
#   
#   # Create a sequence for prediction
#   predictions <- predict(fit, newdata = data.frame(year = year[year > threshold_year]))
#   
#   out <- c(proportion_urban[year <= threshold_year], predictions)
#   
#   return(out)
# }

extrapolate_logistic <- function(year, proportion_urban, threshold_year) {
  
  # Define logistic function
  logistic_function <- function(year, B, C) {
    1 / (1 + exp(-B * (year - C)))
  }
  
  # Filter data for fitting
  data <- data.frame(
    year = year[year <= threshold_year & year >= 2030],
    proportion_urban = proportion_urban[year <= threshold_year & year >= 2030]
  )
  
  # Check if there is enough data to fit the model
  if (nrow(data) < 3) {
    stop("Not enough data points to fit the model.")
  }
  
  # Improved initial guesses
  initial_B <- 0.01
  initial_C <- mean(data$year)
  
  # Set bounds for parameters
  lower_bounds <- c(B = 0.0001, C = min(data$year))
  upper_bounds <- c(B = 1, C = max(data$year))
  
  # Fit a polynomial model with error handling
  fit <- tryCatch({
    nls(
      proportion_urban ~ logistic_function(year, B, C), 
      data = data,
      start = list(B = initial_B, C = initial_C),
      lower = lower_bounds,
      upper = upper_bounds,
      algorithm = "port"  # Use the 'port' algorithm for bounded parameters
    )
  }, error = function(e) {
    warning("NLS fitting failed: ", e$message)
    return(NULL)
  })
  
  # If fitting fails, return NA or a reasonable default
  if (is.null(fit)) {
    stop("Fail")
    return(rep(NA, length(year)))
  }
  
  # Create a sequence for prediction
  predictions <- predict(fit, newdata = data.frame(year = year[year > threshold_year]))
  
  out <- c(proportion_urban[year <= threshold_year], predictions)
  
  return(out)
}