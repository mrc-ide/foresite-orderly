logistic_function <- function(year, B, C) {
  year <- year - 2030
  0.8 / (1 + exp(-B * (year - C)))
}

extrapolate_logistic <- function(year, proportion_urban, threshold_year) {
  
  data <- data.frame(
    year = year[year <= threshold_year & year >= 2030],
    proportion_urban = proportion_urban[year <= threshold_year & year >= 2030]
  )
  # Fit a polynomial model
  fit <- nls(
    proportion_urban ~ logistic_function(year, B, C), 
    data = data,
    start = list(B = 0.1, C = mean(data$proportion_urban))
  )
  
  # Create a sequence for prediction
  predictions <- predict(fit, newdata = data.frame(year = year[year > threshold_year]))
  
  out <- c(proportion_urban[year <= threshold_year], predictions)
  
  return(out)
}