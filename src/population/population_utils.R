extrapolate_with_bounds_polynomial <- function(year, proportion_urban, threshold_year) {
  
  data <- data.frame(
    year = year[year <= threshold_year],
    proportion_urban = proportion_urban[year <= threshold_year]
  )
  # Fit a polynomial model
  formula <- as.formula(paste("proportion_urban", "~ poly(", "year", ",", 2, ")"))
  model <- lm(formula, data = data)
  
  # Create a sequence for prediction
  predictions <- predict(model, newdata = data.frame(year = year[year > threshold_year]))
  predictions <- pmin(predictions, 1)
  predictions <- pmax(predictions, 0)
  
  out <- c(as.vector(data$proportion_urban), predictions)
  
  return(out)
}