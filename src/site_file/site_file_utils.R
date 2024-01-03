
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