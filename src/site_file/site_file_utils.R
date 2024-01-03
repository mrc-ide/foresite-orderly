
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
