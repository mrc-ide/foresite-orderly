
# Seasonality -----------------------------------------------------------------
# TODO: t1 is a loaded spatial df

middle_days <- data.frame(
  month = 1:12,
  month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  t = c(16, 45.5, 75, 105.5, 136, 166.5, 197, 228, 258.5, 289, 319.5, 350)
)

fit_fourier_df <- function(rainfall, t){
  out <- umbrella::fit_fourier(rainfall, t, 0)
  list(data.frame(coefficient = names(out$coefficients), value = out$coefficients))
}

rainfall <- t1 |>
  summarise(
    across(paste0("rainfall_", 1:12), \(x) weighted.mean(x, w = par)),
    .by = c("name_1", "year")
  ) |>
  tidyr::pivot_longer(
    cols = paste0("rainfall_", 1:12),
    names_to = "month",
    values_to = "rainfall",
    names_prefix = "rainfall_",
    names_transform = as.integer
  ) |>
  left_join(
    middle_days,
    by = "month"
  ) |>
  tidyr::replace_na(
    list(rainfall = 0)
  )

seasonal_pars <- rainfall|>
  summarise(
    coefficients = fit_fourier_df(rainfall, t),
    .by = c("name_1")
  ) |>
  tidyr::unnest(col = "coefficients") |>
  tidyr::pivot_wider(names_from = coefficient, values_from = value)

seasonal_curve <- seasonal_pars |>
  summarise(
    predict = list(umbrella::fourier_predict(coef = c(g0, g1, g2, g3, h1, h2, h3), t = 1:365, floor = 0)),
    .by = "name_1"
  ) |>
  tidyr::unnest("predict")

seasonality_plot <- ggplot() +
  geom_jitter(data = rainfall, aes(x = t, y = rainfall, colour = name_1), alpha = 0.2) +
  geom_line(data = seasonal_curve, aes(x = t, y = profile, colour = name_1)) +
  facet_wrap(~ name_1) +
  scale_x_continuous(breaks = middle_days$t, labels = middle_days$month_name) +
  xlab("Day of year") +
  ylab("Rainfall") +
  theme_bw() +
  theme(legend.position = "none")
seasonality_plot
# ------------------------------------------------------------------------------


# Vectors ----------------------------------------------------------------------
use_relative <- !all(is.na(t1[,c("gambiae", "arabiensis", "funestus")]))

if(use_relative){
  vector_columns <- c("gambiae", "arabiensis", "funestus")
  vector_year <- 2016
} else {
  vector_columns <- names(t1)[grepl("occurrence", names(t1))]
  vector_year <- 2011
}

vectors <- t1 |>
  filter(
    year == vector_year
  ) |>
  select(
    any_of(c("name_1", "par", vector_columns))
  ) |>
  mutate(across(vector_columns, \(x) ifelse(is.na(x), 0, x))) |>
  summarise(
    across(vector_columns, \(x) weighted.mean(x, par, na.rm =)),
    .by = name_1
  ) |>
  tidyr::pivot_longer(
    -name_1, names_to = "vector",  values_to = "val"
  ) |>
  group_by(name_1) |>
  mutate(rank = rank(- val, ties = "first")) |>
  filter(rank <= 3) |>
  mutate(val = val / sum(val)) |>
  ungroup() |>
  select(-rank) |>
  mutate(vector = stringr::str_replace(vector, "occurrence_", ""))

table(vectors$vector)
# ------------------------------------------------------------------------------