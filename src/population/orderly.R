# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Population",
  long = "Population projections"
)

orderly2::orderly_parameters(
  iso3c = NULL,
  admin_level = NULL
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = c(
    "un_wup.rds",
    "un_wpp.rds"
  )
)

orderly2::orderly_dependency(
  name = "spatial",
  query = "latest(parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level)",
  files = c(
    "aggregated_spatial_data.rds"
  )
)
# ------------------------------------------------------------------------------

# Population projection --------------------------------------------------------

# Fill missing past years with zero population
## For example if a region had zero urban population until 2010
past_population <- readRDS("aggregated_spatial_data.rds")

past_year <- min(past_population$year):max(past_population$year)

past_population <- past_population |>
  dplyr::select(
    "iso3c", dplyr::any_of(c("name_1", "name_2")), "urban_rural", "year",
    "population", "par_pf", "par_pv", "par"
  ) |>
  dplyr::group_by(
    dplyr::across(
      dplyr::any_of(
        c("iso3c", "name_1", "name_2", "urban_rural")
      )
    )
  ) |>
  tidyr::complete(
    year = past_year
  ) |>
  dplyr::ungroup() |>
  tidyr::replace_na(
    replace = list(
      population = 0,
      par_pf = 0,
      par_pv = 0,
      par  = 0
    )
  )

# Estimate the growth of urban and rural populations relative to a reference year
reference_year <- max(past_population$year)

un_wup <- readRDS("un_wup.rds") |>
  dplyr::filter(iso3c == {{iso3c}}) 

un_wpp <- readRDS("un_wpp.rds") |>
  dplyr::filter(iso3c == {{iso3c}})

un_pop_rate <- un_wpp |>
  dplyr::summarise(
    population = sum(population),
    .by = "year"
  ) |>
  dplyr::left_join(
    un_wup, 
    by = "year"
  ) |>
  dplyr::mutate(
    population_urban = population * proportion_urban,
    population_rural = population * (1 - proportion_urban),
    relative_urban = population_urban / population_urban[year == reference_year],
    relative_rural = population_rural / population_rural[year == reference_year]
  ) |>
  dplyr::filter(year > reference_year) |>
  dplyr::select("year", "relative_urban", "relative_rural") |>
  tidyr::pivot_longer(
    cols = -"year",
    values_to = "un_rate",
    names_to = "urban_rural",
    names_prefix = "relative_"
  ) 

# Project future years
future_years <- (reference_year + 1):end_year

future_population <- past_population |>
  dplyr::filter(year == max(year)) |>
  dplyr::select(-year) |>
  dplyr::cross_join(
    data.frame(year = future_years)
  ) |>
  dplyr::left_join(un_pop_rate, by = c("year", "urban_rural")) |>
  dplyr::mutate(
    population = population * un_rate,
    par_pf = par_pf * un_rate,
    par_pv = par_pv * un_rate,
    par = par * un_rate
  ) |>
  dplyr::select(
    -"un_rate"
  )

population <- dplyr::bind_rows(past_population, future_population) |>
  dplyr::arrange(
    dplyr::across(
      dplyr::any_of(
        c("name_1", "name_2", "urban_rural", "year")
      )
    )
  )
# ------------------------------------------------------------------------------

# Population age-disaggregation ------------------------------------------------
## Assumes that age-distribution varies by year but is the same across all sites
population_age <- population |>
  dplyr::left_join(
    dplyr::select(un_wpp, "year", "age_lower", "age_upper", "population_proportion"),
    by = "year",
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(
    population = population * population_proportion,
    par_pf = par_pf * population_proportion,
    par_pv = par_pv * population_proportion,
    par = par * population_proportion
  ) |>
  dplyr::select(-population_proportion)
# ------------------------------------------------------------------------------

# Diagnostics ------------------------------------------------------------------
library(ggplot2)

# Plot projections
ggplot(data = population, aes(x = year, y = population, colour = urban_rural)) +
  geom_line() +
  facet_wrap(~ name_1, scales = "free_y") +
  theme_bw()

# Check total against un 
total_check <- population |>
  dplyr::summarise(
    population = sum(population),
    .by = "year") |>
  dplyr::mutate(source = "population")

total_age_check <- population_age |>
  dplyr::summarise(
    population = sum(population),
    .by = "year") |>
  dplyr::mutate(source = "population_age")

total_un_check <- un_wpp |>
  dplyr::summarise(
    population = sum(population),
    .by = "year") |>
  dplyr::mutate(source = "population_un")

total_check_pd <- total_check |>
  dplyr::bind_rows(total_age_check) |>
  dplyr::bind_rows(total_un_check)

ggplot(data = total_check_pd, aes(x = year, y = population, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

# Check age distribution (against UN input)
age_comp <- population_age |>
  dplyr::filter(year %% 10 == 0) |>
  dplyr::summarise(population = sum(population), 
                   .by = c("year", "age_lower"))

un_age_comp <- un_wpp |>
  dplyr::filter(year %% 10 == 0) |>
  dplyr::rename(un_population = population) |>
  dplyr::select(
    "year", "age_lower", "un_population"
  )

age_comp_pd <- age_comp |>
  dplyr::left_join(un_age_comp, by = c("year", "age_lower")) |>
  tidyr::pivot_longer(
    cols = -c("year", "age_lower"),
    names_to = "source",
    values_to = "population"
  )

ggplot(data = age_comp_pd, aes(x = age_lower, y = population, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ year) +
  theme_bw()

# Check urban rural (against un input)

urban_check <- population |>
  dplyr::summarise(population = sum(population), .by = c("year", "urban_rural")) |> 
  tidyr::pivot_wider(id_cols = year, names_from = urban_rural, values_from = population) |>
  dplyr::mutate(
    proportion_urban = urban / (urban + rural)
  ) |>
  dplyr::mutate(source = "proportion_urban")

un_urban_check <- un_wup |>
  dplyr::select(-iso3c) |>
  dplyr::mutate(source = "un_proportion_urban")

urban_check_pd <- dplyr::bind_rows(urban_check, un_urban_check)

ggplot(data = urban_check_pd, aes(x = year, y = proportion_urban, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()


