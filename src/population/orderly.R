# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Population projections",
  long = "Creates population and age-disaggregated population projections at
  the requested level of spatial aggregration. Population urbanisation is 
  spatially explicit for (past) years where we have WorldPop data. After that,
  we assumme the same spatial distirbution of urban populations moving forwards,
  from 2050 onwards we do not have UN WUP projections, so the rate of urbanisation
  is extrapolted forwards. It is therefore possible that you will see artefacts 
  at these two methodological change points."
)

orderly2::orderly_parameters(
  version_name = NULL,
  iso3c = NULL
)

orderly2::orderly_resource(
  files = "population_utils.R"
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = c("un_wup.rds", "un_wpp.rds")
)

orderly2::orderly_dependency(
  name = "spatial",
  query = "latest(parameter:version_name == this:version_name && parameter:iso3c ==  this:iso3c)",
  files = c("spatial.rds")
)

orderly2::orderly_artefact(
  description = "Population",
  files = "population.rds"
)

orderly2::orderly_artefact(
  description = "Age-disaggregated population",
  files = "population_age.rds"
)
# ------------------------------------------------------------------------------

# Load inputs ------------------------------------------------------------------
un_wpp <- readRDS("un_wpp.rds") |>
  dplyr::filter(
    iso3c == {{iso3c}},
    year >= 2000
  ) |>
  dplyr::select(iso3c, year, age_lower, age_upper, population)
un_wup <- readRDS("un_wup.rds") |>
  dplyr::filter(
    iso3c == {{iso3c}},
    year >= 2000
  )
spatial <- readRDS("spatial.rds")
# ------------------------------------------------------------------------------

# Future urbanisation ----------------------------------------------------------
source("population_utils.R")
# TODO: should this be earlier to = last pop raster input date?
msy <-  2020

future_urbanisation <-
  un_wup |>
  tidyr::complete(
    iso3c = {{iso3c}},
    year = 2000:2100
  ) |>
  dplyr::mutate(
    proportion_urban = extrapolate_with_bounds_polynomial(year, proportion_urban, 2050)
  ) |>
  dplyr::mutate(
    proportion_rural = 1 - proportion_urban,
    urban = ifelse(year > msy, proportion_urban / proportion_urban[year == msy], 1),
    rural = ifelse(year > msy, proportion_rural / proportion_rural[year == msy], 1)
  ) |>
  tidyr::pivot_longer(
    c(urban, rural),
    names_to = "urban_rural",
    values_to = "adjustment" 
  ) |>
  dplyr::select(iso3c, urban_rural, year, adjustment)
# ------------------------------------------------------------------------------

# Aggregation ------------------------------------------------------------------
aggregation_levels <- c("country", "iso3c", "name_1", "name_2", "name_3", "urban_rural") 
aggregation_levels <- aggregation_levels[aggregation_levels %in% names(spatial)]
years <- min(spatial$year):max(un_wpp$year)

population_age <- spatial |>
  dplyr::summarise(
    pop = sum(pop),
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pf),
    .by = dplyr::all_of(c(aggregation_levels, "year"))
  ) |>
  # Add missing year/place combinations
  dplyr::group_by(dplyr::across(dplyr::all_of(aggregation_levels))) |>
  tidyr::complete(year = years) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    pop = ifelse(year <= max(spatial$year) & is.na(pop), 0, pop),
    par = ifelse(year <= max(spatial$year) & is.na(par), 0, par),
    par_pf = ifelse(year <= max(spatial$year) & is.na(par_pf), 0, par_pf),
    par_pv = ifelse(year <= max(spatial$year) & is.na(par_pv), 0, par_pv)
  ) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(aggregation_levels))) |>
  tidyr::fill(c("pop", "par", "par_pf", "par_pv"), .direction = "down") |>
  dplyr::ungroup() |>
  # Estimate proportions (as proportion of total)
  dplyr::mutate(
    pop_prop = pop / sum(pop),
    par_prop = par / sum(pop),
    par_pf_prop = par_pf / sum(pop),
    par_pv_prop = par_pv / sum(pop),
    .by = c("year")
  ) |>
  # Drop numbers
  dplyr::select(-c("pop", "par", "par_pf", "par_pv")) |>
  # Adjust to include urbanisation in future years
  dplyr::left_join(future_urbanisation,
                   by = c("iso3c", "year", "urban_rural"),
                   relationship = "many-to-many") |>
  tidyr::replace_na(
    list(
      adjustment = 1
    )
  ) |>
  dplyr::mutate(
    pop_prop = pop_prop * adjustment,
    par_prop = par_prop * adjustment,
    par_pf_prop = par_pf_prop * adjustment,
    par_pv_prop = par_pv_prop * adjustment,
  ) |>
  # Link to UN projections
  dplyr::left_join(un_wpp, by = c("iso3c", "year"), relationship = "many-to-many") |>
  dplyr::mutate(
    pop = round(population * pop_prop),
    par = round(population * par_prop),
    par_pf = round(population * par_pf_prop),
    par_pv = round(population * par_pv_prop)
  ) |>
  dplyr::select(dplyr::all_of(c(aggregation_levels, "year", "age_lower", "age_upper", "pop", "par", "par_pf", "par_pv")))

population <- population_age |>
  dplyr::summarise(
    pop = sum(pop),
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = dplyr::all_of(c(aggregation_levels, "year"))
  )

saveRDS(population, "population.rds")
saveRDS(population_age, "population_age.rds")
# ------------------------------------------------------------------------------
