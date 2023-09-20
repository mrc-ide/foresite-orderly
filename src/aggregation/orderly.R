# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Aggregation of pixel-level spatial data",
  long = "Processes raster data by country"
)

orderly2::orderly_parameters(
  iso3c = NULL,
  admin_level = NULL,
  end_year = NULL,
  current_year = NULL
)

orderly2::orderly_dependency(
  name = "process_data",
  query = "latest(parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level)",
  files = c(
    "population_pixel_values.rds",
    "pfpr_pixel_values.rds",
    "pvpr_pixel_values.rds",
    "population_demography.rds",
    "urbanisation.rds",
    "bednet_pixel_values.rds",
    "irs_pixel_values.rds",
    "treatment_pixel_values.rds",
    "gadm_df.rds"
  )
)

orderly2::orderly_artefact(
  description = "Population and population at risk by year and spatial unit", 
  files = "aggregated_population.rds"
)

orderly2::orderly_artefact(
  description = "Spatial intervention data by year and spatial unit", 
  files = "aggregated_interventions.rds"
)

orderly2::orderly_artefact(
  description = "MAP prevalence data by year and spatial unit", 
  files = "aggregated_prevalence.rds"
)
# ------------------------------------------------------------------------------

# Limits of transmission -------------------------------------------------------
pfpr <- readRDS("pfpr_pixel_values.rds")

pvpr <- readRDS("pvpr_pixel_values.rds") |>
  dplyr::mutate(pvpr = ifelse(pvpr == -1, NA, pvpr))

limits <- pfpr |>
  dplyr::left_join(
    pvpr,
    by = c("ID", "pixel", "year")
  ) |>
  dplyr::filter(
    year == 2000
  ) |>
  dplyr::mutate(
    pf_limits = ifelse(is.na(pfpr) | pfpr <= 0, 0, 1),
    pv_limits = ifelse(is.na(pvpr) | pvpr <= 0, 0, 1),
    limits = ifelse(pf_limits == 1 | pv_limits == 1, 1, 0)
  ) |>
  dplyr::select(
    ID, pixel, pf_limits, pv_limits, limits
  )
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
population_pixel <- readRDS("population_pixel_values.rds") |>
  tidyr::replace_na(list(population = 0))

# Expand to max_year
## This assumes the population spatial distribution across pixels remains the same
## in the future (each pixel population is scaled to account for population growth).
max_available <- population_pixel |>
  dplyr::filter(year == max(year))

future_population_pixel <- lapply((max(population_pixel$year) + 1):end_year, function(x){
  max_available |>
    dplyr::mutate(
      year = x
    )
}) |>
  dplyr::bind_rows() 

population_pixel <- dplyr::bind_rows(population_pixel, future_population_pixel)

# Rescaling to match UN WPP projections
un <- readRDS("population_demography.rds") |>
  dplyr::summarise(un_population = sum(population), .by = year)

population_scaler <- population_pixel |>
  dplyr::summarise(population = sum(population), .by = year) |>
  dplyr::left_join(un, by = "year") |>
  dplyr::mutate(
    scaler = un_population / population
  ) |>
  dplyr::select(
    "year", "scaler"
  )

population <- population_pixel |>
  dplyr::left_join(population_scaler, by = "year") |>
  dplyr::mutate(population = population * scaler) |>
  dplyr::select(-scaler)

rm(population_pixel)

# Assigning pixels as rural or urban to match UN estimates of proportion urban
## Pixels are assigned to "urban" starting with the most densely populated first
## and continuing until the UN WUP urban population size is met. After this point
## remaining pixels are assigned as "rural"
urban_pop <- readRDS("urbanisation.rds") |>
  dplyr::left_join(un, by = "year") |>
  dplyr::mutate(urban_population = un_population * proportion_urban) |>
  dplyr::select(year, urban_population)

population <- population |>
  dplyr::arrange(
    year, -population
  ) |>
  dplyr::mutate(cumsum_population = cumsum(population), .by = "year") |>
  dplyr::left_join(urban_pop, by = "year") |>
  dplyr::mutate(urban_rural = ifelse(
    cumsum_population <= urban_population, "urban", "rural"
  )) |>
  dplyr::arrange(
    ID, pixel
  ) |>
  dplyr::select(ID, pixel, urban_rural, year, population)

# Assigning population at risk
population <- population |>
  dplyr::left_join(
    limits,
    by = c("ID", "pixel")
  ) |>
  dplyr::mutate(
    population_at_risk = limits * population,
    population_at_risk_pf = pf_limits * population,
    population_at_risk_pv = pf_limits * population
  ) |>
  dplyr::select(ID, pixel, urban_rural, year, dplyr::contains("population"))
# ------------------------------------------------------------------------------

# Add prevalence ---------------------------------------------------------------
population_prevalence <- population |>
  dplyr::left_join(pfpr, by = c("ID", "pixel", "year")) |>
  dplyr::left_join(pvpr, by = c("ID", "pixel", "year"))

rm(population)
# ------------------------------------------------------------------------------

# Add interventions ------------------------------------------------------------
treatment_pixel_values <- readRDS("treatment_pixel_values.rds")
population_prevalence_interventions <- population_prevalence |>
  dplyr::left_join(treatment_pixel_values, by = c("ID", "pixel", "year"))
rm(population_prevalence)

# Spatial ITN and IRS data are available on for the African continent
continent <- countrycode::countrycode(iso3c, "iso3c", "continent")
if(continent == "Africa"){
  bednet_pixel_values <- readRDS("bednet_pixel_values.rds")
  irs_pixel_values <- readRDS("irs_pixel_values.rds")
  population_prevalence_interventions <- population_prevalence_interventions |>
    dplyr::left_join(bednet_pixel_values, by = c("ID", "pixel", "year")) |>
    dplyr::left_join(irs_pixel_values, by = c("ID", "pixel", "year"))
} else {
  population_prevalence_interventions <- population_prevalence_interventions |>
    dplyr::mutate(
      itn_use = NA,
      irs_coverage = NA
    )
}
# ------------------------------------------------------------------------------

# Aggregate --------------------------------------------------------------------
weighted.mean2 <- function(x, w, na.rm = TRUE){
  if(sum(w, na.rm = TRUE) == 0){
    out <- 0
  } else {
    out <- weighted.mean(x, w, na.rm = na.rm)
  }
  return(out)
}

aggregated <- population_prevalence_interventions |>
  dplyr::summarise(
    pfpr = weighted.mean2(pfpr, population_at_risk_pf),
    pvpr = weighted.mean2(pvpr, population_at_risk_pv),
    bednet_use = weighted.mean2(bednet_use, population_at_risk),
    irs_use = weighted.mean2(irs_use, population_at_risk),
    treatment_coverage = weighted.mean2(treatment, population_at_risk),
    population = sum(population),
    population_at_risk = sum(population_at_risk),
    population_at_risk_pf = sum(population_at_risk_pf),
    population_at_risk_pv = sum(population_at_risk_pv),
    .by = c("ID", "urban_rural", "year")
  )

aggregated_population <- aggregated |>
  dplyr::select("ID", "urban_rural", "year", dplyr::contains("population"))
saveRDS(aggregated_population, "aggregated_population.rds")

aggregated_interventions <- aggregated |>
  dplyr::select("ID", "urban_rural", "year", "treatment_coverage", "bednet_use", "irs_use") |>
  dplyr::filter(year <= current_year)
saveRDS(aggregated_interventions, "aggregated_interventions.rds")

aggregated_prevalence <- aggregated |>
  dplyr::select("ID", "urban_rural", "year", "pfpr", "pvpr") |>
  dplyr::filter(year <= current_year)
saveRDS(aggregated_prevalence, "aggregated_prevalence.rds")
# ------------------------------------------------------------------------------
