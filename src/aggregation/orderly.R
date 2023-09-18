# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Aggregation of pixel-level spatial data",
  long = "Processes raster data by country"
)

orderly2::orderly_parameters(
  iso3c = NULL,
  admin_level = NULL,
  end_year = NULL
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
  description = "Demography data: Site file demography", 
  files = "population.rds"
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
    pf_limits = ifelse(is.na(pfpr) | pfpr == 0, 0, 1),
    pv_limits = ifelse(is.na(pvpr) | pvpr <= 0, 0, 1),
    limits = ifelse(pf_limits == 1 | pv_limits == 1, 1, 0)
  ) |>
  dplyr::select(
    ID, pixel, pf_limits, pv_limits, limits
  )
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
population_pixel <- readRDS("population_pixel_values.rds")

# Expand to max_year
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

# Rescaling to match current UN WPP projections
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
gadm_df <- readRDS("gadm_df.rds")
aggregated <- population_prevalence_interventions |>
  dplyr::summarise(
    pfpr = weighted.mean(pfpr, population_at_risk_pf, na.rm = TRUE),
    pvpr = weighted.mean(pvpr, population_at_risk_pv, na.rm = TRUE),
    bednet_use = weighted.mean(bednet_use, population_at_risk, na.rm = TRUE),
    irs_use = weighted.mean(irs_use, population_at_risk, na.rm = TRUE),
    treatment_coverage = weighted.mean(treatment, population_at_risk, na.rm = TRUE),
    population = sum(population),
    population_at_risk = sum(population_at_risk),
    population_at_risk_pf = sum(population_at_risk_pf),
    population_at_risk_pv = sum(population_at_risk_pv),
    .by = c("ID", "urban_rural", "year")
  ) |>
  dplyr::left_join(gadm_df, by = c("ID" = "id"))
# ------------------------------------------------------------------------------

# TODO:
# CHECK NA remove assumptions make sense.
# Split out and save aggregated parts
