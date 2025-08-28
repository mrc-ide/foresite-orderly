# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Pre-processing UNWPP inputs",
  long = "Pre-processes very large UN WPP population and demography files to obtain
  single year age group death rates, population and population proportions"
)

orderly2::orderly_dependency(
  name = "data_un",
  query = "latest()",
  files = c(
    life_table_past.csv = "data/WPP2022_Life_Table_Complete_Medium_Both_1950-2021.csv",
    life_table_future.csv = "data/WPP2022_Life_Table_Complete_Medium_Both_2022-2100.csv",
    population_past.csv = "data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv",
    population_future.csv = "data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv",
    urbanisation_prospects.csv = "data/WUP2018-F02-Proportion_Urban.csv",
    neonatal_mortality.csv = "data/Neonatal_Mortality_Rates_2022.csv"
  )
)

orderly2::orderly_artefact(
  description = "UN WPP population and demography data", 
  files = "un_wpp.rds"
)

orderly2::orderly_artefact(
  description = "UNICEF neonatal mortality rates", 
  files = "unicef_neonatal_mortality.rds"
)

orderly2::orderly_artefact(
  description = "UN WUP population urbanisation", 
  files = "un_wup.rds"
)
# ------------------------------------------------------------------------------

# Life tables ------------------------------------------------------------------
historical_life_tables <- read.csv("life_table_past.csv")
future_life_tables <- read.csv("life_table_future.csv")

life_tables <- historical_life_tables |>
  dplyr::bind_rows(future_life_tables) |>
  dplyr::rename(
    iso3c = ISO3_code,
    year  =  Time,
    age_lower = AgeGrpStart,
    qx = qx
  ) |>
  dplyr::mutate(
    age_upper = ifelse(
      age_lower < 100, 
      age_lower + 1,
      age_lower + 100
    )
  )  |>
  dplyr::filter(
    !iso3c == ""
  ) |>
  dplyr::select(
    iso3c,
    year, 
    age_lower, 
    age_upper,
    qx
  )
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
historical_populations <- read.csv("population_past.csv")
future_populations <- read.csv("population_future.csv")

populations <- historical_populations |>
  dplyr::bind_rows(future_populations) |>
  dplyr::rename(
    iso3c = ISO3_code,
    year  =  Time,
    age_lower = AgeGrpStart,
    population = PopTotal
  ) |>
  dplyr::mutate(
    population = as.integer(round(population * 1000)),
    age_upper = ifelse(
      age_lower < 100, 
      age_lower + 1,
      age_lower + 100
    )
  )  |>
  dplyr::filter(
    !iso3c == ""
  ) |>
  dplyr::mutate(
    population_proportion = population / sum(population),
    .by = c("iso3c", "year")
  ) |>
  dplyr::select(
    iso3c,
    year, 
    age_lower, 
    age_upper,
    population,
    population_proportion
  ) 
# ------------------------------------------------------------------------------

# Combined ---------------------------------------------------------------------
un_wpp <- life_tables |>
  dplyr::left_join(
    populations,
    by = dplyr::join_by(iso3c, year, age_lower, age_upper)
  )

saveRDS(un_wpp, "un_wpp.rds")
# ------------------------------------------------------------------------------

# Neonatal mortality -----------------------------------------------------------
unicef_neonatal_mortality <- read.csv("neonatal_mortality.csv") |>
  dplyr::filter(
    Uncertainty.Bounds. == "Median",
    Country.Name %in% countrycode::codelist$country.name.en
  ) |>
  dplyr::mutate(iso3c = countrycode::countrycode(Country.Name, "country.name", "iso3c")) |>
  dplyr::filter(!is.na(iso3c)) |>
  dplyr::select(-c("Country.Name", "Uncertainty.Bounds.")) |>
  tidyr::pivot_longer(
    cols = -"iso3c",
    names_to = "year",
    values_to = "neonatal_mortality",
    names_prefix = "X"
  ) |>
  dplyr::mutate(
    year = as.integer(year),
    neonatal_mortality = neonatal_mortality / 1000
  )

saveRDS(unicef_neonatal_mortality, "unicef_neonatal_mortality.rds")
# ------------------------------------------------------------------------------

# Urbanisation -----------------------------------------------------------------
# Format and interpolate between 5 year bands
un_wup <- read.csv("urbanisation_prospects.csv") |>
  dplyr::rename(
    name = `Region..subregion..country.or.area`,
    code = "Country.code") |>
  dplyr::filter(
    code %in% countrycode::codelist$un
  ) |>
  dplyr::mutate(
    iso3c = countrycode::countrycode(code, "un", "iso3c", warn = FALSE)
  ) |>
  dplyr::select(-c("name", "code")) |>
  tidyr::pivot_longer(
    cols = -"iso3c",
    names_to = "year",
    values_to = "percent_urban",
    names_prefix = "X"
  ) |>
  dplyr::mutate(
    year = as.integer(year),
    proportion_urban = percent_urban / 100
  ) |>
  dplyr::select(-"percent_urban") |>
  tidyr::complete(year = min(year):max(year), tidyr::nesting(iso3c)) |>
  dplyr::arrange(iso3c, year) |>
  dplyr::mutate(
    proportion_urban = ifelse(
      is.na(proportion_urban),
      approx(year, proportion_urban, year)$y,
      proportion_urban
    ),
    .by = "iso3c"
  )

saveRDS(un_wup, "un_wup.rds")
# ------------------------------------------------------------------------------