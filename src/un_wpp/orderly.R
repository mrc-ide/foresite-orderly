# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Pre-processing UNWPP inputs",
  long = "Pre-processes very large UN WPP population and demography files to obtain
  single year age group death rates, population and population proportions"
)

orderly2::orderly_artefact(
  description = "UN WPP population and demography data", 
  files = "un_wpp.RDS"
)

orderly2::orderly_artefact(
  description = "UNICEF neonatal mortality rates", 
  files = "unicef_neonatal_mortality.RDS"
)

orderly2::orderly_parameters(
  start_year = NULL,
  end_year = NULL
)
# ------------------------------------------------------------------------------

external_data_address <- "C:/Users/pwinskil/OneDrive - Imperial College London/"

# Life tables ------------------------------------------------------------------
historical_life_tables <- read.csv(
  file = 
    paste0(
      external_data_address,
      "malaria_sites_data/2023/WPP2022_Life_Table_Complete_Medium_Both_1950-2021.csv"
    )
)
future_life_tables <- read.csv(
  file = paste0(
    external_data_address,
    "malaria_sites_data/2023/WPP2022_Life_Table_Complete_Medium_Both_2022-2100.csv"
  )
)

life_tables <- historical_life_tables |>
  dplyr::bind_rows(future_life_tables) |>
  dplyr::rename(
    iso3c = ISO3_code,
    year  =  Time,
    age_lower = AgeGrpStart,
    qx = qx
  ) |>
  dplyr::filter(
    year >= start_year,
    year <= end_year
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
historical_populations <- read.csv(
  file = paste0(
    external_data_address,
    "malaria_sites_data/2023/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv"
  )
)
future_populations <- read.csv(
  file = paste0(
    external_data_address,
    "malaria_sites_data/2023/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv"
  )
)

populations <- historical_populations |>
  dplyr::bind_rows(future_populations) |>
  dplyr::rename(
    iso3c = ISO3_code,
    year  =  Time,
    age_lower = AgeGrpStart,
    population = PopTotal
  ) |>
  dplyr::filter(
    year >= start_year,
    year <= end_year
  ) |>
  dplyr::mutate(
    population = as.integer(round(population  * 1000)),
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

saveRDS(un_wpp, "un_wpp.RDS")
# ------------------------------------------------------------------------------

# Neonatal mortality -----------------------------------------------------------
unicef_neonatal_mortality <- read.csv(
  file = paste0(
    external_data_address,
    "malaria_sites_data/2023/Neonatal_Mortality_Rates_2022.csv"
  )
) |>
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

saveRDS(unicef_neonatal_mortality, "unicef_neonatal_mortality.RDS")
# ------------------------------------------------------------------------------