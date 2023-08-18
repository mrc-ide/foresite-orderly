orderly2::orderly_description(
  display = "Process data",
  long = "Processes raw input data by country in preparation for creation of
  site-file elements"
)

orderly2::orderly_parameters(iso3c = NULL)

orderly2::orderly_artefact(
  description = "Demography data: UNWPP mortality rates and age-structure", 
  files = "demography.rds"
)

orderly2::orderly_artefact(
  description = "Neonatal mortality data: UNICEF neonatal mortality rates", 
  files = "neonatal_mortality.rds"
)

external_data_address <- "C:/Users/pwinskil/OneDrive - Imperial College London/"

# Demography -------------------------------------------------------------------
demography_full <- readRDS(
  file = paste0(
    external_data_address,
    "demography/data/demography.RDS"
  )
)
neonatal_mortality_full <- readRDS(
  file = paste0(
    external_data_address,
    "demography/data/neonatal_mortality.RDS"
  )
)

demography <- demography_full |>
  dplyr::filter(iso3c == iso3c) |>
  dplyr::select("iso3c", "year", "qx", "p")

neonatal_mortality <- neonatal_mortality_full |>
  dplyr::filter(iso3c == iso3c)

saveRDS(demography, "demography.rds")
saveRDS(neonatal_mortality, "neonatal_mortality.rds")
# ------------------------------------------------------------------------------
