# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Create site file demography",
  long = "Processes demography and neonatal mortality data by country for creation 
  of site-file demography element"
)

orderly2::orderly_parameters(
  iso3c = NULL
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = c(
    "un_wpp.rds",
    "unicef_neonatal_mortality.rds"
  )
)

orderly2::orderly_artefact(
  description = "Demography data: Site file demography", 
  files = "demography.rds"
)
# ------------------------------------------------------------------------------

# Make demography adjustments --------------------------------------------------
demography_data <- readRDS("un_wpp.rds") |>
  dplyr::filter(iso3c == {{iso3c}}) |>
  dplyr::mutate(
    adjusted_mortality_rates = peeps::estimate_mortality_rates(population_proportion, qx),
    .by = "year"
  )

neonatal_mortality_data <- readRDS("unicef_neonatal_mortality.rds") |>
  dplyr::filter(iso3c == {{iso3c}}) |>
  dplyr::mutate(neonatal_mortality = peeps::rescale_prob(neonatal_mortality, 28, 365))

demography_sub_matrix <- matrix(
  demography_data$adjusted_mortality_rates,
  ncol = length(unique(demography_data$age_lower)),
  byrow = TRUE
)

neonatal_mortality_sub_matrix <- matrix(
  c(
    unlist(neonatal_mortality_data[,"neonatal_mortality"]),
    rep(NA, length(unique(demography_data$year)) - nrow(neonatal_mortality_data))
  ),
  ncol = 1
)

# Fill future NAs for NNM by extrapolating trend to match UNWPP trend in 0-1 year olds
if(sum(is.na(neonatal_mortality_sub_matrix)) > 0){
  last_data <- which(is.na(neonatal_mortality_sub_matrix[,1]))[1] - 1
  age_0_relative <- demography_sub_matrix[, 1]  / demography_sub_matrix[last_data, 1] 
  extrapolated_nnm <- neonatal_mortality_sub_matrix[last_data, 1] * age_0_relative
  index <- (last_data + 1):nrow(neonatal_mortality_sub_matrix)
  neonatal_mortality_sub_matrix[index, 1] <- extrapolated_nnm[index]
}

demography_matrix <- cbind(
  neonatal_mortality_sub_matrix,
  demography_sub_matrix
)
# ------------------------------------------------------------------------------

# Diagnostics ------------------------------------------------------------------

# ------------------------------------------------------------------------------

# Site file element ------------------------------------------------------------
# Convert to daily for direct use in malariasimulation
demography <- peeps::rescale_prob(demography_matrix, 365, 1)
saveRDS(demography, "demography.rds")
# ------------------------------------------------------------------------------