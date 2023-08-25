# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Create site file demography",
  long = "Processes demography and neonatal mortality data by country for creation 
  of site-file demography element"
)

orderly2::orderly_parameters(
  iso3c = NULL,
  start_year = NULL,
  end_year = NULL
)

orderly2::orderly_dependency(
  name = "process_data",
  query = "latest(parameter:iso3c == this:iso3c)",
  files = c("demography_data.rds", "neonatal_mortality_data.rds")
)

orderly2::orderly_artefact(
  description = "Demography data: Site file demography", 
  files = "demography.rds"
)
# ------------------------------------------------------------------------------

demography_data <- readRDS("demography_data.rds") |>
  dplyr::filter(
    year >= start_year,
    year <= end_year
  )  |>
  dplyr::mutate(
    adjusted_mortality_rates = peeps::estimate_mortality_rates(p, qx), .by = "year"
  )

neonatal_mortality_data <- readRDS("neonatal_mortality_data.rds") |>
  dplyr::filter(
    year >= start_year,
    year <= end_year
  ) |>
  dplyr::mutate(nnm = peeps::rescale_prob(nnm, 28, 365))

demography_sub_matrix <- matrix(
  demography_data$adjusted_mortality_rates,
  ncol = length(unique(demography_data$age_lower)),
  byrow = TRUE
)

neonatal_mortality_sub_matrix <- matrix(
  c(
    unlist(neonatal_mortality_data[,"nnm"]),
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

# Convert to daily for direct use in malariasimulation
demography_matrix <- peeps::rescale_prob(demography_matrix, 365, 1)

saveRDS(demography_matrix, "demography.rds")