# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Mortality rate adjustments",
  long = "Adjusts UN mortality rates such that a model run with a fixed simulation
  population size would return the observed age distribution (at equilibrium)."
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "BFA"
)

orderly2::orderly_resource("adjust_rates.R")

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = c("un_wpp.rds")
)

orderly2::orderly_artefact(
  description = "Adjusted mortality rates",
  files = "adjusted_demography.rds"
)
# ------------------------------------------------------------------------------

# Prepare data for parallel processing -----------------------------------------
demography <- readRDS("un_wpp.rds") |>
  dplyr::filter(
    year >= 2000,
    iso3c == {{iso3c}}
  )
demography_split <- dplyr::group_split(demography, iso3c, year)

source("adjust_rates.R")

cores <- Sys.getenv("CCP_NUMCPUS")

cluster <- parallel::makeCluster(as.integer(cores))

invisible(parallel::clusterCall(cluster, ".libPaths", .libPaths()))

parallel::clusterCall(cluster, function() {
  library(peeps)
  source("adjust_rates.R")
  TRUE
})

adjusted_demography_split <- parallel::parLapply(
  cl = cluster,
  X = demography_split,
  fun = adjust_rates
)

parallel::stopCluster(cl = cluster)

adjusted_demography <- dplyr::bind_rows(adjusted_demography_split)

saveRDS(adjusted_demography, "adjusted_demography.rds")
# ------------------------------------------------------------------------------
