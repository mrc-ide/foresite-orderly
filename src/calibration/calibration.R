# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Calibration",
  long = "Calibrates baseline EIR to match prevalence"
)

orderly2::orderly_parameters(
  boundary = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL
)

orderly2::orderly_resource(
  files = "calibration_utils.R"
)

orderly2::orderly_dependency(
  name = "site_file",
  query = "latest(parameter:boundary == this:boundary && parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural)",
  files = c("site.rds")
)

orderly2::orderly_artefact(
  description = "Raw list calibration output",
  files = "calibration_output_raw.rds"
)

orderly2::orderly_artefact(
  description = "Calibrated site",
  files = "calibrated_site.rds"
)
# ------------------------------------------------------------------------------

# Initial set up ---------------------------------------------------------------
library(sf)
site <- readRDS("site.rds")
source("calibration_utils.R")
# ------------------------------------------------------------------------------

# Calibration ------------------------------------------------------------------
parallel <- TRUE
human_population <- c(5000, 10000, 100000)
diagnostic_burnin <- 20 
max_attempts <- 30

# Split out individual jobs
eirs <- site$eir
eirs <- split(eirs, 1:nrow(eirs))

if(parallel){
  cores <- Sys.getenv("CCP_NUMCPUS")
  cluster <- parallel::makeCluster(as.integer(cores))
  invisible(parallel::clusterCall(cluster, ".libPaths", .libPaths()))
  parallel::clusterCall(cluster, function() {
    library(sf)
    library(site)
    library(postie)
    library(cali)
    library(knitr)
    library(dplyr)
    library(ggplot2)
    library(quarto)
    source("calibration_utils.R")
    TRUE
  })
  
  calibration_output <- parallel::parLapply(
    cl = cluster,
    X = eirs,
    fun = calibrate_site,
    site = site,
    human_population = human_population,
    diagnostic_burnin = diagnostic_burnin,
    max_attempts = max_attempts
  )
  parallel::stopCluster(cl = cluster)
} else {
  calibration_output <- lapply(
    X = eirs,
    FUN = calibrate_site,
    site = site,
    human_population = human_population,
    diagnostic_burnin = diagnostic_burnin,
    max_attempts = max_attempts
  )
}

saveRDS(calibration_output, "calibration_output_raw.rds")

# Collate EIR
eir_estimates <- 
  lapply(calibration_output, "[[", 1) |>
  dplyr::bind_rows()

site$eir <- eir_estimates
saveRDS(site, "calibrated_site.rds")
# ------------------------------------------------------------------------------
