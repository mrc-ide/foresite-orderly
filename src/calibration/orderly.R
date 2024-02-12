# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Calibration",
  long = "Calibrates baseline EIR to match prevalence"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "BFA",
  admin_level = 1,
  urban_rural = TRUE,
  overwrite = FALSE
)

orderly2::orderly_resource(
  files = "calibration_utils.R"
)

orderly2::orderly_dependency(
  name = "site_file",
  query = "latest(parameter:version_name == this:version_name && parameter:iso3c ==  this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural)",
  files = c("site.rds")
)
# ------------------------------------------------------------------------------

# Initial set up ---------------------------------------------------------------
site <- readRDS("site.rds")
source("calibration_utils.R")
# ------------------------------------------------------------------------------

# Calibration ------------------------------------------------------------------

# Split out individual jobs
eirs <- site$eir[1,]
if(!overwrite){
  eirs <- eirs |>
    dplyr::filter(is.na(eir))
}
eirs <- split(eirs, 1:nrow(eirs))

calibrated_eirs <- lapply(
  X = eirs,
  FUN = calibrate,
  site = site,
  human_population = 1000,
  burnin = 0,
  max_attempts = 10,
  eir_limits = c(0, 1000)
)

# ------------------------------------------------------------------------------