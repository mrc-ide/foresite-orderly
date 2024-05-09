# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Calibration diagnostics",
  long = "Report to assess calibration performance"
)

orderly2::orderly_parameters(
  version = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL
)

orderly2::orderly_resource(
  files = "calibration_report.qmd"
)

orderly2::orderly_dependency(
  name = "calibration",
  query = "latest(parameter:version == this:version && parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural)",
  files = c("calibrated_site.rds", "calibration_plots.rds")
)

orderly2::orderly_artefact(
  description = "HTML calibration report",
  files = "calibration_report.html"
)
# ------------------------------------------------------------------------------

# Create diagnostic report -----------------------------------------------------
site <- readRDS("calibrated_site.rds")

quarto::quarto_render(
  input = "calibration_report.qmd",
  execute_params = list(
    iso3c = iso3c,
    country = site$country,
    admin_level = admin_level,
    version = site$version,
    n_sites = nrow(site$sites)
  )
)
# ------------------------------------------------------------------------------
