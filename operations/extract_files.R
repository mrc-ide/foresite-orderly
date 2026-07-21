# Extract a named file from a report and put into an external directory --------
# This is a temporary solution whilst we get more formal distribution channels
# up and running.
get_file <- function(parameters, report, file){
  dest <- paste0("operations/extracted/", parameters$iso3c)

  if(!dir.exists(dest)){
    # recursive so the parent operations/extracted/ dir is created if absent
    dir.create(dest, recursive = TRUE)
  }

  condition_string <- paste0(
    "latest(",
    paste0(
      "parameter:", names(parameters), " == this:", names(parameters), collapse = " && "
    ),
    ")"
  )

  path <- orderly::orderly_search(
    condition_string,
    parameters = parameters,
    name = report
  )

  if(!is.na(path)){
    names(file) <- paste0(parameters$iso3c, "_", file)
    orderly::orderly_copy_files(path, files = file, dest = dest)
  }
}
# ------------------------------------------------------------------------------

# Run extraction ---------------------------------------------------------------
boundary <- "GADM_4.1.0"
isos <- list.files(paste0("src/data_boundaries/boundaries/", boundary, "/"))

parameters <- list(
  boundary = "GADM_4.1.0",
  iso3c = NULL,
  admin_level = 1,
  urban_rural = TRUE,
  # Keep in sync with the release you intend to extract (cf. push_packit.R).
  version = "malariaverse_06_2026"
)

for(iso in isos){
  parameters$iso3c <- iso
  get_file(parameters, "diagnostics", "diagnostic_report.pdf")
  get_file(parameters, "calibration", "calibrated_scaled_site.rds")
}
# ------------------------------------------------------------------------------
