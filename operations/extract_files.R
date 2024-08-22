# Extract a named file from a report and put into an external directory --------
# This is a temporary solutin whilst we get more formal distribution channels
# up and running.
get_file <- function(parameters, report, file){
  dest <- paste0("operations/extracted/", report)
  
  if(!dir.exists(dest)){
    dir.create(dest)
  }
  
  condition_string <- paste0(
    "latest(",
    paste0(
      "parameter:", names(parameters), " == this:", names(parameters), collapse = " && "
    ),
    ")"
  )
  
  path <- orderly2::orderly_search(
    condition_string,
    parameters = parameters,
    name = report
  )
  
  if(!is.na(path)){
    names(file) <- paste0(parameters$iso3c, "_", file)
    orderly2::orderly_copy_files(path, files = file, dest = dest)
  }
}
# ------------------------------------------------------------------------------

# Run extraction ---------------------------------------------------------------
isos <- list.files(paste0("src/data_boundaries/boundaries/", version))

parameters <- list(
  version = "GADM_4.1.0",
  iso3c = NULL,
  admin_level = 1,
  urban_rural = TRUE
)

for(iso in isos){
  parameters$iso3c <- iso
  get_file(parameters, "diagnostics", "diagnostic_report.html")
  get_file(parameters, "calibration_diagnostics", "calibration_report.html")
  get_file(parameters, "calibration_diagnostics", "calibrated_scaled_site.rds")
}
# ------------------------------------------------------------------------------