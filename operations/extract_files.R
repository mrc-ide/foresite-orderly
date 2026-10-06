# Extract files ================================================================
#
# Copies named artefacts out of the orderly archive into a local folder, for
# manual inspection or ad-hoc sharing. Deliberately outside the orderly
# workflow. Output goes to operations/<folder>/, which is gitignored.
#
# Run section by section from the repo root, as with mission_control.R.

# Function ---------------------------------------------------------------------
# Copies `file` from the latest `report` packet matching `parameters`, for each
# ISO in `isos`, into `dest` as <iso>_<file>. ISOs with no matching packet are
# skipped with a message.
extract_files <- function(report, file, parameters, isos, dest){
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  for(iso in isos){
    parameters$iso3c <- iso
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

    if(is.na(path)){
      message("No ", report, " packet found for ", iso, ", skipping")
      next
    }
    files <- stats::setNames(file, paste0(iso, "_", file))
    orderly::orderly_copy_files(path, files = files, dest = dest)
  }
}

# Release to extract -----------------------------------------------------------
# Keep in sync with the release you intend to extract (cf. push_packit.R).
boundary <- "GADM_4.1.0"
version <- "malariaverse_06_2026"
isos <- list.files(paste0("src/data_boundaries/boundaries/", boundary))

release <- list(
  boundary = boundary,
  admin_level = 1,
  urban_rural = TRUE,
  version = version
)
out <- file.path("operations", version)

# Diagnostics: pre-calibration -------------------------------------------------
extract_files(
  report = "diagnostics",
  file = "diagnostic_report.pdf",
  parameters = c(release, calibration = FALSE),
  isos = isos,
  dest = file.path(out, "pre_calibration_diagnostics")
)

# Diagnostics: post-calibration ------------------------------------------------
extract_files(
  report = "diagnostics",
  file = "diagnostic_report.pdf",
  parameters = c(release, calibration = TRUE),
  isos = isos,
  dest = file.path(out, "post_calibration_diagnostics")
)

# Calibration epi output -------------------------------------------------------
extract_files(
  report = "calibration",
  file = "diagnostic_epi.rds",
  parameters = release,
  isos = isos,
  dest = file.path(out, "calibration_epi_output")
)

# Calibrated site files --------------------------------------------------------
extract_files(
  report = "calibration",
  file = "calibrated_scaled_site.rds",
  parameters = release,
  isos = isos,
  dest = file.path(out, "site_files")
)
