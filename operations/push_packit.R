# Push to packit ===============================================================
#
# Publishes calibrated site files to the malariaverse packit server. Each push
# sends one country's calibration packet plus its whole dependency tree (incl.
# large rasters), so countries are pushed one at a time.
#
# Run section by section from the repo root, as with mission_control.R.

# Setup (one-off) --------------------------------------------------------------
# Needs a GitHub PAT with read:org scope in .Renviron:
# 1. Generate a PAT on GitHub (tick read:org)
# 2. Call usethis::edit_r_environ()
# 3. Add a line: GITHUB_PAT = '<your token>'
location <- "packit.dide2"
if(!location %in% orderly::orderly_location_list()){
  orderly::orderly_location_add_packit(
    name = location,
    url = "https://malariaverse-sitefiles.packit.dide.ic.ac.uk/"
  )
}

# Function ---------------------------------------------------------------------
# Pushes the latest calibration packet matching `parameters` for each ISO in
# `isos` to `location`. ISOs with no matching packet, or whose push fails, are
# skipped and returned so they can be retried.
push_sitefiles <- function(parameters, isos, location, dry_run = FALSE){
  failed <- character()

  for(iso in isos){
    parameters$iso3c <- iso
    condition_string <- paste0(
      "latest(",
      paste0(
        "parameter:", names(parameters), " == this:", names(parameters), collapse = " && "
      ),
      ")"
    )

    packet_id <- orderly::orderly_search(
      condition_string,
      parameters = parameters,
      name = "calibration"
    )

    if(is.na(packet_id)){
      message("No calibration packet found for ", iso, ", skipping")
      failed <- c(failed, iso)
      next
    }

    message("Pushing ", iso, " (", packet_id, ")")
    pushed <- tryCatch({
      orderly::orderly_location_push(
        expr = packet_id,
        location = location,
        dry_run = dry_run
      )
      TRUE
    }, error = function(e){
      message("Push failed for ", iso, ": ", conditionMessage(e))
      FALSE
    })
    if(!pushed) failed <- c(failed, iso)
  }

  invisible(failed)
}

# Release to push --------------------------------------------------------------
# Keep in sync with the release you intend to push (cf. extract_files.R).
boundary <- "GADM_4.1.0"
version <- "malariaverse_06_2026"
isos <- list.files(paste0("src/data_boundaries/boundaries/", boundary))

release <- list(
  boundary = boundary,
  admin_level = 1,
  urban_rural = TRUE,
  version = version
)

# Push -------------------------------------------------------------------------
# Check one country with a dry run first
push_sitefiles(release, isos = "TGO", location = location, dry_run = TRUE)

failed <- push_sitefiles(release, isos = isos, location = location)
failed

# Debugging: largest file in src/ ----------------------------------------------
# Handy if a push is unexpectedly large or slow.
find_largest_file <- function(directory) {
  # List all files in the directory and subdirectories
  files <- list.files(directory, recursive = TRUE, full.names = TRUE)

  # Filter out directories, we only want files
  files <- files[file.info(files)$isdir == FALSE]

  # Get file sizes
  file_sizes <- file.info(files)$size

  # Find the index of the largest file
  largest_file_index <- which.max(file_sizes)

  # Return the largest file and its size
  largest_file <- files[largest_file_index]
  largest_size <- file_sizes[largest_file_index]

  return(list(file = largest_file, size = largest_size))
}

# flf <- find_largest_file("src/")
