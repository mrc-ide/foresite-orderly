# Orderly2 packit interaction --------------------------------------------------

# Adding the remote location to push packets -----------------------------------
# This has been done and shouldn't need to be done again.
# To get "GITHUB_PAT":
# 1. Generate a PAT on github (making sure that read:org permission is ticked)
# 2. Call usethis::edit_r_environ()
# 3. Add a new line with (e.g.) GITHUB_PAT = 'asj382058235u0sdij0486jj205270d'
add_new_location <- FALSE
if(add_new_location){
  orderly2::orderly_location_add_packit(
    "packit.dide",
    url = "https://packit.dide.ic.ac.uk/malariaverse-sitefiles"
  )
}
# ------------------------------------------------------------------------------

# Pushing ----------------------------------------------------------------------
# Define the parameters to search for
parameters = list(
  boundary = "GADM_4.1.0",
  iso3c = "ETH",
  admin_level = 3,
  urban_rural = FALSE,
  version = "ETH_admin_3_request_08_2025"
)

condition_string <- paste0(
  "latest(",
  paste0(
    "parameter:", names(parameters), " == this:", names(parameters), collapse = " && "
  ),
  ")"
)

# Search for the latest packet ID
packet_id <- orderly2::orderly_search(
  condition_string,
  parameters = parameters,
  name = "calibration"
)

# Push the packet the orderly location.
# This will push the packet and everything in the dependency tree. In this case
# that is a lot of large (raster) files.
orderly2::orderly_location_push(
  expr = packet_id,
  location = "packit.dide",
  root = NULL,
  dry_run = FALSE
)
# ------------------------------------------------------------------------------

# Finding the largest file in the src tree -------------------------------------
# A helper to search for the largest file in the src directory and subdirs,
# could be helpful if debugging push.
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
# ------------------------------------------------------------------------------