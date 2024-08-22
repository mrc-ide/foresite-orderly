# Orderly2 packit interaction --------------------------------------------------

# Adding the remote location to push packets -----------------------------------
# This has been done and shouldn't need to be done again.
# To get "GITHUB_PAT":
# 1. Generate a PAT on github (making sure that read:org permission is ticked)
# 2. Call usethis::edit_r_environ()
# 3. Add a new line with (e.g.) GITHUB_PAT = 'asj382058235u0sdij0486jj205270d'
add_new_location <- FALSE
if(add_new_location){
  orderly2::orderly_location_add(
    "packit.dide",
    type = "packit", 
    args = list(
      url = "https://packit.dide.ic.ac.uk/malariaverse-sitefiles",
      token = Sys.getenv("GITHUB_PAT")
    )
  )
}
# ------------------------------------------------------------------------------

# Pushing ----------------------------------------------------------------------
# Define the parameters to search for
parameters <- list(
  version = "GADM_4.1.0",
  iso3c = "NGA",
  admin_level = 1,
  urban_rural = TRUE
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
  name = "calibration_diagnostics"
)

# Push the packet the orderly location.
# This will push the packet and everything in the dependency tree. In this case
# that is a lot of large (raster) files.
orderly2::orderly_location_push(
  packet_id = packet_id,
  location = "packit.dide",
  root = NULL,
  locate = TRUE
)
# ------------------------------------------------------------------------------