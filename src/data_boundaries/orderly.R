orderly2::orderly_parameters(
  boundary_version = NULL
)

boundary_files <- list.files(
  paste0("boundaries/", boundary_version, "/"),
  pattern = "*.RDS",
  full.names = TRUE,
  recursive = TRUE
)

orderly2::orderly_resource(boundary_files)
orderly2::orderly_artefact(
  description = "Boundary files",
  files = boundary_files
)

orderly2::orderly_artefact(
  description = "Spatial extents",
  files = "extents.rds"
)

# Get spatial extent for each country
isos <- list.files(paste0("boundaries/", boundary_version, "/"))

extents <- list()
for(iso in isos){
  admin0 <- readRDS(paste0("boundaries/", boundary_version, "/", iso, "/", iso, "_0.RDS"))
  extents[[iso]] <- as.vector(terra::ext(admin0))
}
saveRDS(extents, "extents.rds")
