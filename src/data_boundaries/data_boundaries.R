# Orderly set-up ---------------------------------------------------------------
p <- orderly::orderly_parameters(
  boundary = NULL
)

boundary_files <- list.files(
  paste0("boundaries/", boundary, "/"),
  pattern = "*.RDS",
  full.names = TRUE,
  recursive = TRUE
)

orderly::orderly_resource(boundary_files)

orderly::orderly_artefact(
  description = "Boundary files",
  files = boundary_files
)

orderly::orderly_artefact(
  description = "Spatial extents",
  files = "extents.rds"
)
# ------------------------------------------------------------------------------

# Spatial extents --------------------------------------------------------------
# Record the bounding box of each country's admin-0 boundary, so downstream
# raster-clipping reports can work from a small extents table rather than the
# full boundary files.
boundary_isos <- list.files(paste0("boundaries/", boundary, "/"))

extents <- list()
for(iso in boundary_isos){
  admin0 <- readRDS(paste0("boundaries/", boundary, "/", iso, "/", iso, "_0.RDS"))
  extents[[iso]] <- as.vector(terra::ext(admin0))
}
saveRDS(extents, "extents.rds")
# ------------------------------------------------------------------------------
