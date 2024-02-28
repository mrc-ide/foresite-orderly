orderly2::orderly_parameters(
  boundary_version = NULL
)

boundaries_datafiles <- list.files(
  paste0("data/", boundary_version, "/"),
  pattern = "*.RDS", full.names = TRUE, recursive = TRUE
)

orderly2::orderly_resource(boundaries_datafiles)
orderly2::orderly_artefact(
  description = "Spatial boundaries",
  files = boundaries_datafiles
)
