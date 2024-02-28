rainfall_datafiles <- list.files("data/rainfall/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(rainfall_datafiles)
orderly2::orderly_artefact(
  description = "CHIRPS rainfall rasters",
  files = rainfall_datafiles
)
