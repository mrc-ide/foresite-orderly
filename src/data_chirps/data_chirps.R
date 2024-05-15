orderly2::orderly_parameters(
  version = NULL
)

orderly2::orderly_resource("data/")
orderly2::orderly_resource("README.md")
orderly2::orderly_resource("download_chirps.R")

orderly2::orderly_shared_resource("utils.R")

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:version == this:version)",
  files = "extents.rds"
)

extents <- readRDS("extents.rds")
isos <- names(extents)
# TODO: Why does this not cleanup properly?
country_boundary_files <- paste0("boundaries/", version, "/", isos, "/", isos, "_0.RDS")
names(country_boundary_files) <- paste0("country_boundaries/", isos, "_0.RDS")
orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:version == this:version)",
  files = country_boundary_files
)


rainfall_datafiles <- list.files("data/", pattern = "*.tif", full.names = TRUE)
rainfall_stack <- terra::rast(rainfall_datafiles)

source("utils.R")

dir.create("rainfall/")
for(iso in isos){
  dir.create(paste0("rainfall/", iso, "/"))
  boundary <- readRDS(paste0("country_boundaries/", iso, "_0.RDS"))
  raster <- process_raster(rainfall_stack, boundary)
  if(!is.null(raster)){
    address <- paste0("rainfall/", iso, "/rainfall.tif")
    orderly2::orderly_artefact(
      description = "CHIRPS rainfall raster",
      files = address
    )
    terra::writeRaster(raster, address, NAflag = -9999)
  }
}

