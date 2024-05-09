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

rainfall_datafiles <- list.files("data/", pattern = "*.tif", full.names = TRUE)
rainfall_stack <- terra::rast(rainfall_datafiles)
extents <- readRDS("extents.rds")
isos <- names(extents)

source("utils.R")

dir.create("rainfall/")
for(iso in isos){
  dir.create(paste0("rainfall/", iso, "/"))
  extent <- terra::ext(extents[[iso]])
  if(extents_overlap(rainfall_stack, extent)){
    address <- paste0("rainfall/", iso, "/rainfall.tif")
    orderly2::orderly_artefact(
      description = "CHIRPS rainfall raster",
      files = address
    )
    chirps <- terra::crop(rainfall_stack, extent)
    terra::writeRaster(chirps, address, NAflag = -9999)
  }
}

