orderly2::orderly_parameters(
  boundary_version = "GADM_4.1.0"
)

orderly2::orderly_resource("data/")
orderly2::orderly_resource("README.md")
orderly2::orderly_resource("download_chirps.R")

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:boundary_version == this:boundary_version)",
  files = "extents.rds"
)

rainfall_datafiles <- list.files("data/", pattern = "*.tif", full.names = TRUE)
rainfall_stack <- terra::rast(rainfall_datafiles)
extents <- readRDS("extents.rds")
isos <- names(extents)

# Check if raster extents overlap
extents_overlap <- function(x, extent){
  extent_x <- terra::ext(x)
  overlap <- TRUE
  if(is.null(terra::intersect(extent_x, extent))){
    overlap <- FALSE
  }
  return(overlap)
}

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

