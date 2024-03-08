orderly2::orderly_resource("data/")
orderly2::orderly_resource("README.md")

isos <- list.files("data/")

pop_years <- 2000:2020
dir.create("population/")
for(iso in isos){
  dir.create(paste0("population/", iso, "/"))
  address <- paste0("population/", iso, "/population.tif")
  orderly2::orderly_artefact(
    description = "WorldPop raster",
    files = address
  )
  pop_files <- paste0("data/", iso, "/population_", iso, "_", pop_years, ".tif")
  pop_raster <- terra::rast(pop_files)
  names(pop_raster) <- pop_years
  terra::writeRaster(pop_raster, address)
}

