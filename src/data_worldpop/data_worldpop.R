orderly::orderly_resource("data/")
orderly::orderly_resource("download_worldpop.R")
orderly::orderly_resource("README.md")

isos <- list.files("data/")


# Get the intersection of all extents
get_common_extent <- function(file_list) {
  extents <- lapply(file_list, function(f) terra::ext(terra::rast(f)))
  
  # Find intersection
  xmin <- max(sapply(extents, function(e) e$xmin))
  xmax <- min(sapply(extents, function(e) e$xmax))
  ymin <- max(sapply(extents, function(e) e$ymin))
  ymax <- min(sapply(extents, function(e) e$ymax))
  
  return(terra::ext(xmin, xmax, ymin, ymax))
}

pop_years <- 2000:2030
dir.create("population/")
for(iso in isos){
  print(iso)
  dir.create(paste0("population/", iso, "/"))
  address <- paste0("population/", iso, "/population.tif")
  orderly::orderly_artefact(
    description = "WorldPop raster",
    files = address
  )
  pop_files <- paste0("data/", iso, "/population_", iso, "_", pop_years, ".tif")
  country_template <- terra::rast(tail(pop_files, 1))
  pop_rasters <- lapply(pop_files, function(f) {
    r <- terra::rast(f)
    terra::resample(r, country_template, method = "bilinear")
  })
  pop_raster <- terra::rast(pop_rasters)
  names(pop_raster) <- pop_years
  terra::writeRaster(pop_raster, address)
}

