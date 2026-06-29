orderly::orderly_resource("data/")
orderly::orderly_resource("download_chirps.R")

orderly::orderly_shared_resource("utils.R")

orderly::orderly_dependency(
  name = "extents",
  query = "latest()",
  files = "extents.csv"
)

extents <- read.csv("extents.csv")
isos <- extents$iso3c

rainfall_datafiles <- list.files("data/", pattern = "*.tif", full.names = TRUE)
rainfall_stack <- terra::rast(rainfall_datafiles)

source("utils.R")

dir.create("rainfall/")
for(iso in isos){
  dir.create(paste0("rainfall/", iso, "/"))
  extent <- terra::ext(unlist(extents[extents$iso3c == iso, 2:5]))
  raster <- process_raster(rainfall_stack, extent)
  if(!is.null(raster)){
    address <- paste0("rainfall/", iso, "/rainfall.tif")
    orderly::orderly_artefact(
      description = "CHIRPS rainfall raster",
      files = address
    )
    terra::writeRaster(raster, address, NAflag = -9999)
  }
}
