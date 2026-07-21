
# One-off data-acquisition script: downloads per-country, per-year WorldPop 1km
# population rasters into src/data_worldpop/data/<iso3c>/. Run manually from the
# repo root (declared as an orderly_resource for provenance).

# Download a single country-year raster and save it under the country's folder
country_year_worldpop <- function(iso3c, year, url = "https://www.worldpop.org/rest/data/pop/G2_CN_POP_R25A_1km?iso3="){

  raster_meta <- jsonlite::fromJSON(paste0(
    url,
    iso3c
  ))

  raster_files <- raster_meta$data |>
    dplyr::filter(popyear == as.character(year)) |>
    dplyr::select(files) |>
    unlist()

  raster_file <- raster_files[grepl(".tif", raster_files)]

  td <- tempdir()
  raster_address <- paste0(td, "/", iso3c, ".tif")
  df <- utils::download.file(
    url = raster_file,
    destfile = raster_address,
    mode = "wb"
  )
  pop <- terra::rast(raster_address)
  names(pop) <- "pop"

  dir <- paste0("src/data_worldpop/data/", iso3c)
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  file <- paste0(dir, "/population_", iso3c, "_", year, ".tif")
  terra::writeRaster(pop, filename = file, overwrite = TRUE)
}

# WorldPop serves the 2000-2014 back-series from a different endpoint than
# 2015 onwards, so fetch the two ranges separately
for(iso in malaria_endemic_isos){
  print(iso)
  for(y in 2000:2014){
    country_year_worldpop(iso, y,
                          url = "https://www.worldpop.org/rest/data/pop/wpicuadj1km?iso3=")
  }
  for(y in 2015:2030){
    country_year_worldpop(iso, y)
  }
}
