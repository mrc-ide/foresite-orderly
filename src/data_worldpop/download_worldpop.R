
country_year_worldpop <- function(iso3c, year){
  
  raster_meta <- jsonlite::fromJSON(paste0(
    "https://www.worldpop.org/rest/data/pop/G2_CN_POP_R25A_1km?iso3=",
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
  
  dir <- paste0("src/data_worldpop/data_site-2601/", iso3c)
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  file <- paste0(dir, "/population_", iso3c, "_", year, ".tif")
  terra::writeRaster(pop, filename = file)
}

for(iso in malaria_endemic_isos){
  for(y in 2015:2030){
    print(iso)
    country_year_worldpop(iso, y)
  }
  for(y in 2000:2014){
    file.copy(
      from = paste0("src/data_worldpop/data/", iso, "/population_", iso, "_", y, ".tif"),
      to = paste0("src/data_worldpop/data_site-2601/", iso, "/population_", iso, "_", y, ".tif")
    )
  }
}