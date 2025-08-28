# Check for updates here:
available_rasters <- malariaAtlas::listRaster()

download <- function(variable, years, address, overwrite = TRUE){
  for(year in years){
    print(year)
    rast <- malariaAtlas::getRaster("variable", year = paste(year))
    rast <- rast[[1]]
    terra::writeRaster(rast, paste0(address, year, ".tif"), overwrite = overwrite)
  }
}

# E.g. updating Net use
# Probably want to delete old rasters first
download(
  variable = "Interventions__202406_Africa_Insecticide_Treated_Net_Use",
  years = 2000:2022,
  address = "data/itn/202406_Africa_Insecticide_Treated_Net_Use_"
)