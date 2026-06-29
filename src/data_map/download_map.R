# Check for updates here:
available_rasters <- malariaAtlas::listRaster()
View(dplyr::filter(available_rasters, max_raster_year == 2024))

download <- function(variable, years, address, overwrite = TRUE){
  for(year in years){
    print(year)
    rast <- malariaAtlas::getRaster(variable, year = paste(year))
    rast <- rast[[1]]
    terra::writeRaster(rast, paste0(address, year, ".tif"), overwrite = overwrite)
  }
}

# Net use (Africa)
download(
  variable = "Interventions__202508_Africa_Insecticide_Treated_Net_Use",
  years = 2000:2024,
  address = "src/data_map/data_site-2601/itn/202508_Africa_Insecticide_Treated_Net_Use_"
)

download(
  variable = "Interventions__202508_Africa_Insecticide_Treated_Net_Use_Rate",
  years = 2000:2024,
  address = "src/data_map/data_site-2601/itn_use_rate/202508_Africa_Insecticide_Treated_Net_Use_Rate_"
)


# Tx
download(
  variable = "Interventions__202508_Global_Antimalarial_Effective_Treatment",
  years = 2000:2024,
  address = "src/data_map/data_site-2601/tx/202508_Global_Antimalarial_Effective_Treatment_"
)

# IRS (Africa)
download(
  variable = "Interventions__202508_Africa_IRS_Coverage",
  years = 2000:2024,
  address = "src/data_map/data_site-2601/irs/202508_Africa_IRS_Coverage_"
)

# pfpr 2-10
download(
  variable = "Malaria__202508_Global_Pf_Parasite_Rate",
  years = 2000:2024,
  address = "src/data_map/data_site-2601/pfpr/202508_Global_Pf_Parasite_Rate_"
)

# pvpr all age
download(
  variable = "Malaria__202508_Global_Pv_Parasite_Rate",
  years = 2000:2024,
  address = "src/data_map/data_site-2601/pvpr/202508_Global_Pv_Parasite_Rate_"
)


