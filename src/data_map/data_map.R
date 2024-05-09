orderly2::orderly_parameters(
  version = NULL
)

orderly2::orderly_resource("data/")
orderly2::orderly_resource("README.md")

orderly2::orderly_shared_resource("utils.R")

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:version == this:version)",
  files = "extents.rds"
)

extents <- readRDS("extents.rds")
isos <- names(extents)

raster_stack <- function(name, years){
  raster <- terra::rast(
    paste0("data/", name, years, ".tif")
  )
  names(raster) <- years
  return(raster)
}

# Interventions
itn_raster <- raster_stack("itn/202106_Africa_Insecticide_Treated_Net_Use_", 2000:2020)
irs_raster <- raster_stack("irs/202106_Africa_Indoor_Residual_Spraying_Coverage_", 2000:2020)
tx_raster <- raster_stack("tx/202106_Global_Antimalarial_Effective_Treatment_", 2000:2020) 
smc_times <- paste0(
  rep(2012:2020, each = 12),
  ".",
  c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
)
smc_raster <- raster_stack("smc/SMC_", smc_times) 

# Prevalence
pfpr_raster <- raster_stack("pfpr/202206_Global_Pf_Parasite_Rate_", 2000:2020)
pvpr_raster <- raster_stack("pvpr/202206_Global_Pv_Parasite_Rate_", 2000:2020) 

# Access
cities_raster <- raster_stack("access/201501_Global_Travel_Time_to_Cities_", 2015) 
motor_raster <- raster_stack("access/202001_Global_Motorized_Travel_Time_to_Healthcare_", 2019) 
walk_raster <- raster_stack("access/202001_Global_Walking_Only_Travel_Time_To_Healthcare_", 2019) 

# Blood disorders
hbc_raster <- raster_stack("blood_disorders/201201_Africa_HbC_Allele_Frequency_", 2010) 
duffy_raster <- raster_stack("blood_disorders/201201_Global_Duffy_Negativity_Phenotype_Frequency_", 2010) 
g6pd_raster <- raster_stack("blood_disorders/201201_Global_G6PDd_Allele_Frequency_", 2010) 
sickle_raster <- raster_stack("blood_disorders/201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency_", 2010) 

source("utils.R")

split <- function(raster, extent, iso, name, NAflag = NULL){
  if(extents_overlap(raster, extent)){
    address <- paste0("map/", iso, "/", name, ".tif")
    cropped <- terra::crop(raster, extent)
    orderly2::orderly_artefact(
      description = paste("MAP", name, "raster"),
      files = address
    )
    if(is.null(NAflag)){
      terra::writeRaster(cropped, address)
    } else {
      terra::writeRaster(cropped, address, NAflag = NAflag)
    }
  }
}

dir.create("map/")
paths <- paste0("map/", isos, "/")
make <- sapply(paths, dir.create)

for(iso in isos){
  extent <- terra::ext(extents[[iso]])
  
  split(itn_raster, extent, iso, "itn")
  split(irs_raster, extent, iso, "irs")
  split(tx_raster, extent, iso, "tx")
  split(smc_raster, extent, iso, "smc")
  split(pvpr_raster, extent, iso, "pvpr",  -1)
  split(pfpr_raster, extent, iso, "pfpr")
  split(cities_raster, extent, iso, "cities")
  split(motor_raster, extent, iso, "motor")
  split(walk_raster, extent, iso, "walk")
  split(hbc_raster, extent, iso, "hbc")
  split(duffy_raster, extent, iso, "duffy")
  split(g6pd_raster, extent, iso, "g6pd")
  split(sickle_raster, extent, iso, "sickle")
}

