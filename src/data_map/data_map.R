# Orderly set-up ---------------------------------------------------------------
orderly::orderly_resource("data/")
orderly::orderly_resource("download_map.R")

orderly::orderly_shared_resource("utils.R")

orderly::orderly_dependency(
  name = "extents",
  query = "latest()",
  files = "extents.csv"
)
# ------------------------------------------------------------------------------

# Load global MAP rasters ------------------------------------------------------
extents <- read.csv("extents.csv")
isos <- extents$iso3c

# Read a set of yearly .tif files into one multi-layer raster named by year
raster_stack <- function(name, years){
  raster <- terra::rast(
    paste0("data/", name, years, ".tif")
  )
  names(raster) <- years
  return(raster)
}

# Interventions
itn_raster <- raster_stack("itn/202508_Africa_Insecticide_Treated_Net_Use_", 2000:2024)
irs_raster <- raster_stack("irs/202508_Africa_IRS_Coverage_", 2000:2024)
tx_raster <- raster_stack("tx/202508_Global_Antimalarial_Effective_Treatment_", 2000:2024)
smc_times <- paste0(
  rep(2012:2020, each = 12),
  ".",
  c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
)
smc_raster <- raster_stack("smc/SMC_", smc_times)

# Prevalence
pfpr_raster <- raster_stack("pfpr/202508_Global_Pf_Parasite_Rate_", 2000:2024)
pvpr_raster <- raster_stack("pvpr/202508_Global_Pv_Parasite_Rate_", 2000:2024)

# Access
cities_raster <- raster_stack("access/201501_Global_Travel_Time_to_Cities_", 2015)
motor_raster <- raster_stack("access/202001_Global_Motorized_Travel_Time_to_Healthcare_", 2019)
walk_raster <- raster_stack("access/202001_Global_Walking_Only_Travel_Time_To_Healthcare_", 2019)

# Blood disorders
hbc_raster <- raster_stack("blood_disorders/201201_Africa_HbC_Allele_Frequency_", 2010)
duffy_raster <- raster_stack("blood_disorders/201201_Global_Duffy_Negativity_Phenotype_Frequency_", 2010)
g6pd_raster <- raster_stack("blood_disorders/201201_Global_G6PDd_Allele_Frequency_", 2010)
sickle_raster <- raster_stack("blood_disorders/201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency_", 2010)
# ------------------------------------------------------------------------------

# Clip rasters to each country -------------------------------------------------
source("utils.R")

# Clip a global raster to one country's extent and write it, registering the
# result as an orderly artefact. Countries the raster does not cover are skipped
# (process_raster() returns NULL) unless force_out = TRUE.
split <- function(raster, extent, iso, name, NAflag = NULL, force_out = FALSE){
  raster <- process_raster(raster, extent, force_out)
  if(!is.null(raster)){
    address <- paste0("map/", iso, "/", name, ".tif")
    orderly::orderly_artefact(
      description = paste("MAP", name, "raster"),
      files = address
    )
    if(is.null(NAflag)){
      terra::writeRaster(raster, address)
    } else {
      terra::writeRaster(raster, address, NAflag = NAflag)
    }
  }
}

dir.create("map/")
paths <- paste0("map/", isos, "/")
make <- sapply(paths, dir.create)

# For each country, clip and write every MAP layer
for(iso in isos){
  extent <- terra::ext(unlist(extents[extents$iso3c == iso, 2:5]))

  split(itn_raster, extent, iso, "itn")
  split(irs_raster, extent, iso, "irs")
  split(tx_raster, extent, iso, "tx")
  split(smc_raster, extent, iso, "smc")
  split(pvpr_raster, extent, iso, "pvpr", -1, TRUE)
  split(pfpr_raster, extent, iso, "pfpr")
  split(cities_raster, extent, iso, "cities")
  split(motor_raster, extent, iso, "motor")
  split(walk_raster, extent, iso, "walk")
  split(hbc_raster, extent, iso, "hbc")
  split(duffy_raster, extent, iso, "duffy")
  split(g6pd_raster, extent, iso, "g6pd")
  split(sickle_raster, extent, iso, "sickle")
}
# ------------------------------------------------------------------------------

