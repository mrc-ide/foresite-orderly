orderly2::orderly_parameters(
  boundary_version = "GADM_4.1.0"
)

orderly2::orderly_resource("data/")
orderly2::orderly_resource("README.md")

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:boundary_version == this:boundary_version)",
  files = "extents.rds"
)

extents <- readRDS("extents.rds")
isos <- names(extents)

itn_raster <- terra::rast(list.files("data/itn/", pattern = "*.tif", full.names = TRUE))
irs_raster <- terra::rast(list.files("data/irs/", pattern = "*.tif", full.names = TRUE))
tx_raster <- terra::rast(list.files("data/tx/", pattern = "*.tif", full.names = TRUE))
smc_raster <- terra::rast(list.files("data/smc/", pattern = "*.tif", full.names = TRUE))
pfpr_raster <- terra::rast(list.files("data/pfpr/", pattern = "*.tif", full.names = TRUE))
pvpr_raster <- terra::rast(list.files("data/pvpr/", pattern = "*.tif", full.names = TRUE))
cities_raster <- terra::rast("data/access/201501_Global_Travel_Time_to_Cities_2015.tif")
motor_raster <- terra::rast("data/access/202001_Global_Motorized_Travel_Time_to_Healthcare_2019.tif")
walk_raster <- terra::rast("data/access/202001_Global_Walking_Only_Travel_Time_To_Healthcare_2019.tif")
hbc_raster <- terra::rast("data/blood_disorders/201201_Africa_HbC_Allele_Frequency_2010.tif")
duffy_raster <- terra::rast("data/blood_disorders/201201_Global_Duffy_Negativity_Phenotype_Frequency_2010.tif")
g6pd_raster <- terra::rast("data/blood_disorders/201201_Global_G6PDd_Allele_Frequency_2010.tif")
sickle_raster <- terra::rast("data/blood_disorders/201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency_2010.tif")

# Check if raster extents overlap
extents_overlap <- function(x, extent){
  extent_x <- terra::ext(x)
  overlap <- TRUE
  if(is.null(terra::intersect(extent_x, extent))){
    overlap <- FALSE
  }
  return(overlap)
}

split <- function(raster, extent, iso, name){
  if(extents_overlap(raster, extent)){
    address <- paste0("map/", iso, "/", name, ".tif")
    cropped <- terra::crop(raster, extent)
    orderly2::orderly_artefact(
      description = paste("MAP", name, "raster"),
      files = address
    )
    terra::writeRaster(cropped, address)
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
  split(pvpr_raster, extent, iso, "pvpr")
  split(pfpr_raster, extent, iso, "pfpr")
  split(cities_raster, extent, iso, "cities")
  split(motor_raster, extent, iso, "motor")
  split(walk_raster, extent, iso, "walk")
  split(hbc_raster, extent, iso, "hbc")
  split(duffy_raster, extent, iso, "duffy")
  split(g6pd_raster, extent, iso, "g6pd")
  split(sickle_raster, extent, iso, "sickle")
}
 
