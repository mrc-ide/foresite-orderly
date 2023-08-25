# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Process data",
  long = "Processes raw input data by country in preparation for creation of
  site-file elements"
)

orderly2::orderly_parameters(
  iso3c = NULL,
  admin_level = NULL
)

orderly2::orderly_artefact(
  description = "Spatial simple features boundary", 
  files = "gadm.rds"
)

orderly2::orderly_artefact(
  description = "Spatial metadata data frame", 
  files = "gadm_df.rds"
)

orderly2::orderly_artefact(
  description = "Population rasters", 
  files = "population_raster_stack.rds"
)

orderly2::orderly_artefact(
  description = "Population raster pixel values", 
  files = "population_pixel_values.RDS"
)

orderly2::orderly_artefact(
  description = "PfPr rasters", 
  files = "pfpr_raster_stack.rds"
)

orderly2::orderly_artefact(
  description = "PfPr raster pixel values", 
  files = "pfpr_pixel_values.RDS"
)

orderly2::orderly_artefact(
  description = "PvPr rasters", 
  files = "pvpr_raster_stack.rds"
)

orderly2::orderly_artefact(
  description = "PvPr raster pixel values", 
  files = "pvpr_pixel_values.RDS"
)

orderly2::orderly_artefact(
  description = "Effective treatment coverage rasters", 
  files = "treatment_raster_stack.rds"
)

orderly2::orderly_artefact(
  description = "Effective treatment coverage raster pixel values", 
  files = "treatment_pixel_values.RDS"
)

orderly2::orderly_artefact(
  description = "Bednet use rasters", 
  files = "bednet_raster_stack.rds"
)

orderly2::orderly_artefact(
  description = "Bednet use raster pixel values", 
  files = "bednet_pixel_values.RDS"
)

orderly2::orderly_artefact(
  description = "IRS coverage rasters", 
  files = "irs_raster_stack.rds"
)

orderly2::orderly_artefact(
  description = "IRS coverage raster pixel values", 
  files = "irs_pixel_values.RDS"
)

orderly2::orderly_artefact(
  description = "Demography data: UNWPP mortality rates and age-structure", 
  files = "demography_data.rds"
)

orderly2::orderly_artefact(
  description = "Neonatal mortality data: UNICEF neonatal mortality rates", 
  files = "neonatal_mortality_data.rds"
)
# ------------------------------------------------------------------------------

external_data_address <- "C:/Users/pwinskil/OneDrive - Imperial College London/"

# Spatial boundaries -----------------------------------------------------------
library(sf)
library(terra)

rename_vector <- c(
  iso3c = "ID_0",
  country = "COUNTRY",
  name_1 = "NAME_1",
  name_2 = "NAME_2"
)

gadm <- readRDS(
  file = paste0(
    external_data_address,
    "GADM/version_4.0.4/iso3c/",
    iso3c,
    "/",
    iso3c,
    "_",
    admin_level,
    ".RDS"
  )
) |>
  dplyr::rename(
    dplyr::any_of(rename_vector)
  ) |>
  dplyr::mutate(
    continent = countrycode::countrycode(
      sourcevar = iso3c,
      origin = "iso3c",
      destination = "continent"
    ),
    id = 1:dplyr::n()
  ) |>
  dplyr::select(
    "continent",
    "iso3c",
    "name_1",
    dplyr::any_of("name_2"),
    "id"
  )

gadm_spatvector <- methods::as(gadm, "SpatVector")
gadm_df <- sf::st_drop_geometry(x = gadm)

saveRDS(gadm, "gadm.rds")
saveRDS(gadm_df, "gadm_df.rds")
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
population_rasters <- list.files(
  path = paste0(
    external_data_address,
    "malaria_sites_data/raster"
  ),
  pattern = paste0("population_", iso3c),
  full.names = TRUE
)

population_raster_stack <- terra::rast(x = population_rasters) |>
  terra::crop(y = gadm_spatvector) 

names(population_raster_stack) <- as.numeric(gsub("\\D", "", population_rasters))

population_pixel_values <- terra::extract(
  x = population_raster_stack,
  y = gadm_spatvector
) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  tidyr::pivot_longer(
    cols = -c("ID", "pixel"),
    names_to = "year",
    values_to = "population",
    names_transform = list(
      year = as.integer
    )
  ) |>
  tidyr::replace_na(
    replace = list(
      population = 0
    )
  )

saveRDS(population_raster_stack, "population_raster_stack.RDS")
saveRDS(population_pixel_values, "population_pixel_values.RDS")
# ------------------------------------------------------------------------------

# Plasmodium falciparum parasite rate in 2-10 year olds (PfPr_2_10) ------------
pfpr_rasters <- list.files(
  path = paste0(
    external_data_address,
    "malaria_sites_data/2023/202206_Global_Pf_Parasite_Rate_2000/"
  ),
  pattern = "*.tif",
  full.names = TRUE
)

pfpr_raster_stack <- terra::rast(x = pfpr_rasters) |>
  terra::crop(y = gadm_spatvector) |>
  terra::resample(population_raster_stack)

names(pfpr_raster_stack) <- gsub(
  pattern = "202206_Global_Pf_Parasite_Rate_",
  replacement = "",
  names(pfpr_raster_stack)
)

pfpr_pixel_values <- terra::extract(
  x = pfpr_raster_stack,
  y = gadm_spatvector
) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  tidyr::pivot_longer(
    cols = -c("ID", "pixel"),
    names_to = "year",
    values_to = "pfpr",
    names_transform = list(
      year = as.integer
    )
  )

saveRDS(pfpr_raster_stack, "pfpr_raster_stack.RDS")
saveRDS(pfpr_pixel_values, "pfpr_pixel_values.RDS")
# ------------------------------------------------------------------------------

# Plasmodium vivax parasite rate in 0-100 year olds (PvPr_0_100) ---------------
pvpr_rasters <- list.files(
  path = paste0(
    external_data_address,
    "malaria_sites_data/2023/202206_Global_Pv_Parasite_Rate_2000/"
  ),
  pattern = "*.tif",
  full.names = TRUE
)

pvpr_raster_stack <- terra::rast(x = pvpr_rasters) |>
  terra::crop(y = gadm_spatvector) |>
  terra::resample(population_raster_stack)

names(pvpr_raster_stack) <- gsub(
  pattern = "202206_Global_Pv_Parasite_Rate_",
  replacement = "",
  names(pvpr_raster_stack)
)

pvpr_pixel_values <- terra::extract(
  x = pvpr_raster_stack,
  y = gadm_spatvector
) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  tidyr::pivot_longer(
    cols = -c("ID", "pixel"),
    names_to = "year",
    values_to = "pvpr",
    names_transform = list(
      year = as.integer
    )
  )

saveRDS(pvpr_raster_stack, "pvpr_raster_stack.RDS")
saveRDS(pvpr_pixel_values, "pvpr_pixel_values.RDS")
# ------------------------------------------------------------------------------

# Effective antimalarial treatment coverage ------------------------------------
treatment_rasters <- list.files(
  path = paste0(
    external_data_address,
    "malaria_sites_data/2023/202106_Global_Antimalarial_Effective_Treatment_2000/"
  ),
  pattern = "*.tif",
  full.names = TRUE
)

treatment_raster_stack <- terra::rast(x = treatment_rasters) |>
  terra::crop(y = gadm_spatvector) |>
  terra::resample(population_raster_stack)

names(treatment_raster_stack) <- gsub(
  pattern = "202106_Global_Antimalarial_Effective_Treatment_",
  replacement = "",
  names(treatment_raster_stack)
)

treatment_pixel_values <- terra::extract(
  x = treatment_raster_stack,
  y = gadm_spatvector
) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  tidyr::pivot_longer(
    cols = -c("ID", "pixel"),
    names_to = "year",
    values_to = "treatment",
    names_transform = list(
      year = as.integer
    )
  )

saveRDS(treatment_raster_stack, "treatment_raster_stack.RDS")
saveRDS(treatment_pixel_values, "treatment_pixel_values.RDS")
# ------------------------------------------------------------------------------

# Bed net usage in Africa ------------------------------------------------------
if(gadm_df$continent[1] == "Africa"){
  
  bednet_rasters <- list.files(
    path = paste0(
      external_data_address,
      "malaria_sites_data/2023/202106_Africa_Insecticide_Treated_Net_Use_2000/"
    ),
    pattern = "*.tif",
    full.names = TRUE
  )
  
  bednet_raster_stack <- terra::rast(x = bednet_rasters) |>
    terra::crop(y = gadm_spatvector) |>
    terra::resample(population_raster_stack)
  
  names(bednet_raster_stack) <- gsub(
    pattern = "202106_Africa_Insecticide_Treated_Net_Use_",
    replacement = "",
    names(bednet_raster_stack)
  )
  
  bednet_pixel_values <- terra::extract(
    x = bednet_raster_stack,
    y = gadm_spatvector
  ) |>
    dplyr::mutate(
      pixel = 1:dplyr::n()
    ) |>
    tidyr::pivot_longer(
      cols = -c("ID", "pixel"),
      names_to = "year",
      values_to = "bednet_use",
      names_transform = list(
        year = as.integer
      )
    )
  
  saveRDS(bednet_raster_stack, "bednet_raster_stack.RDS")
  saveRDS(bednet_pixel_values, "bednet_pixel_values.RDS")
} else {
  saveRDS(NA, "bednet_raster_stack.RDS")
  saveRDS(NA, "bednet_pixel_values.RDS")
}
# ------------------------------------------------------------------------------

# Indoor residual spraying coverage in Africa ----------------------------------
if(gadm_df$continent[1] == "Africa"){
  
  irs_rasters <- list.files(
    path = paste0(
      external_data_address,
      "malaria_sites_data/2023/202106_Africa_Indoor_Residual_Spraying_Coverage_2000/"
    ),
    pattern = "*.tif",
    full.names = TRUE
  )
  
  irs_raster_stack <- terra::rast(x = irs_rasters) |>
    terra::crop(y = gadm_spatvector) |>
    terra::resample(population_raster_stack)
  
  names(irs_raster_stack) <- gsub(
    pattern = "202106_Africa_Indoor_Residual_Spraying_Coverage_",
    replacement = "",
    names(irs_raster_stack)
  )
  
  irs_pixel_values <- terra::extract(
    x = irs_raster_stack,
    y = gadm_spatvector
  ) |>
    dplyr::mutate(
      pixel = 1:dplyr::n()
    ) |>
    tidyr::pivot_longer(
      cols = -c("ID", "pixel"),
      names_to = "year",
      values_to = "irs_use",
      names_transform = list(
        year = as.integer
      )
    )
  
  saveRDS(irs_raster_stack, "irs_raster_stack.RDS")
  saveRDS(irs_pixel_values, "irs_pixel_values.RDS")
} else {
  saveRDS(NA, "irs_raster_stack.RDS")
  saveRDS(NA, "irs_pixel_values.RDS")
}
# ------------------------------------------------------------------------------

# Demography -------------------------------------------------------------------
demography_full <- readRDS(
  file = paste0(
    external_data_address,
    "demography/data/demography.RDS"
  )
)
neonatal_mortality_full <- readRDS(
  file = paste0(
    external_data_address,
    "demography/data/neonatal_mortality.RDS"
  )
)

demography <- demography_full |>
  dplyr::filter(iso3c == {{iso3c}}) |>
  dplyr::select("iso3c", "year", "age_lower", "age_upper", "qx", "p")

neonatal_mortality <- neonatal_mortality_full |>
  dplyr::filter(iso3c == {{iso3c}})

saveRDS(demography, "demography_data.rds")
saveRDS(neonatal_mortality, "neonatal_mortality_data.rds")
# ------------------------------------------------------------------------------
