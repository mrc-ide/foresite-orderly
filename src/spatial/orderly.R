# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Spatial",
  long = "Define the spatial units, specify urban/rural populations and 
  extract and aggregate spatial data"
)

orderly2::orderly_parameters(
  iso3c = NULL,
  admin_level = NULL
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = c(
    "un_wup.rds"
  )
)

orderly2::orderly_artefact(
  description = "Spatial simple features boundary", 
  files = "gadm.rds"
)

orderly2::orderly_artefact(
  description = "Spatial sites", 
  files = "sites.rds"
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
    ".rds"
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
    "country", 
    "iso3c",
    "name_1",
    dplyr::any_of("name_2"),
    "id"
  )

saveRDS(gadm, "gadm.rds")
gadm_spatvector <- methods::as(gadm, "SpatVector")
gadm_df <- sf::st_drop_geometry(gadm)
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

spatial_data <- population_raster_stack |>
  terra::extract(
    y = gadm_spatvector
  ) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  dplyr::rename(id = ID) |>
  tidyr::pivot_longer(
    cols = -c("id", "pixel"),
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
# ------------------------------------------------------------------------------

# Assign pixels as urban or rural ----------------------------------------------

## Pixels are assigned to "urban" starting with the most densely populated first
## and continuing until the UN WUP urban proportion is met. After this point
## remaining pixels are assigned as "rural"
urban_prop <- readRDS("un_wup.rds") |>
  dplyr::filter(iso3c == {{iso3c}}) |>
  dplyr::select(-iso3c)

spatial_data <- spatial_data |>
  dplyr::arrange(
    year, -population
  ) |>
  dplyr::mutate(
    cumulative_proportion = cumsum(population) / sum(population),
    .by = "year"
  ) |>
  dplyr::left_join(urban_prop, by = "year") |>
  dplyr::mutate(urban_rural = ifelse(
    cumulative_proportion <= proportion_urban, "urban", "rural"
  )) |>
  dplyr::arrange(
    id, pixel, year
  )
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

pfpr_pixel_values <- pfpr_raster_stack |>
  terra::extract(
    y = gadm_spatvector
  ) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  dplyr::rename(id = ID) |>
  tidyr::pivot_longer(
    cols = -c("id", "pixel"),
    names_to = "year",
    values_to = "pfpr",
    names_transform = list(
      year = as.integer
    )
  )

spatial_data <- spatial_data |>
  dplyr::left_join(
    y = pfpr_pixel_values, 
    by = c("id", "pixel", "year")
  )

rm(pfpr_pixel_values)
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

pvpr_pixel_values <- pvpr_raster_stack |>
  terra::extract(
    y = gadm_spatvector
  ) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  dplyr::rename(id = ID) |>
  tidyr::pivot_longer(
    cols = -c("id", "pixel"),
    names_to = "year",
    values_to = "pvpr",
    names_transform = list(
      year = as.integer
    )
  ) |>
  dplyr::mutate(pvpr = ifelse(pvpr < 0, NA, pvpr))

spatial_data <- spatial_data |>
  dplyr::left_join(
    y = pvpr_pixel_values, 
    by = c("id", "pixel", "year")
  )

rm(pvpr_pixel_values)
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

treatment_pixel_values <- treatment_raster_stack |>
  terra::extract(
    y = gadm_spatvector
  ) |>
  dplyr::mutate(
    pixel = 1:dplyr::n()
  ) |>
  dplyr::rename(id = ID) |>
  tidyr::pivot_longer(
    cols = -c("id", "pixel"),
    names_to = "year",
    values_to = "treatment",
    names_transform = list(
      year = as.integer
    )
  )

spatial_data <- spatial_data |>
  dplyr::left_join(
    y = treatment_pixel_values, 
    by = c("id", "pixel", "year")
  )

rm(treatment_pixel_values)
# ------------------------------------------------------------------------------

# Bed net usage in Africa ------------------------------------------------------
if(gadm$continent[1] == "Africa"){
  
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
  
  bednet_pixel_values <- bednet_raster_stack |>
    terra::extract(
      y = gadm_spatvector
    ) |>
    dplyr::mutate(
      pixel = 1:dplyr::n()
    ) |>
    dplyr::rename(id = ID) |>
    tidyr::pivot_longer(
      cols = -c("id", "pixel"),
      names_to = "year",
      values_to = "net_use",
      names_transform = list(
        year = as.integer
      )
    )
  
  spatial_data <- spatial_data |>
    dplyr::left_join(
      y = bednet_pixel_values, 
      by = c("id", "pixel", "year")
    )
  
  rm(bednet_pixel_values)
} else {
  spatial_data$net_use <- NA
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
  
  irs_pixel_values <- irs_raster_stack |>
    terra::extract(
      y = gadm_spatvector
    ) |>
    dplyr::mutate(
      pixel = 1:dplyr::n()
    ) |>
    dplyr::rename(id = ID) |>
    tidyr::pivot_longer(
      cols = -c("id", "pixel"),
      names_to = "year",
      values_to = "irs_cov",
      names_transform = list(
        year = as.integer
      )
    )
  
  spatial_data <- spatial_data |>
    dplyr::left_join(
      y = irs_pixel_values, 
      by = c("id", "pixel", "year")
    )
  
  rm(irs_pixel_values)
  
} else {
  spatial_data$irs_cov = NA
}
# ------------------------------------------------------------------------------

# TODO: Note! We have not rescaled or projected population yet

# Spatial limits and population at risk ----------------------------------------
limits <- spatial_data |>
  dplyr::filter(
    year == 2000
  ) |>
  dplyr::mutate(
    pf_limits = ifelse(is.na(pfpr) | pfpr <= 0, 0, 1),
    pv_limits = ifelse(is.na(pvpr) | pvpr <= 0, 0, 1),
    limits = ifelse(pf_limits == 1 | pv_limits == 1, 1, 0)
  ) |>
  dplyr::select(
    id, pixel, pf_limits, pv_limits, limits
  )

spatial_data <- spatial_data |>
  dplyr::left_join(limits, by = c("id", "pixel")) |>
  dplyr::mutate(
    par = limits * population,
    par_pf = pf_limits * population,
    par_pv = pf_limits * population
  ) |>
  dplyr::select(-c("pf_limits", "pv_limits", "limits"))

rm(limits)
# ------------------------------------------------------------------------------

# Aggregate spatial variables --------------------------------------------------
weighted.mean2 <- function(x, w, na.rm = TRUE){
  if(sum(w, na.rm = TRUE) == 0){
    out <- 0
  } else {
    out <- weighted.mean(x, w, na.rm = na.rm)
  }
  return(out)
}

aggregated_spatial_data <-  spatial_data |>
  dplyr::summarise(
    pfpr = weighted.mean2(pfpr, par_pf),
    pvpr = weighted.mean2(pvpr, par_pv),
    net_use = weighted.mean2(net_use, par),
    irs_cov = weighted.mean2(irs_cov, par),
    tx_cov = weighted.mean2(treatment, par),
    population = sum(population),
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = c("id", "urban_rural", "year")
  ) |>
  dplyr::left_join(gadm_df, by = "id") |>
  dplyr::select(-"id") |>
  dplyr::select(
    "continent", "country", "iso3c",
    dplyr::any_of(c("name_1", "name_2")),
    "urban_rural",
    "year",
    "population", "par_pf", "par_pv",
    "pfpr", "pvpr",
    "tx_cov", "net_use", "irs_cov"
  )
# ------------------------------------------------------------------------------

# Vectors ----------------------------------------------------------------------
if(gadm_df$continent[1] == "Africa"){
  vector_rasters <- list.files(
    path = paste0(
      external_data_address,
      "malaria_sites_data/raster"
    ),
    pattern = "relative",
    full.names = TRUE
  )
  
  vector_raster_names <- vector_rasters |>
    stringr::str_replace(paste0(
      external_data_address,
      "malaria_sites_data/raster/relative_"
    ), "") |>
    stringr::str_replace(".tif", "")
  
  vector_raster <- list()
  for(i in 1:length(vector_rasters)){
    vector_raster[[i]] <- 
      terra::rast(x = vector_rasters[i]) |>
      terra::crop(y = gadm_spatvector) |>
      terra::resample(population_raster_stack)
  }
  vector_raster_stack <- terra::rast(vector_raster)
  names(vector_raster_stack) <- vector_raster_names
  
  vector_pixel_values <- vector_raster_stack |>
    terra::extract(
      y = gadm_spatvector
    ) |>
    dplyr::mutate(
      pixel = 1:dplyr::n()
    ) |>
    dplyr::rename(id = ID) |>
    tidyr::pivot_longer(cols = -c(id, pixel), names_to = "species", values_to = "prop") |>
    dplyr::filter(!is.na(prop)) |>
    dplyr::summarise(
      prop = mean(prop, na.rm = TRUE),
      .by = c("id", "species")
    ) |>
    dplyr::mutate(
      prop = prop / sum(prop),
      .by = c("id")
    ) |>
    dplyr::left_join(gadm_df, by = "id") |>
    dplyr::select(-"id") |>
    dplyr::select(
      "continent", "country", "iso3c",
      dplyr::any_of(c("name_1", "name_2")),
      "species", "prop"
    )
  
} else {
  
}
# ------------------------------------------------------------------------------

# Rainfall ---------------------------------------------------------------------
rainfall_rasters <- list.files(
  path = paste0(
    external_data_address,
    "malaria_sites_data/raster"
  ),
  pattern = "rainfall",
  full.names = TRUE
)

rainfall_raster_stack <- terra::rast(x = rainfall_rasters) |>
  terra::crop(y = gadm_spatvector)

mean2 <- function(x, na.rm = TRUE){
  return(mean(x[x>=0], na.rm = na.rm))
}

rainfall_values <- rainfall_raster_stack |>
  terra::extract(
    y = gadm_spatvector,
    fun = mean2
  ) |>
  dplyr::rename(id = ID) |>
  tidyr::pivot_longer(
    cols = -id,
    names_to = "date",
    values_to = "rainfall",
    names_prefix = "rainfall_"
  ) |>
  dplyr::mutate(
    date = stringr::str_replace_all(
      date, "_", "-"
    ),
    day = lubridate::yday(date)
  ) |>
  dplyr::filter(day < 366) |>
  dplyr::left_join(gadm_df, by = "id") |>
  dplyr::select(-"id") |>
  dplyr::select(
    "continent", "country", "iso3c",
    dplyr::any_of(c("name_1", "name_2")),
    "rainfall"
  )
# ------------------------------------------------------------------------------


