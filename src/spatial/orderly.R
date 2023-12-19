# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Process spatial data",
  long = "Extracts pixel-level information from all available rasters"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "DOM"
)

orderly2::orderly_resource(
  files = "spatial_utils.R"
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = "un_wup.rds"
)

orderly2::orderly_artefact(
  description = "Spatial data",
  files = "spatial.rds"
)

orderly2::orderly_artefact(
  description = "Spatial polygons",
  files = "shape.rds"
)
# ------------------------------------------------------------------------------

# Fixed inputs -----------------------------------------------------------------
external_data_address <- "C:/Users/pwinskil/OneDrive - Imperial College London/malaria_sites_data/2023/"
years <- 2000:as.integer(format(Sys.Date(), "%Y"))
source("spatial_utils.R")
# ------------------------------------------------------------------------------

# Shape file -------------------------------------------------------------------

# Load shape file at lowest available admin level
admin_levels <- 3:1
for(level in admin_levels){
  shape_address <- paste0(
    "C:/Users/pwinskil/OneDrive - Imperial College London/GADM/version_4.1.0/iso3c/",
    iso3c,
    "/",
    iso3c,
    "_",
    level,
    ".RDS"
  )
  shape <- readRDS(shape_address)
  if(nrow(shape) > 0){
    break
  }
}


lookup <- c(
  uid = "uid",
  iso3c = "GID_0",
  country = "COUNTRY",
  name_1 = "NAME_1",
  name_2 = "NAME_2",
  name_3 = "NAME_3",
  geom = "geom"
)

shape <- shape |>
  dplyr::mutate(
    uid = 1:dplyr::n()
  ) |>
  dplyr::rename(
    any_of(lookup)
  ) |>
  dplyr::select(dplyr::any_of(names(lookup)))

shape_df <- shape |>
  sf::st_drop_geometry(shape)
# ------------------------------------------------------------------------------

# Prevalence -------------------------------------------------------------------
pfpr_data <- paste0(external_data_address, "202206_Global_Pf_Parasite_Rate_2000/")
pfpr_years <- 2000:2020
pfpr_files <- paste0(pfpr_data, "202206_Global_Pf_Parasite_Rate_", pfpr_years, ".tif")
pfpr_raster <- terra::rast(pfpr_files) |>
  terra::crop(shape) |>
  pad_raster(pfpr_years, years)
names(pfpr_raster) <- paste0("pfpr_", years)

pvpr_data <- paste0(external_data_address, "202206_Global_Pv_Parasite_Rate_2000/")
pvpr_years <- 2000:2020
pvpr_files <- paste0(pvpr_data, "202206_Global_Pv_Parasite_Rate_", pvpr_years, ".tif")
pvpr_raster <- terra::rast(pvpr_files) |>
  terra::crop(shape) |>
  pad_raster(pfpr_years, years)
terra::values(pvpr_raster)[terra::values(pvpr_raster) == -1] <- NA
names(pvpr_raster) <- paste0("pvpr_", years)

pfpr_limits <- create_limits(pfpr_raster[[1]])
names(pfpr_limits) <- "pfpr_limits"
pvpr_limits <- create_limits(pvpr_raster[[1]])
names(pvpr_limits) <- "pvpr_limits"
pfpr_or_pvpr_limits <- pfpr_limits | pvpr_limits
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
population_data <- paste0(external_data_address, "WorldPop/")
population_years <- 2000:2020
population_files <- paste0(population_data, "population_", iso3c, "_", population_years, ".tif")
population_raster <- terra::rast(population_files) |>
  terra::crop(shape) |>
  terra::resample(pfpr_raster, method = "sum") |>
  pad_raster(population_years, years)
names(population_raster) <- paste0("population_", years)

urban_population <- readRDS("un_wup.rds") |>
  dplyr::filter(
    iso3c == {{iso3c}},
    year %in% years
  ) |>
  dplyr::arrange(year) |>
  dplyr::pull(proportion_urban) |>
  as.list()

urban_rural_raster <- purrr::map2(
  .x = terra::as.list(population_raster),
  .y = urban_population,
  .f = create_urban_rural_raster
) |>
  terra::rast()
names(urban_rural_raster) <- paste0("urban_rural_", years)

pop_at_risk_raster <- purrr::map2(
  .x = terra::as.list(population_raster),
  .y = pfpr_limits,
  .f = `*`
) |>
  terra::rast()
names(pop_at_risk_raster) <- paste0("pop_at_risk_", years)

pop_at_risk_pf_raster <- purrr::map2(
  .x = terra::as.list(population_raster),
  .y = pfpr_or_pvpr_limits,
  .f = `*`
) |>
  terra::rast()
names(pop_at_risk_pf_raster) <- paste0("pop_at_risk_pf_", years)

pop_at_risk_pv_raster <- purrr::map2(
  .x = terra::as.list(population_raster),
  .y = pvpr_limits,
  .f = `*`
) |>
  terra::rast()
names(pop_at_risk_pv_raster) <- paste0("pop_at_risk_pv_", years)
# ------------------------------------------------------------------------------

# ITNs -------------------------------------------------------------------------
itn_data <- paste0(external_data_address, "202106_Africa_Insecticide_Treated_Net_Use_2000/")
itn_years <- 2000:2020
itn_files <- paste0(itn_data, "202106_Africa_Insecticide_Treated_Net_Use_", itn_years, ".tiff")
itn_raster <- terra::rast(itn_files) |>
  pad_raster(itn_years, years)
approximate_itn <- FALSE

if(extents_overlap(itn_raster, shape)){
  itn_raster <- itn_raster |>
    terra::crop(shape)
  names(itn_raster) <- paste0("itn_use_", years)
} else {
  itn_raster <- NA
  approximate_itn <- TRUE
}
# ------------------------------------------------------------------------------

# IRS -------------------------------------------------------------------------
irs_data <- paste0(external_data_address, "202106_Africa_Indoor_Residual_Spraying_Coverage_2000/")
irs_years <- 2000:2020
irs_files <- paste0(irs_data, "/202106_Africa_Indoor_Residual_Spraying_Coverage_", irs_years, ".tif")
irs_raster <- terra::rast(irs_files) |>
  pad_raster(irs_years, years)
approximate_irs <- FALSE

if(extents_overlap(irs_raster, shape)){
  irs_raster <- irs_raster |>
    terra::crop(shape)
  names(irs_raster) <- paste0("irs_cov_", years)
} else{
  irs_raster <- NA
  approximate_irs <- TRUE
}
# ------------------------------------------------------------------------------

# Tx ---------------------------------------------------------------------------
tx_data <- paste0(external_data_address, "202106_Global_Antimalarial_Effective_Treatment_2000/")
tx_years <- 2000:2020
tx_files <- paste0(tx_data, "/202106_Global_Antimalarial_Effective_Treatment_", tx_years, ".tif")
tx_raster <- terra::rast(tx_files) |>
  pad_raster(tx_years, years)


if(extents_overlap(tx_raster, shape)){
  tx_raster <- tx_raster |>
    terra::crop(shape)
  names(tx_raster) <- paste0("tx_cov_", years)
} else{
  tx_raster <- NA
}
# ------------------------------------------------------------------------------

# SMC --------------------------------------------------------------------------
smc_data <- paste0(external_data_address, "SMC rasters-20231122T161433Z-001/")
smc_years <- 2012:2020
smc_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

smc_raster <- list()
for(m in seq_along(smc_months)){
  smc_files <- paste0(smc_data, "SMC_", smc_years, ".", smc_months[m], ".tif")
  smc_raster_month <- terra::rast(smc_files)
  if(extents_overlap(smc_raster_month, shape)){
    smc_raster[[m]] <- smc_raster_month |>
      terra::crop(shape) |>
      pad_raster(smc_years, years)
    names(smc_raster[[m]]) <- paste0("smc_cov_", years, "_", m)
  } else {
    smc_raster[[m]] <- NA
  }
}

# ------------------------------------------------------------------------------

# Vectors ----------------------------------------------------------------------
vector_data <- paste0(external_data_address, "Sinka_relative_abundance/")
vectors <- c("gambiae", "arabiensis", "funestus")

vector_raster <- list()
for(v in seq_along(vectors)){
  vector_files <- paste0(vector_data, "relative_", vectors[v], ".tif")
  vector_raster[[vectors[v]]] <- terra::rast(vector_files) 
  
  if(extents_overlap(vector_raster[[v]], shape)){
    vector_raster[[vectors[v]]] <- vector_raster[[vectors[v]]] |>
      terra::crop(shape)  |>
      terra::resample(pfpr_raster) |>
      pad_raster(2016, years, forward_empty = TRUE)
    names(vector_raster[[vectors[v]]]) <- paste0(vectors[v], "_", years)
  } else {
    vector_raster[[vectors[v]]] <- NA
  }
}
# ------------------------------------------------------------------------------

# Rainfall ---------------------------------------------------------------------
rainfall_data <- paste0(external_data_address, "chirps_monthly/")
rainfall_years <- 2000:2022
rainfall_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

rainfall_raster <- list()
for(m in seq_along(rainfall_months)){
  rainfall_files <- paste0(rainfall_data, rainfall_years, "_", rainfall_months[m], ".tif")
  rainfall_raster[[m]] <- terra:::rast(rainfall_files) |>
    terra::crop(shape) 
  terra::values(rainfall_raster[[m]])[terra::values(rainfall_raster[[m]]) == -9999] <- NA
  rainfall_raster[[m]] <- rainfall_raster[[m]]|>
    terra::resample(pfpr_raster) |>
    pad_raster(rainfall_years, years)
  names(rainfall_raster[[m]]) <- paste0("rainfall_", years, "_", m)
}
# ------------------------------------------------------------------------------

# Blood disorders --------------------------------------------------------------
sicklecell_data <- paste0(external_data_address, "201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency_2010/")
g6pd_data <- paste0(external_data_address, "201201_Global_G6PDd_Allele_Frequency_2010/")
hpc_data <- paste0(external_data_address, "201201_Africa_HbC_Allele_Frequency_2010/")
duffy_data <- paste0(external_data_address, "201201_Global_Duffy_Negativity_Phenotype_Frequency_2010/")
blood_disorder_years <- 2010

sicklecell_files <- list.files(sicklecell_data, pattern = "*.tif", full.names = TRUE)
sicklecell_raster <- terra::rast(sicklecell_files)
if(extents_overlap(sicklecell_raster, shape)){
  sicklecell_raster <- sicklecell_raster |>
    terra::crop(shape) |>
    pad_raster(blood_disorder_years, years, forward_empty = TRUE)
} else {
  sicklecell_raster <- NA
}

g6pd_files <- list.files(g6pd_data, pattern = "*.tif", full.names = TRUE)
g6pd_raster <- terra::rast(g6pd_files)
if(extents_overlap(g6pd_raster, shape)){
  g6pd_raster <- g6pd_raster |>
    terra::crop(shape) |>
    pad_raster(blood_disorder_years, years, forward_empty = TRUE)
} else {
  g6pd_raster <- NA
}

hpc_files <- list.files(hpc_data, pattern = "*.tif", full.names = TRUE)
hpc_raster <- terra::rast(hpc_files)
if(extents_overlap(hpc_raster, shape)){
  hpc_raster <- hpc_raster |>
    terra::crop(shape) |>
    pad_raster(blood_disorder_years, years, forward_empty = TRUE)
} else {
  hpc_raster <- NA
}

duffy_files <- list.files(duffy_data, pattern = "*.tif", full.names = TRUE)
duffy_raster <- terra::rast(duffy_files)
if(extents_overlap(duffy_raster, shape)){
  duffy_raster <- duffy_raster |>
    terra::crop(shape) |>
    pad_raster(blood_disorder_years, years, forward_empty = TRUE)
} else {
  duffy_raster <- NA
}
# ------------------------------------------------------------------------------

# Travel times -----------------------------------------------------------------
motor_travel_healthcare_data <- paste0(external_data_address , "202001_Global_Motorized_Travel_Time_to_Healthcare_2019/")
walking_travel_healthcare_data <- paste0(external_data_address , "202001_Global_Walking_Only_Travel_Time_To_Healthcare_2019/")
city_travel_time_data <- paste0(external_data_address , "201501_Global_Travel_Time_to_Cities_2015/")

motor_travel_healthcare_files <- list.files(motor_travel_healthcare_data, pattern = "*.tif", full.names = TRUE)
motor_travel_healthcare_raster <- terra::rast(motor_travel_healthcare_files)
if(extents_overlap(motor_travel_healthcare_raster, shape)){
  motor_travel_healthcare_raster <- motor_travel_healthcare_raster |>
    terra::crop(shape) |>
    terra::resample(pfpr_raster) |>  
    pad_raster(2019, years, forward_empty = TRUE)
} else {
  motor_travel_healthcare_raster <- NA
}

walking_travel_healthcare_files <- list.files(walking_travel_healthcare_data, pattern = "*.tif", full.names = TRUE)
walking_travel_healthcare_raster <- terra::rast(walking_travel_healthcare_files)
if(extents_overlap(walking_travel_healthcare_raster, shape)){
  walking_travel_healthcare_raster <- walking_travel_healthcare_raster |>
    terra::crop(shape) |>
    terra::resample(pfpr_raster) |>  
    pad_raster(2019, years, forward_empty = TRUE)
} else {
  walking_travel_healthcare_raster <- NA
}

city_travel_time_files <- list.files(city_travel_time_data, pattern = "*.tif", full.names = TRUE)
city_travel_time_raster <- terra::rast(city_travel_time_files)
if(extents_overlap(city_travel_time_raster, shape)){
  city_travel_time_raster <- city_travel_time_raster |>
    terra::crop(shape) |>
    terra::resample(pfpr_raster) |>  
    pad_raster(2015, years, forward_empty = TRUE)
} else {
  city_travel_time_raster <- NA
}
# ------------------------------------------------------------------------------

# Extract values ---------------------------------------------------------------

# Extract unique ID for each pixel to link to shape file
shape_raster <- terra::rasterize(shape, pfpr_raster, field = "uid")
uid <- terra::values(shape_raster)

df <- data.frame(
  pixel = 1:length(uid),
  uid = uid,
  urban_rural = raster_values(urban_rural_raster),
  year = rep(years, each = nrow(uid)),
  pop = raster_values(population_raster, na_replace = 0),
  par = raster_values(pop_at_risk_raster, na_replace = 0),
  par_pf = raster_values(pop_at_risk_pf_raster, na_replace = 0),
  par_pv = raster_values(pop_at_risk_pv_raster, na_replace = 0),
  pfpr = raster_values(pfpr_raster),
  pvpr = raster_values(pvpr_raster),
  itn_use = raster_values(itn_raster),
  irs_cov = raster_values(irs_raster),
  tx_cov = raster_values(tx_raster),
  smc_1 = raster_values(smc_raster[[1]], na_replace = 0),
  smc_2 = raster_values(smc_raster[[2]], na_replace = 0),
  smc_3 = raster_values(smc_raster[[3]], na_replace = 0),
  smc_4 = raster_values(smc_raster[[4]], na_replace = 0),
  smc_5 = raster_values(smc_raster[[5]], na_replace = 0),
  smc_6 = raster_values(smc_raster[[6]], na_replace = 0),
  smc_7 = raster_values(smc_raster[[7]], na_replace = 0),
  smc_8 = raster_values(smc_raster[[8]], na_replace = 0),
  smc_9 = raster_values(smc_raster[[9]], na_replace = 0),
  smc_10 = raster_values(smc_raster[[10]], na_replace = 0),
  smc_11 = raster_values(smc_raster[[11]], na_replace = 0),
  smc_12 = raster_values(smc_raster[[12]], na_replace = 0),
  rainfall_1 = raster_values(rainfall_raster[[1]]),
  rainfall_2 = raster_values(rainfall_raster[[2]]),
  rainfall_3 = raster_values(rainfall_raster[[3]]),
  rainfall_4 = raster_values(rainfall_raster[[4]]),
  rainfall_5 = raster_values(rainfall_raster[[5]]),
  rainfall_6 = raster_values(rainfall_raster[[6]]),
  rainfall_7 = raster_values(rainfall_raster[[7]]),
  rainfall_8 = raster_values(rainfall_raster[[8]]),
  rainfall_9 = raster_values(rainfall_raster[[9]]),
  rainfall_10 = raster_values(rainfall_raster[[10]]),
  rainfall_11 = raster_values(rainfall_raster[[11]]),
  rainfall_12 = raster_values(rainfall_raster[[12]]),
  gambiae = raster_values(vector_raster$gambiae),
  arabiensis = raster_values(vector_raster$arabiensis),
  funestus = raster_values(vector_raster$funestus),
  sicklecell = raster_values(sicklecell_raster),
  gdp6 = raster_values(g6pd_raster),
  hpc = raster_values(hpc_raster),
  duffy_negativity = raster_values(duffy_raster),
  motor_travel_time_healthcare = raster_values(motor_travel_healthcare_raster),
  walking_travel_time_healthcare = raster_values(walking_travel_healthcare_raster),
  city_travel_time = raster_values(city_travel_time_raster)
) |>
  # Remove pixels that don't fall within a polygon
  dplyr::filter(!is.na(uid)) |> 
  # Categorise urban_rural
  dplyr::mutate(urban_rural = ifelse(urban_rural == 1, "urban", "rural")) |>
  # Normalise vectors
  dplyr::mutate(
    vector_sum = gambiae + arabiensis + funestus,
    gambiae = gambiae / vector_sum,
    arabiensis = arabiensis / vector_sum,
    funestus = funestus / vector_sum
  ) |>
  dplyr::select(-vector_sum) |>
  # Link to shape data
  dplyr::left_join(shape_df, by = "uid") |>
  # Order columns
  dplyr::select(dplyr::any_of(names(lookup)), dplyr::everything())

format(object.size(df), "Mb")

# ------------------------------------------------------------------------------

# Fill nets and IRS outside SSA ------------------------------------------------

# TODO: How to handle year extrapolation?
if(approximate_itn | approximate_irs){
  par <- df |>
    dplyr::summarise(
      par = sum(par),
      .by = "year"
    )
  
  rank <- df |>
    dplyr::filter(year == 2000) |>
    dplyr::mutate(pr = 1 - (1 - pfpr) * (1 - pvpr)) |>
    dplyr::arrange(-pr) |>
    dplyr::mutate(rank = 1:dplyr::n()) |>
    dplyr::select(pixel, rank)
}

if(approximate_itn){
  
  # Assume median half life and usage rate
  hl <- netz::get_halflife_data() |>
    dplyr::pull(half_life) |>
    median()
  ur <- netz::get_usage_rate_data() |>
    dplyr::pull(usage_rate) |>
    median()
  
  # Estimate the total people using nets each year | WHO net delivery/distribution
  nets_distributed <- read.csv(paste0(external_data_address, "itn_delivered.csv")) |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::left_join(par, by = "year") |>
    dplyr::mutate(
      crop = netz::distribution_to_crop_dynamic(itn_interpolated, netz::net_loss_map, half_life = hl) / par,
      # TODO: usin a hybrid here, forcing linear at acess < 0.5, might be worth adding to netz package?
      access = crop_to_access2(crop),
      usage = netz::access_to_usage(access, ur),
      people_using_nets = usage * par
    ) |>
    dplyr::select(year, people_using_nets)
  
  # Target nets at maximum of 50% use (to be in linear section of net model),
  # Targeting is based on 2000 prevalence
  max_use <- 0.5
  df <- df |>
    dplyr::left_join(
      rank,
      by = "pixel",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      nets_distributed,
      by = "year"
    ) |>
    dplyr::group_by(year) |>
    dplyr::arrange(rank) |>
    dplyr::mutate(
      pop_cov = cumsum(par * max_use),
      itn_use = ifelse(pop_cov <= people_using_nets, max_use, 0)
    ) |>
    dplyr::ungroup() |>
    tidyr::replace_na(
      list(itn_use = 0)
    ) |>
    dplyr::select(-c("rank", "pop_cov", "people_using_nets"))
}

if(approximate_irs){
  irs_people <- read.csv(paste0(external_data_address, "irs_people_protected.csv")) |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::select(year, irs_interpolated)
  
  # Target irs at maximum of 80% coverage,
  # Targeting is based on 2000 prevalence
  max_cov <- 0.8
  df <- df |>
    dplyr::left_join(
      rank,
      by = "pixel",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      irs_people,
      by = "year"
    ) |>
    dplyr::group_by(year) |>
    dplyr::arrange(rank) |>
    dplyr::mutate(
      pop_cov = cumsum(par * max_cov),
      irs_cov = ifelse(pop_cov <= irs_interpolated, max_cov, 0)
    ) |>
    dplyr::ungroup() |>
    tidyr::replace_na(
      list(irs_cov = 0)
    ) |>
    dplyr::select(-c("rank", "pop_cov", "irs_interpolated"))
  
}
# ------------------------------------------------------------------------------

# Save outputs -----------------------------------------------------------------
saveRDS(df, "spatial.rds")
saveRDS(shape, "shape.rds")
# ------------------------------------------------------------------------------
