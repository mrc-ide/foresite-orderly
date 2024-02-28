# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Process spatial data",
  long = "Extracts pixel-level information from all available rasters"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "BFA",
  boundary_version = "GADM_4.1.0"
)

orderly2::orderly_resource(
  files = "spatial_utils.R"
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = "un_wup.rds"
)

orderly2::orderly_dependency(
  name = "data_map",
  query = "latest()",
  files = c(
    "data/access/",
    "data/blood_disorders/",
    "data/irs/",
    "data/itn/",
    "data/pfpr/",
    "data/pvpr/",
    "data/smc/",
    "data/tx/"
  )
)

orderly2::orderly_dependency(
  name = "data_worldpop",
  query = "latest()",
  files = c("data/population" = paste0("data/population/", iso3c, "/"))
)

orderly2::orderly_dependency(
  name = "data_chirps",
  query = "latest()",
  files = "data/rainfall/"
)

orderly2::orderly_dependency(
  name = "data_dhs",
  query = "latest()",
  files = "data/dhs/"
)

orderly2::orderly_dependency(
  name = "data_who",
  query = "latest()",
  files = "data/who/"
)

orderly2::orderly_dependency(
  name = "data_vectors",
  query = "latest()",
  files = c(
    "data/vector_bionomics/",
    "data/vector_occurrence/",
    "data/vector_relative_abundance/"
  )
)

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:boundary_version == this:boundary_version)",
  files = c("data/boundaries" = paste0("data/", boundary_version, "/", iso3c, "/"))
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
years <- 2000:as.integer(format(Sys.Date(), "%Y"))
source("spatial_utils.R")
# ------------------------------------------------------------------------------

# Shape file -------------------------------------------------------------------

# Load shape file at lowest available admin level
admin_levels <- 3:1
for(level in admin_levels){
  shape_address <- paste0(
    "data/boundaries/",
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
pfpr_years <- 2000:2020
pfpr_files <- paste0("data/pfpr/202206_Global_Pf_Parasite_Rate_", pfpr_years, ".tif")
pfpr_raster <- terra::rast(pfpr_files) |>
  terra::crop(shape) |>
  pad_raster(pfpr_years, years)
names(pfpr_raster) <- paste0("pfpr_", years)

pvpr_years <- 2000:2020
pvpr_files <- paste0("data/pvpr/202206_Global_Pv_Parasite_Rate_", pvpr_years, ".tif")
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
population_years <- 2000:2020
population_files <- paste0("data/population/population_", iso3c, "_", population_years, ".tif")
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
itn_years <- 2000:2020
itn_files <- paste0("data/itn/202106_Africa_Insecticide_Treated_Net_Use_", itn_years, ".tiff")
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
irs_years <- 2000:2020
irs_files <- paste0("data/irs/202106_Africa_Indoor_Residual_Spraying_Coverage_", irs_years, ".tif")
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
tx_years <- 2000:2020
tx_files <- paste0("data/tx/202106_Global_Antimalarial_Effective_Treatment_", tx_years, ".tif")
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
smc_years <- 2012:2020
smc_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

smc_raster <- list()
for(m in seq_along(smc_months)){
  smc_files <- paste0("data/smc/SMC_", smc_years, ".", smc_months[m], ".tif")
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
vector_data <- "data/vector_relative_abundance/"
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

vector_occurrence_data <- "data/vector_occurrence/"
vectors_occurrence <- list.files(vector_occurrence_data) |>
  stringr::str_replace("occurrence_", "") |>
  stringr::str_replace(".tif", "")

vector_occurrence_raster <- list()
for(v in seq_along(vectors_occurrence)){
  vector_files <- paste0(vector_occurrence_data, "occurrence_", vectors_occurrence[v], ".tif")
  vector_occurrence_raster[[vectors_occurrence[v]]] <- terra::rast(vector_files) 
  
  if(extents_overlap(vector_occurrence_raster[[v]], shape)){
    vector_occurrence_raster[[vectors_occurrence[v]]] <- vector_occurrence_raster[[vectors_occurrence[v]]] |>
      terra::crop(shape)  |>
      terra::resample(pfpr_raster) |>
      pad_raster(2011, years, forward_empty = TRUE)
    names(vector_occurrence_raster[[vectors_occurrence[v]]]) <- paste0(vectors_occurrence[v], "_", years)
  } else {
    vector_occurrence_raster[[vectors_occurrence[v]]] <- NA
  }
}
vector_occurrence_raster <- vector_occurrence_raster[!is.na(vector_occurrence_raster)]
vector_occurrence_df <-
  lapply(vector_occurrence_raster, raster_values) |>
  as.data.frame()
colnames(vector_occurrence_df) <- paste0("occurrence_",names(vector_occurrence_raster))
# ------------------------------------------------------------------------------

# Rainfall ---------------------------------------------------------------------
rainfall_years <- 2000:2022
rainfall_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

rainfall_raster <- list()
for(m in seq_along(rainfall_months)){
  rainfall_files <- paste0("data/rainfall/", rainfall_years, "_", rainfall_months[m], ".tif")
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
blood_disorder_years <- 2010

sicklecell_files <- "data/blood_disorders/201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency_2010.tif"
sicklecell_raster <- terra::rast(sicklecell_files)
if(extents_overlap(sicklecell_raster, shape)){
  sicklecell_raster <- sicklecell_raster |>
    terra::crop(shape) |>
    pad_raster(blood_disorder_years, years, forward_empty = TRUE)
} else {
  sicklecell_raster <- NA
}

g6pd_files <- "data/blood_disorders/201201_Global_G6PDd_Allele_Frequency_2010.tif"
g6pd_raster <- terra::rast(g6pd_files)
if(extents_overlap(g6pd_raster, shape)){
  g6pd_raster <- g6pd_raster |>
    terra::crop(shape) |>
    pad_raster(blood_disorder_years, years, forward_empty = TRUE)
} else {
  g6pd_raster <- NA
}

hpc_files <- "data/blood_disorders/201201_Africa_HbC_Allele_Frequency_2010.tif"
hpc_raster <- terra::rast(hpc_files)
if(extents_overlap(hpc_raster, shape)){
  hpc_raster <- hpc_raster |>
    terra::crop(shape) |>
    pad_raster(blood_disorder_years, years, forward_empty = TRUE)
} else {
  hpc_raster <- NA
}

duffy_files <- "data/blood_disorders/201201_Global_Duffy_Negativity_Phenotype_Frequency_2010.tif"
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
travel_time_year <- 2019
city_travel_time_year <- 2015

motor_travel_healthcare_files <- "data/access/202001_Global_Motorized_Travel_Time_to_Healthcare_2019.tif"
motor_travel_healthcare_raster <- terra::rast(motor_travel_healthcare_files)
if(extents_overlap(motor_travel_healthcare_raster, shape)){
  motor_travel_healthcare_raster <- motor_travel_healthcare_raster |>
    terra::crop(shape) |>
    terra::resample(pfpr_raster) |>  
    pad_raster(travel_time_year, years, forward_empty = TRUE)
} else {
  motor_travel_healthcare_raster <- NA
}

walking_travel_healthcare_files <- "data/access/202001_Global_Walking_Only_Travel_Time_To_Healthcare_2019.tif"
walking_travel_healthcare_raster <- terra::rast(walking_travel_healthcare_files)
if(extents_overlap(walking_travel_healthcare_raster, shape)){
  walking_travel_healthcare_raster <- walking_travel_healthcare_raster |>
    terra::crop(shape) |>
    terra::resample(pfpr_raster) |>  
    pad_raster(travel_time_year, years, forward_empty = TRUE)
} else {
  walking_travel_healthcare_raster <- NA
}

city_travel_time_files <- "data/access/201501_Global_Travel_Time_to_Cities_2015.tif"
city_travel_time_raster <- terra::rast(city_travel_time_files)
if(extents_overlap(city_travel_time_raster, shape)){
  city_travel_time_raster <- city_travel_time_raster |>
    terra::crop(shape) |>
    terra::resample(pfpr_raster) |>  
    pad_raster(city_travel_time_year, years, forward_empty = TRUE)
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
  urban_rural = raster_values(urban_rural_raster, na_replace = 0),
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
  dplyr::bind_cols(
    vector_occurrence_df
  ) |>
  # Remove pixels that don't fall within a polygon
  dplyr::filter(!is.na(uid)) |> 
  # Categorise urban_rural
  dplyr::mutate(urban_rural = ifelse(urban_rural == 1, "urban", "rural")) |>
  # Link to shape data
  dplyr::left_join(shape_df, by = "uid") |>
  # Order columns
  dplyr::select(dplyr::any_of(names(lookup)), dplyr::everything())

format(object.size(df), "Mb")

# ------------------------------------------------------------------------------

# Fill interventions without spatial-raster inputs -----------------------------

## Vaccine
rtss_cov_data <- read.csv("data/who/rtss_coverage.csv") |>
  dplyr::group_by(iso3c, name_1) |>
  tidyr::complete(year = min(year):as.integer(format(Sys.Date(), "%Y"))) |>
  tidyr::fill(rtss_cov) |>
  dplyr::ungroup()
if(iso3c %in% rtss_cov_data$iso3c){
  df <- df |>
    dplyr::left_join(
      rtss_cov_data,
      by = c("iso3c", "name_1", "year")
    ) |>
    tidyr::replace_na(
      replace = list(
        rtss_cov = 0
        )
    )
} else {
  df$rtss_cov = 0
}
df$r21_cov <- 0

# Historical larval source management
df$lsm_cov <- 0
# Historical PMC (formerly IPTi)
df$pmc_cov <- 0

# Proportion of treatment that is act
prop_act <- read.csv("data/dhs/proportion_act.csv")
if(iso3c %in% prop_act$iso3c){
  prop_act <-
    prop_act |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::select(year, prop_act)
} else{
  prop_act <-
    prop_act |>
    summarise(
      prop_act = median(prop_act),
      .by = "year"
    )
}
prop_act <- prop_act |>
  tidyr::complete(year = 2000:max(years)) |>
  tidyr::fill(dplyr::all_of("prop_act"))
df <- df |>
  dplyr::left_join(
    prop_act,
    by = "year"
  )

# Proportion of treatment in the public sector
prop_public <- read.csv("data/dhs/proportion_public.csv")
if(iso3c %in% prop_public$iso3c){
  prop_public <-
    prop_public |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::pull(prop_public)
} else{
  prop_act <- median(prop_public$prop_public)
}
prop_public <- data.frame(year = years, prop_public = prop_public)
df <- df |>
  dplyr::left_join(
    prop_public,
    by = "year"
  )
# ------------------------------------------------------------------------------

# Fill nets and IRS outside SSA ------------------------------------------------

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
  nets_distributed <- read.csv("data/who/itn_delivered.csv") |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::left_join(par, by = "year") |>
    dplyr::mutate(
      crop = netz::distribution_to_crop_dynamic(itn_interpolated, netz::net_loss_map, half_life = hl) / par,
      # TODO: using a hybrid here, forcing linear at access < 0.5, might be worth adding to netz package?
      access = crop_to_access2(crop),
      usage = netz::access_to_usage(access, ur),
      people_using_nets = usage * par
    ) |>
    dplyr::select(year, people_using_nets) |>
    tidyr::complete(year = years)
  
  if(sum(is.na(nets_distributed$people_using_nets)) > 0){
    # Extrapolate assuming 3 year cycle
    na_index <- which(is.na(nets_distributed$people_using_nets))
    nets_distributed$people_using_nets[na_index] <- nets_distributed$people_using_nets[pmax(0, na_index - 3)]
  }
  
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
  irs_people <- read.csv("data/who/irs_people_protected.csv") |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::select(year, irs_interpolated) |>
    tidyr::complete(year = years)
  
  if(sum(is.na(irs_people$irs_interpolated)) > 0){
    # Extrapolate assuming last year with data continues
    na_index <- which(is.na(irs_people$irs_interpolated))
    irs_people$irs_interpolated[na_index] <- irs_people$irs_interpolated[max(1, min(na_index) - 1)]
  }
  
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
