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
  files = c("data/map" = paste0("map/", iso3c, "/"))
)

orderly2::orderly_dependency(
  name = "data_worldpop",
  query = "latest()",
  files = c("data/population" = paste0("population/", iso3c, "/"))
)

orderly2::orderly_dependency(
  name = "data_chirps",
  query = "latest()",
  files = c("data/rainfall" = paste0("rainfall/", iso3c, "/"))
)

orderly2::orderly_dependency(
  name = "data_dhs",
  query = "latest()",
  files = "data/dhs/"
)

orderly2::orderly_dependency(
  name = "data_who",
  query = "latest()",
  files = c("data/who/" = "data/")
)

orderly2::orderly_dependency(
  name = "data_vectors",
  query = "latest()",
  files = c("data/vectors" = paste0("vectors/", iso3c, "/"))
)
orderly2::orderly_dependency(
  name = "data_vectors",
  query = "latest()",
  files = c("data/vectors/vector_bionomics.csv" = "vectors/vector_bionomics.csv")
)

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:boundary_version == this:boundary_version)",
  files = c("data/boundaries" = paste0("boundaries/", boundary_version, "/", iso3c, "/"))
)

orderly2::orderly_artefact(
  description = "Spatial data",
  files = "spatial.rds"
)
# ------------------------------------------------------------------------------

# Fixed inputs -----------------------------------------------------------------
years <- 2000:as.integer(format(Sys.Date(), "%Y"))
source("spatial_utils.R")
# ------------------------------------------------------------------------------

# Shape file -------------------------------------------------------------------

# TODO: Update to new netz package

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
pfpr_raster <- terra::rast("data/map/pfpr.tif") |>
  pad_raster(years)
names(pfpr_raster) <- paste0("pfpr_", years)

pvpr_raster <- terra::rast("data/map/pvpr.tif") |>
  pad_raster(years)
names(pvpr_raster) <- paste0("pvpr_", years)

pfpr_limits <- create_limits(pfpr_raster[[1]])
names(pfpr_limits) <- "pfpr_limits"
pvpr_limits <- create_limits(pvpr_raster[[1]])
names(pvpr_limits) <- "pvpr_limits"
pfpr_or_pvpr_limits <- pfpr_limits | pvpr_limits
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
population_raster <- terra::rast("data/population/population.tif") |>
  terra::resample(pfpr_raster, method = "sum") |>
  pad_raster(years)
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
itn_raster <- NA
approximate_itn <- TRUE
if(file.exists("data/map/itn.tif")){
  itn_raster <- terra::rast("data/map/itn.tif")
  itn_max_year <- max(names(itn_raster))
  itn_raster <- itn_raster |>
    pad_raster(years)
  names(itn_raster) <- paste0("itn_use_", years)
  approximate_itn <- FALSE
}
# ------------------------------------------------------------------------------

# IRS -------------------------------------------------------------------------
irs_raster <- NA
approximate_irs <- TRUE
if(file.exists("data/map/irs.tif")){
  irs_raster <- terra::rast("data/map/irs.tif") |>
    pad_raster(years)
  names(irs_raster) <- paste0("irs_cov_", years)
  approximate_irs <- FALSE
}
# ------------------------------------------------------------------------------

# Tx ---------------------------------------------------------------------------
tx_raster <- NA
if(file.exists("data/map/tx.tif")){
  tx_raster <- terra::rast("data/map/tx.tif") |>
    pad_raster(years)
  names(tx_raster) <- paste0("tx_cov_", years)
}
# ------------------------------------------------------------------------------

# SMC --------------------------------------------------------------------------
if(file.exists("data/map/smc.tif")){
  smc_raster <- terra::rast("data/map/smc.tif") |>
    monthify()
  smc_raster <- lapply(smc_raster, pad_raster, all_years = years)
  smc_raster <- lapply(smc_raster, function(x){
    names(x) <- paste0("smc_cov_", years)
    return(x)
  })
} else {
  smc_raster <- lapply(1:12, function(x){
    NA
  })
  names(smc_raster) <- 1:12
}
# ------------------------------------------------------------------------------

# Vectors ----------------------------------------------------------------------
vector_data <- "data/vectors/"
vectors <- c("gambiae", "arabiensis", "funestus")

vector_raster <- list()
for(v in seq_along(vectors)){
  vector_raster[[vectors[v]]] <- NA
  vector_file <- paste0(vector_data, "relative_", vectors[v], ".tif")
  if(file.exists(vector_file)){
    vector_raster[[vectors[v]]] <- terra::rast(vector_file) |>
      terra::resample(pfpr_raster) |>
      pad_raster(years, forward_empty = TRUE)
    names(vector_raster[[vectors[v]]]) <- paste0(vectors[v], "_", years)
  }
}

vectors_occurrence <- list.files(vector_data, pattern = "occurrence*") |>
  stringr::str_replace("occurrence_", "") |>
  stringr::str_replace(".tif", "")

vector_occurrence_raster <- list()
for(v in seq_along(vectors_occurrence)){
  vector_occurrence_raster[[vectors_occurrence[v]]] <- NA
  vector_file <- paste0(vector_data, "occurrence_", vectors_occurrence[v], ".tif")
  if(file.exists(vector_file)){
    vector_occurrence_raster[[vectors_occurrence[v]]] <- terra::rast(vector_file) |>
      terra::resample(pfpr_raster) |>
      pad_raster(years, forward_empty = TRUE)
    names(vector_occurrence_raster[[vectors_occurrence[v]]]) <- paste0(vectors_occurrence[v], "_", years)
  }
}

vector_occurrence_raster <- vector_occurrence_raster[!is.na(vector_occurrence_raster)]
vector_occurrence_df <-
  lapply(vector_occurrence_raster, raster_values) |>
  as.data.frame()
colnames(vector_occurrence_df) <- paste0("occurrence_",names(vector_occurrence_raster))
# ------------------------------------------------------------------------------

# Rainfall ---------------------------------------------------------------------
rainfall_raster <- terra::rast("data/rainfall/rainfall.tif") |>
  terra::resample(pfpr_raster) |>
  monthify(sep = "_")

rainfall_raster <- lapply(rainfall_raster, pad_raster, all_years = years)
rainfall_raster <- lapply(rainfall_raster, function(x){
  names(x) <- paste0("rainfall_", years)
  return(x)
})
# ------------------------------------------------------------------------------

# Blood disorders --------------------------------------------------------------

sicklecell_raster <- NA
if(file.exists("data/map/sickle.tif")){
  sicklecell_raster <- terra::rast("data/map/sickle.tif") |>
    pad_raster(years, forward_empty = TRUE)
}

g6pd_raster <- NA
if(file.exists("data/map/g6pd.tif")){
  g6pd_raster <- terra::rast("data/map/g6pd.tif") |>
    pad_raster(years, forward_empty = TRUE)
}

hpc_raster <- NA
if(file.exists("data/map/hbc.tif")){
  hpc_raster <- terra::rast("data/map/hbc.tif") |>
    pad_raster(years, forward_empty = TRUE)
}

duffy_raster <- NA
if(file.exists("data/map/duffy.tif")){
  duffy_raster <- terra::rast("data/map/duffy.tif") |>
    pad_raster(years, forward_empty = TRUE)
}
# ------------------------------------------------------------------------------

# Travel times -----------------------------------------------------------------
motor_travel_healthcare_raster <- NA
if(file.exists("data/map/motor.tif")){
  motor_travel_healthcare_raster <- terra::rast("data/map/motor.tif") |>
    terra::resample(pfpr_raster) |>  
    pad_raster(years, forward_empty = TRUE)
}

walking_travel_healthcare_raster <- NA
if(file.exists("data/map/walk.tif")){
  walking_travel_healthcare_raster <- terra::rast("data/map/walk.tif") |>
    terra::resample(pfpr_raster) |>  
    pad_raster(years, forward_empty = TRUE)
}

city_travel_time_raster <- NA
if(file.exists("data/map/cities.tif")){
  city_travel_time_raster <- terra::rast("data/map/cities.tif") |>
    terra::resample(pfpr_raster) |>  
    pad_raster(years, forward_empty = TRUE)
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
    dplyr::summarise(
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



# Additional nets and IRS interpolation ----------------------------------------
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


# Update itn use for years post last itn use raster, where we have information
# on nets delivered to the country
if(!approximate_itn){
  # Assume median half life and usage rate
  hl <- netz::get_halflife(iso3c)
  ur <- netz::get_usage_rate(iso3c)
  
  # Estimate the total people using nets each year | WHO net delivery/distribution
  wmr_use <- read.csv("data/who/llins_delivered.csv") |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::left_join(par, by = "year") |>
    dplyr::mutate(
      # Assume here distributions are made on first day of year and crop measured at the midpoint
      crop = netz::distribution_to_crop(
        distribution = itn_interpolated,
        distribution_timesteps = 365 * (year - 2000) + 1,
        crop_timesteps = 365 * (year - 2000) + (365 / 2),
        netz::net_loss_map, half_life = hl) / par,
      access = netz::crop_to_access(crop),
      usage = netz::access_to_usage(access, ur)
    ) |>
    dplyr::filter(year > itn_max_year) |>
    dplyr::select(year, usage)
  
  df_use <- df |>
    dplyr::summarise(
      wu = weighted.mean(itn_use, par),
      .by = "year"
    )
  
  max_observed_use <- max(df$itn_use, na.rm = TRUE)
  for(i in seq_along(wmr_use$year)){
    year <- wmr_use$year[i]
    scaler <- wmr_use[wmr_use$year == year, "usage"] / df_use[df_use$year == year, "wu"]
    df[df$year == year, "itn_use"] <- pmin(max_observed_use, df[df$year == year, "itn_use"] * scaler)
  }
}

if(approximate_itn){
  
  # Assume median half life and usage rate
  hl <- netz::get_halflife()
  ur <- netz::get_usage_rate()
  
  # Estimate the total people using nets each year | WHO net delivery/distribution
  nets_distributed <- read.csv("data/who/llins_delivered.csv") |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::left_join(par, by = "year") |>
    dplyr::mutate(
      # Assume here distributions are made on first day of year and crop measured at the midpoint
      crop = netz::distribution_to_crop(
        distribution = itn_interpolated,
        distribution_timesteps = 365 * (year - 2000) + 1,
        crop_timesteps = 365 * (year - 2000) + (365 / 2),
        netz::net_loss_map, half_life = hl) / par,
      access = netz::crop_to_access(crop),
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
# ------------------------------------------------------------------------------
