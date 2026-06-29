# Orderly set-up ---------------------------------------------------------------
orderly::orderly_description(
  display = "Site file",
  long = "Processes and formats data into the site file configuration"
)

p <- orderly::orderly_parameters(
  boundary = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL,
  version = NULL
)

orderly::orderly_resource(
  files = "site_file_utils.R"
)

orderly::orderly_shared_resource("utils.R")

orderly::orderly_dependency(
  name = "demography",
  query = "latest(parameter:iso3c ==  this:iso3c)",
  files = c("adjusted_demography.rds")
)

orderly::orderly_dependency(
  name = "spatial",
  query = "latest(parameter:boundary == this:boundary && parameter:iso3c ==  this:iso3c)",
  files = c("spatial.rds")
)

orderly::orderly_dependency(
  name = "data_interventions_manual",
  query = "latest()",
  files = "vaccine_delivery.csv"
)

orderly::orderly_dependency(
  name = "population",
  query = "latest(parameter:boundary == this:boundary && parameter:iso3c ==  this:iso3c)",
  files = c("population.rds", "population_age.rds")
)

orderly::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:boundary == this:boundary)",
  files = c("boundaries" = paste0("boundaries/", boundary, "/", iso3c, "/"))
)

orderly::orderly_dependency(
  name = "data_vectors",
  query = "latest()",
  files = c(
    "vectors/irs_insecticide_parameters.csv" = "vectors/irs_insecticide_parameters.csv",
    "vectors/net_efficacy_adjusted.csv" = "vectors/net_efficacy_adjusted.csv",
    "vectors/pyrethroid_resistance.csv" = "vectors/pyrethroid_resistance.csv",
    "vectors/new_net_introductions.csv" = "vectors/new_net_introductions.csv",
    "vectors/vector_bionomics.csv" = "vectors/vector_bionomics.csv"
  )
)

orderly::orderly_dependency(
  name = "data_who",
  query = "latest()",
  files = c(
    "wmr_cases_deaths.csv" = "wmr_cases_deaths.csv",
    "wmr_itns_distributed.csv" = "wmr_itns_distributed.csv",
    "wmr_irs_people_protected.csv" = "wmr_irs_people_protected.csv"
  )
)

orderly::orderly_dependency(
  name = "data_dhs",
  query = "latest()",
  files = c(
    "proportion_public.csv" = "data/dhs/proportion_public.csv"
  )
)

orderly::orderly_artefact(
  description = "Site file",
  files = "site.rds"
)
# ------------------------------------------------------------------------------

# Load inputs ------------------------------------------------------------------
source("utils.R")
source("site_file_utils.R")
spatial <- readRDS("spatial.rds")
# ------------------------------------------------------------------------------

# Grouping variable ------------------------------------------------------------
grouping <- c("country", "iso3c", paste0("name_", 1:admin_level))
if(urban_rural){
  grouping <- c(grouping, "urban_rural")
}
if(!all(grouping %in% names(spatial))){
  stop("Admin-level(s) missing - you may need to aggregate to a higher level")
}
# ------------------------------------------------------------------------------

# Sites ------------------------------------------------------------------------
sites <- unique(spatial[, grouping])
# ------------------------------------------------------------------------------

# Shape ------------------------------------------------------------------------
shape <- list()
## User provided admin levels and all higher levels included
levels <- admin_level:0
for(level in levels){
  
  shape_address <- paste0(
    "boundaries/",
    iso3c,
    "_",
    level,
    ".RDS"
  )
  
  s <- readRDS(shape_address)
  
  lookup <- c(
    uid = "uid",
    iso3c = "GID_0",
    country = "COUNTRY",
    name_1 = "NAME_1",
    name_2 = "NAME_2",
    name_3 = "NAME_3",
    geom = "geom"
  )
  
  s <- s |>
    dplyr::rename(
      dplyr::any_of(lookup)
    ) |>
    dplyr::select(dplyr::any_of(names(lookup)))
  
  shape[[paste0("level_", level)]] <- s
}

# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
population <- readRDS("population.rds") |>
  dplyr::summarise(
    pop = sum(pop),
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = dplyr::all_of(c(grouping, "year"))
  )

population_age <- readRDS("population_age.rds") |>
  dplyr::summarise(
    pop = sum(pop),
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = dplyr::all_of(c(grouping, "year", "age_lower", "age_upper"))
  )
# ------------------------------------------------------------------------------

# Pyrethroid resistance --------------------------------------------------------
old_resistance <- read.csv("vectors/pyrethroid_resistance.csv")

if(iso3c %in% old_resistance$iso3c){
  old_resistance <- old_resistance |>
    dplyr::filter(iso3c == {{iso3c}}) 
  old_centroids <- old_resistance |>
    dplyr::select(unit, X, Y) |>
    unique() |>
    dplyr::filter(!(is.na(X) | is.na(Y)))
  
  new_centroids <- shape[[paste0("level_", admin_level)]]
  sf::st_agr(new_centroids) <- "constant"
  new_centroids <- new_centroids |>
    sf::st_centroid()
  
  unit <- c()
  for(i in 1:nrow(new_centroids)){
    coord <- sf::st_geometry(new_centroids[i,]) |>
      sf::st_coordinates()
    dist <- sqrt((coord[,1] - old_centroids$X) ^ 2 + (coord[,2] - old_centroids$Y) ^ 2)
    unit[i] <- old_centroids$unit[which.min(dist)]
  }
  
  pyrethroid_resistance <- shape[[paste0("level_", admin_level)]] |>
    sf::st_drop_geometry() |>
    dplyr::mutate(unit = unit) |>
    dplyr::left_join(old_resistance,
                     by = c("iso3c", "unit"),
                     relationship = "many-to-many"
    ) |>
    dplyr::select(-unit, -X, -Y)
} else {
  pyrethroid_resistance <- shape[[paste0("level_", admin_level)]] |>
    sf::st_drop_geometry() |>
    dplyr::mutate(pyrethroid_resistance = 0) |>
    dplyr::cross_join(data.frame(year = 2000:2050))
}
# ------------------------------------------------------------------------------

# Prevalence -------------------------------------------------------------------
prevalence <- spatial |>
  dplyr::summarise(
    pfpr = weighted.mean2(pfpr, par_pf),
    pvpr = weighted.mean2(pvpr, par_pv),
    .by = dplyr::all_of(c(grouping, "year"))
  )

if(urban_rural){
  # Some areas (usually urban) may come into (or drop out of) existence after 2000
  # for these areas we assume prevalence for model inputs maps to
  # the matching admin unit (urban -> rural or rural -> urban)
  not_full <- prevalence |>
    dplyr::mutate(
      n = dplyr::n(),
      .by = dplyr::all_of(grouping)
    ) |>
    dplyr::filter(
      n != max(n)
    ) |>
    dplyr::select(dplyr::all_of(c(grouping, "year")))
  
  if(nrow(not_full) > 0){
    replaced <- not_full |>
      dplyr::mutate(
        urban_rural = ifelse(urban_rural == "urban", "rural", "urban")
      ) |>
      tidyr::complete(year = min(prevalence$year):max(prevalence$year), tidyr::nesting(!!!rlang::syms(grouping))) |>
      dplyr::left_join(prevalence, by = c(grouping, "year")) |>
      dplyr::mutate(
        urban_rural = ifelse(urban_rural == "urban", "rural", "urban")
      ) |>
      dplyr::anti_join(not_full, by = c(grouping, "year"))
    
    prevalence <- prevalence |>
      dplyr::bind_rows(replaced) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping, "year"))))
  }
}
# ------------------------------------------------------------------------------

# Cases and Deaths from WMR ----------------------------------------------------
cases_deaths <- read.csv("wmr_cases_deaths.csv") |>
  tidyr::fill(country, iso3c) |>
  dplyr::filter(iso3c == {{iso3c}}) |>
  dplyr::mutate(
    wmr_incidence_l = wmr_cases_l / wmr_par,
    wmr_incidence = wmr_cases / wmr_par,
    wmr_incidence_u = wmr_cases_u / wmr_par,
    wmr_mortality_l = wmr_deaths_l / wmr_par,
    wmr_mortality = wmr_deaths / wmr_par,
    wmr_mortality_u = wmr_deaths_u / wmr_par,
  )
# ------------------------------------------------------------------------------

# Demography -------------------------------------------------------------------
demography <- readRDS("adjusted_demography.rds") |>
  dplyr::select(-fitted_age_dist)
# ------------------------------------------------------------------------------

# Seasonality ------------------------------------------------------------------
rainfall <- spatial |>
  dplyr::summarise(
    dplyr::across(paste0("rainfall_", 1:12), \(x) weighted.mean2(x, w = par)),
    .by = dplyr::all_of(c(grouping, "year"))
  ) |>
  tidyr::pivot_longer(
    cols = paste0("rainfall_", 1:12),
    names_to = "month",
    values_to = "rainfall",
    names_prefix = "rainfall_",
    names_transform = as.integer
  ) |>
  dplyr::left_join(
    middle_days,
    by = "month"
  ) |>
  tidyr::replace_na(
    list(rainfall = 0)
  ) |>
  dplyr::select(
    dplyr::all_of(grouping), year, month, month_name, t, rainfall
  )

seasonal_parameters <- rainfall|>
  dplyr::summarise(
    coefficients = fit_fourier_df(rainfall, t),
    .by = dplyr::all_of(grouping)
  ) |>
  tidyr::unnest(col = "coefficients") |>
  tidyr::pivot_wider(names_from = coefficient, values_from = value)

seasonal_curve <- seasonal_parameters |>
  dplyr::summarise(
    predict = list(
      umbrella::fourier_predict(
        coef = c(g0, g1, g2, g3, h1, h2, h3),
        t = 1:365,
        floor = 0
      )
    ),
    .by = dplyr::all_of(grouping)
  ) |>
  tidyr::unnest("predict")

peak_season <- seasonal_curve |>
  dplyr::slice_max(profile, n = 1, by = dplyr::all_of(grouping)) |>
  dplyr::rename("peak_season" = "t") |>
  dplyr::select(dplyr::all_of(c(grouping, "peak_season")))
# ------------------------------------------------------------------------------

# Interventions ----------------------------------------------------------------

# TODO: Should all "Timesteps" or "Model day" values be relative to the year.
## We need to deal with adding "burnin" for the sites. Absolute timesteps makes
## this hard, and actually isn't meaningful to the site-file user?

interventions <- spatial |>
  dplyr::summarise(
    # Some map has some areas with NA (usually due to a misalignment between)
    # boundary file and raster) that we define as having PAR, these 
    # shouldn't be interpreted as 0, so are dropped 
    tx_cov = weighted.mean2(tx_cov, par, na.rm = TRUE),
    itn_use = weighted.mean2(itn_use, par, na.rm = TRUE),
    irs_cov = weighted.mean2(irs_cov, par, na.rm = TRUE),
    rtss_cov = weighted.mean2(rtss_cov, par, na.rm = TRUE),
    r21_cov = weighted.mean2(r21_cov, par, na.rm = TRUE),
    lsm_cov = weighted.mean2(lsm_cov, par, na.rm = TRUE),
    smc_cov = weighted.mean2(smc_cov, par, na.rm = TRUE),
    pmc_cov = weighted.mean2(pmc_cov, par, na.rm = TRUE),
    prop_act = weighted.mean2(prop_act, par, na.rm = TRUE),
    #dplyr::across(dplyr::contains("smc"), \(x) weighted.mean2(x, par, na.rm = TRUE)),
    .by = dplyr::all_of(c(grouping, "year"))
  ) |>
  dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping, "year"))))

if(urban_rural){
  # Some areas (usually urban) may come into (or drop out of) existence after 2000
  # for these areas we assume coverage of interventions for model inputs maps to
  # the matching admin unit (urban -> rural or rural -> urban)
  not_full <- interventions |>
    dplyr::mutate(
      n = dplyr::n(),
      .by = dplyr::all_of(grouping)
    ) |>
    dplyr::filter(
      n != max(n)
    ) |>
    dplyr::select(dplyr::all_of(c(grouping, "year")))
  
  if(nrow(not_full) > 0){
    replaced <- not_full |>
      dplyr::mutate(
        urban_rural = ifelse(urban_rural == "urban", "rural", "urban")
      ) |>
      tidyr::complete(year = min(interventions$year):max(interventions$year), tidyr::nesting(!!!rlang::syms(grouping))) |>
      dplyr::left_join(interventions, by = c(grouping, "year")) |>
      dplyr::mutate(
        urban_rural = ifelse(urban_rural == "urban", "rural", "urban")
      ) |>
      dplyr::anti_join(not_full, by = c(grouping, "year"))
    
    interventions <- interventions |>
      dplyr::bind_rows(replaced)
  }
}

interventions <- interventions |>
  # Some areas are not covered at all, so tx_cov gets the region year median
  dplyr::mutate(
    tx_cov = ifelse(is.na(tx_cov), median(tx_cov, na.rm = TRUE), tx_cov),
    .by = c(grouping[1:3], "year")
  ) |>
  # Some areas are not covered at all, so tx_cov gets the country year median
  dplyr::mutate(
    tx_cov = ifelse(is.na(tx_cov), median(tx_cov, na.rm = TRUE), tx_cov),
    .by = "year"
  )

interventions <- interventions |>
  dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping, "year"))))

## Treatment
tx_cov <- interventions |>
  dplyr::mutate(day_of_year = 1) |>
  dplyr::select(dplyr::all_of(c(grouping, "year", "day_of_year", "tx_cov", "prop_act")))

# Proportion of treatment in the public sector
tx_prop_public <- read.csv("proportion_public.csv")
if(iso3c %in% tx_prop_public$iso3c){
  tx_prop_public <-
    tx_prop_public |>
    dplyr::filter(iso3c == {{iso3c}}) |>
    dplyr::pull(prop_public)
} else{
  tx_prop_public <- median(tx_prop_public$prop_public)
}

# ITNs
itn_hl <- netz::get_halflife(iso3c)

itn_data_latest_year <- 2023 
itn_use <- interventions |>
  dplyr::select(dplyr::all_of(c(grouping, "year", "itn_use"))) |>
  # As a crude, but ok approximation (have checked) to these being average use over the year, 
  ## we assume the usage value is taken at a single timepoint in the year. The user can always
  ## specify specific distributions and usage data points for more accuracy
  dplyr::mutate(usage_day_of_year = 182) |>
  # Instead of constant use extrapolation we use %% 3 years
  dplyr::mutate(
    itn_use = ifelse(
      year > itn_data_latest_year, 
      itn_use[match(year - 3, year)], 
      itn_use
    ),
    .by = dplyr::all_of(grouping)
  )

new_net_introductions <- read.csv("vectors/new_net_introductions.csv") 
itn_implementation <- interventions |>
  dplyr::select(dplyr::all_of(c(grouping, "year"))) |>
  dplyr::mutate(
    net_type = ifelse(year == 2000, "pyrethroid_only", NA)
  ) |>
  dplyr::left_join(new_net_introductions, by = c("iso3c", "name_1", "year")) |>
  dplyr::mutate(net_type = ifelse(is.na(type), net_type, type)) |>
  dplyr::select(-type) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
  tidyr::fill(net_type) |>
  dplyr::ungroup() |>
  tidyr::expand_grid(
    data.frame(
      distribution_type = c("mass", rep("routine", 4)),
      distribution_day_of_year = c(1, 2, 90, 180, 270),
      distribution_lower = 0
    )
  ) |>
  dplyr::mutate(
    distribution_upper = dplyr::case_when(
      distribution_type == "mass" ~ 1,
      distribution_type == "routine" ~ 0.01
    )
  ) |>
  dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping, "year")))) |>
  dplyr::filter((year + distribution_day_of_year)<= max(
    itn_use$year + itn_use$usage_day_of_year
  ))
  

if(FALSE){
  # TODO: remove, The following gets done by site:
  # TODO: site should check that there isn't more than one distribution per day in the
  ## Flow:
  ### Parameters given to site
  ### Checks for columns itn_input_dist and itn_predicted_use
  ### If missing prompts the user to add them before running the site parameters
  ### inputs
  t1 <- dplyr::filter(itn_implementation, name_1 == "Mukono", urban_rural == "rural")
  t2 <- dplyr::filter(itn_use, name_1 == "Mukono", urban_rural == "rural")
  
  itn_input_dist <- netz::usage_to_model_distribution(
    usage = t2$itn_use,
    usage_timesteps = t2$usage_timestep,
    distribution_timesteps = t1$distribution_timestep,
    distribution_upper = t1$upper,
    distribution_lower = t1$lower,
    net_loss_function = netz::net_loss_map,
    half_life = itn_hl
  )
  
  itn_predicted_use <- netz::model_distribution_to_usage(
    usage_timesteps = t2$usage_timestep,
    distribution = itn_input_dist,
    distribution_timesteps = t1$distribution_timestep,
    net_loss_function = netz::net_loss_map,
    half_life = itn_hl
  )
  y <- 2000 + (t2$usage_timestep - 1) / 365
  plot(itn_predicted_use ~ y, t = "l", ylim =c(0, 1))
  points(2000:2026, t2$itn_use, pch = 19)  
  points(2000 + (t1$distribution_timestep - 1) / 365, itn_input_dist, col = "orange")
}

actellic_switch_year <- 2017
irs_implementation <- interventions |>
  dplyr::select(dplyr::all_of(c(grouping, "year", "irs_cov"))) |>
  dplyr::left_join(peak_season, by = grouping) |>
  dplyr::mutate(
    # Switch to Actellic-like insecticide
    insecticide = ifelse(year < actellic_switch_year, "ddt", "actellic"),
    # Assume single round per year
    round = 1,
    spray_day_of_year = dplyr::case_when(
      # First round assumed 3 months prior to peak
      round == 1 ~ peak_season - (3 * 30),
      # Second round (if implemented) assumed 3 months post peak
      round == 2 ~ peak_season + (3 * 30)
    ),
    # Deal with overlap into preceding or next year
    year = ifelse(spray_day_of_year < 1, year - 1, year),
    year = ifelse(spray_day_of_year > 365, year + 1, year),
    spray_day_of_year = ifelse(spray_day_of_year < 1, spray_day_of_year + 365, spray_day_of_year),
    spray_day_of_year = ifelse(spray_day_of_year > 365, spray_day_of_year - 365, spray_day_of_year),
  ) |>
  dplyr::filter(year >= 2000)

smc_drug <- "sp_aq"

smc_implementation <- interventions |>
  dplyr::select(dplyr::all_of(c(grouping, "year", "smc_cov"))) |>
  dplyr::left_join(peak_season, by = grouping) |>
  dplyr::mutate(
    smc_min_age = 91,
    smc_max_age = 1825
  ) |>
  tidyr::expand_grid(
    data.frame(
      round = 1:4
    )
  ) |>
  dplyr::mutate(
    # Assumed 1 month apart, centered on seasonal peak
    round_day_of_year = dplyr::case_when(
      round == 1 ~ peak_season - 45,
      round == 2 ~ peak_season - 15,
      round == 3 ~ peak_season + 15,
      round == 4 ~ peak_season + 45
    ),
    # Deal with overlap into preceding or next year
    year = ifelse(round_day_of_year < 1, year - 1, year),
    year = ifelse(round_day_of_year > 365, year + 1, year),
    round_day_of_year = ifelse(round_day_of_year < 1, round_day_of_year + 365, round_day_of_year),
    round_day_of_year = ifelse(round_day_of_year > 365, round_day_of_year - 365, round_day_of_year),
  ) |>
  dplyr::filter(year >= 2000)

pmc_age <- c(2, 3, 9) * 30
pmc_drug <- "sp"
pmc_coverage <- interventions |>
  dplyr::mutate(day_of_year = 1) |>
  dplyr::select(dplyr::all_of(c(grouping, "year", "day_of_year", "pmc_cov")))


# TODO: in site have to devide booser cov by primary cov to get correct input form
vaccine_data <- read.csv("vaccine_delivery.csv") |>
  dplyr::filter(iso3c == {{iso3c}}) |>
  dplyr::slice_max(year, n = 1) |>
  dplyr::slice_tail(n = 1)

if(nrow(vaccine_data) == 0){
  vaccine_delivery <- "age-based"
  vaccine_primary_schedule <- round(c(6, 7, 8) * (365 / 12))
  vaccine_booster_space <- 365
} else {
  vaccine_delivery <- unlist(vaccine_data$delivery)
  vaccine_primary_schedule <- round(c(unlist(vaccine_data[,c("first", "second", "third")])) * (365 / 12))
  vaccine_booster_space <- NA
  if(vaccine_delivery == "age-based"){
    vaccine_booster_space = round(unlist(vaccine_data[,"boost1"]) * (365 / 12)) - vaccine_primary_schedule[3]
  }
}

vaccine_coverage <- interventions |>
  dplyr::mutate(day_of_year = 1) |>
  dplyr::select(dplyr::all_of(c(grouping, "year", "day_of_year", "r21_cov", "rtss_cov"))) |>
  dplyr::left_join(peak_season, by = grouping) |>
  dplyr::rename(
    "r21_primary_cov" = "r21_cov",
    "rtss_primary_cov" = "rtss_cov"
  ) |>
  dplyr::mutate(
    r21_booster1_cov = r21_primary_cov,
    rtss_booster1_cov = rtss_primary_cov
  )

lsm_coverage <- interventions |>
  dplyr::mutate(day_of_year = 1) |>
  dplyr::select(dplyr::all_of(c(grouping, "year", "day_of_year", "lsm_cov")))

interventions_list <- list(
  treatment = list(
    implementation = tx_cov,
    prop_public = tx_prop_public
  ),
  itn = list(
    retention_half_life = itn_hl,
    use = itn_use,
    implementation = itn_implementation
  ),
  irs = list(
    implementation = irs_implementation
  ),
  smc = list(
    drug = smc_drug,
    implementation = smc_implementation
  ),
  pmc = list(
    drug = pmc_drug,
    age = pmc_age,
    implementation = pmc_coverage
  ),
  vaccine = list(
    delivery = vaccine_delivery,
    primary_schedule = vaccine_primary_schedule,
    booster_spacing = vaccine_booster_space,
    implementation = vaccine_coverage
  ),
  lsm = list(
    implementation = lsm_coverage
  )
)
# ------------------------------------------------------------------------------

# Vectors ----------------------------------------------------------------------
use_relative <- !all(is.na(spatial[,c("gambiae", "arabiensis", "funestus")]))

if(use_relative){
  vector_columns <- c("gambiae", "arabiensis", "funestus")
  vector_year <- 2016
} else {
  vector_columns <- names(spatial)[grepl("occurrence", names(spatial))]
  vector_year <- 2011
}

bionomics <- read.csv("vectors/vector_bionomics.csv")

vectors <- spatial |>
  dplyr::select(
    dplyr::all_of(
      c(grouping, "par", vector_columns)
    )
  ) |>
  dplyr::mutate(
    across(
      dplyr::all_of(vector_columns), \(x) ifelse(is.na(x), 0, x)
    )
  ) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vector_columns), \(x) weighted.mean2(x, par, na.rm = TRUE)
    ),
    .by = dplyr::all_of(grouping)
  ) |>
  tidyr::pivot_longer(
    -dplyr::all_of(grouping), names_to = "species",  values_to = "prop"
  ) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
  dplyr::mutate(rank = rank(-prop, ties = "first")) |>
  dplyr::filter(rank <= 3) |>
  dplyr::select(-rank) |>
  # Missing values get assigned equal probability of occurence
  dplyr::mutate(prop = ifelse(is.na(prop) | sum(prop) == 0, 1/dplyr::n(), prop)) |>
  dplyr::mutate(prop = prop / sum(prop)) |>
  dplyr::ungroup() |>
  dplyr::mutate(species = stringr::str_replace(species, "occurrence_", "")) |>
  dplyr::left_join(bionomics, by = "species")
# ------------------------------------------------------------------------------

# Blood disorders --------------------------------------------------------------
blood_disorders <- spatial |>
  dplyr::summarise(
    sicklecell = weighted.mean2(sicklecell, par, na.rm = TRUE),
    gdp6 = weighted.mean2(gdp6, par, na.rm = TRUE),
    hpc = weighted.mean2(hpc, par, na.rm = TRUE),
    duffy_negativity = weighted.mean2(duffy_negativity, par, na.rm = TRUE),
    .by = dplyr::all_of(grouping)
  )
# ------------------------------------------------------------------------------

# Accessibility -----------------------------------------------------------------
accessibility <- spatial |>
  dplyr::summarise(
    motor_travel_time_healthcare = weighted.mean2(motor_travel_time_healthcare, par, na.rm = TRUE),
    walking_travel_time_healthcare = weighted.mean2(walking_travel_time_healthcare, par, na.rm = TRUE),
    city_travel_time = weighted.mean2(city_travel_time, par, na.rm = TRUE),
    .by = dplyr::all_of(grouping)
  )
# ------------------------------------------------------------------------------

# EIR --------------------------------------------------------------------------
prevalence_summary <- prevalence |>
  dplyr::filter(year == 2000)

pf_eir <- prevalence_summary |>
  dplyr::filter(pfpr > 0.005) |>
  dplyr::select(dplyr::all_of(grouping)) |>
  dplyr::mutate(
    sp = "pf",
    eir = NA
  )
pv_eir <- prevalence_summary |>
  dplyr::filter(pvpr > 0.005) |>
  dplyr::select(dplyr::all_of(grouping)) |>
  dplyr::mutate(
    sp = "pv",
    eir = NA
  )
eir <- dplyr::bind_rows(pf_eir, pv_eir)
# ------------------------------------------------------------------------------

# Create the site file ---------------------------------------------------------
site_file <- list()

site_file$country = unique(spatial$country)
site_file$boundary = boundary
site_file$admin_level = grouping

site_file$metadata <- list(
  country = unique(spatial$country),
  iso3c = iso3c,
  boundary = boundary,
  admin_level = grouping,
  version = version
)

site_file$sites = sites

site_file$shape = shape

site_file$cases_deaths = cases_deaths

site_file$prevalence = prevalence

if(sum(is.na(interventions)) > 0){
  stop("NAs in interventions")
}
site_file$interventions = interventions_list

if(any(population$par > population$pop)){
  stop("PAR > pop in population_total")
}
if(any(population_age$par > population_age$pop)){
  stop("PAR > pop in population_age")
}
if(any(population_age$par_pf > population_age$par)){
  stop("PAR_pf > par in population_age")
}
if(any(population_age$par_pv > population_age$par)){
  stop("PAR_pv > par in population_age")
}

site_file$population = list(
  population_total = population,
  population_by_age = population_age
)

site_file$demography = demography

site_file$vectors = list(
  vector_species = vectors,
  pyrethroid_resistance = pyrethroid_resistance
)

site_file$seasonality = list(
  seasonality_parameters = seasonal_parameters,
  monthly_rainfall = rainfall,
  fourier_prediction = seasonal_curve,
  peak_season = peak_season
)

site_file$blood_disorders = blood_disorders

site_file$accessibility = accessibility

site_file$eir = eir

format(object.size(site_file), "Mb")

# Check that the resulting site file can be used to create a malariasimulation
## parameter list
check_params(site_file)

saveRDS(site_file, "site.rds")
# ------------------------------------------------------------------------------


