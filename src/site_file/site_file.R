# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Site file",
  long = "Processes and formats data into the site file configuration"
)

orderly2::orderly_parameters(
  version = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL
)

orderly2::orderly_resource(
  files = "site_file_utils.R"
)

orderly2::orderly_shared_resource("utils.R")

orderly2::orderly_dependency(
  name = "demography",
  query = "latest(parameter:version == this:version && parameter:iso3c ==  this:iso3c)",
  files = c("adjusted_demography.rds")
)

orderly2::orderly_dependency(
  name = "spatial",
  query = "latest(parameter:version == this:version && parameter:iso3c ==  this:iso3c)",
  files = c("spatial.rds")
)

orderly2::orderly_dependency(
  name = "population",
  query = "latest(parameter:version == this:version && parameter:iso3c ==  this:iso3c)",
  files = c("population.rds", "population_age.rds")
)

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:version == this:version)",
  files = c("boundaries" = paste0("boundaries/", version, "/", iso3c, "/"))
)

orderly2::orderly_dependency(
  name = "data_vectors",
  query = "latest(parameter:version == this:version)",
  files = c(
    "vectors/irs_insecticide_parameters.csv" = "vectors/irs_insecticide_parameters.csv",
    "vectors/net_efficacy_adjusted.csv" = "vectors/net_efficacy_adjusted.csv",
    "vectors/pyrethroid_resistance.csv" = "vectors/pyrethroid_resistance.csv",
    "vectors/new_net_introductions.csv" = "vectors/new_net_introductions.csv",
    "vectors/vector_bionomics.csv" = "vectors/vector_bionomics.csv"
  )
)

orderly2::orderly_dependency(
  name = "data_who",
  query = "latest(parameter:version == this:version)",
  files = c("wmr_cases_deaths.csv" = "data/wmr_cases_deaths.csv")
)

orderly2::orderly_artefact(
  description = "Site file",
  files = "site.rds"
)
# ------------------------------------------------------------------------------

# Load inputs ------------------------------------------------------------------
source("utils.R")
source("site_file_utils.R")
spatial <- readRDS("spatial.rds")
#external_data_address <- "C:/Users/pwinskil/OneDrive - Imperial College London/malaria_sites_data/2023/"
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

# Interventions ----------------------------------------------------------------
interventions <- spatial |>
  dplyr::summarise(
    # Some map has some areas with NA (usually due to a misalignment between)
    # boundary file and raster) that we define as having PAR, these 
    # shouldn't be interpreted as 0, so are dropped 
    tx_cov = weighted.mean2(tx_cov, par, na.rm = TRUE),
    itn_use = weighted.mean2(itn_use, par, na.rm = TRUE),
    irs_cov = weighted.mean2(irs_cov, par), na.rm = TRUE,
    rtss_cov = weighted.mean2(rtss_cov, par, na.rm = TRUE),
    r21_cov = weighted.mean2(r21_cov, par, na.rm = TRUE),
    lsm_cov = weighted.mean2(lsm_cov, par, na.rm = TRUE),
    pmc_cov = weighted.mean2(pmc_cov, par, na.rm = TRUE),
    prop_act = weighted.mean2(prop_act, par, na.rm = TRUE),
    prop_public = weighted.mean2(prop_public, par, na.rm = TRUE),
    dplyr::across(dplyr::contains("smc"), \(x) weighted.mean2(x, par, na.rm = TRUE)),
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

## Overwrite SMC, as we cannot currently use the new MAP estimates widely
smc_overwrite <- interventions
smc_overwrite$smc <- ifelse(rowSums(interventions[,paste0("smc_", 1:12)]) > 0, 1, 0)
smc_overwrite$smc_n_rounds <- rowSums(interventions[,paste0("smc_", 1:12)] > 0.5) 
smc_overwrite <- smc_overwrite |>
  dplyr::summarise(
    smc_cov = ifelse(mean(smc) > 0.5, 0.9, 0),
    smc_n_rounds = round(mean(smc_n_rounds, na.rm = TRUE)),
    .by = dplyr::all_of(c(grouping[1:3], "year"))
  )
interventions <- interventions |>
  dplyr::select(-dplyr::contains("smc")) |>
  dplyr::left_join(
    smc_overwrite,
    by = c(grouping[1:3], "year")
  )
interventions <- interventions |>
  dplyr::mutate(
    smc_min_age = 91,
    smc_max_age = 1825,
    smc_drug = "sp_aq"
  )

# Add in IRS assumptions
irs_parameters <- read.csv("vectors/irs_insecticide_parameters.csv")
actellic_switch_year <- 2017
interventions <- interventions |>
  dplyr::mutate(
    # Switch to Actellic-like insecticide
    irs_insecticide = ifelse(year < actellic_switch_year, "ddt", "actellic"),
    # Assume 1 round per year
    irs_spray_rounds = 1
  ) |>
  dplyr::left_join(irs_parameters, by = "irs_insecticide")

# Any remaining missing values we extrapolate forwards from last available data point
interventions <- interventions |>
  scene::fill_extrapolate(
    group_var = grouping,
    not = NULL
  )

# In rare cases we miss early data (if pops don't exist in urban areas), here
# we extrapolate backwards to ensure a complete data frame for interventions
interventions <- interventions |>
  scene::fill_extrapolate(
    group_var = grouping,
    not = NULL,
    dir = "up"
  )


## ITN half-life to mean retention conversion
## Match our exponential mean retention as closely as possible to the MAP 
## function with given half life (min sum of squared differences over first 3 years):
if(iso3c %in% netz::halflife$iso3c){
  hl <- netz::get_halflife(iso3c)
} else {
  hl <- netz::get_halflife()
}

mean_retention <- optimise(
  net_loss_match_objective, lower = 1, upper = 365 * 10, half_life = hl
)$minimum

## Add in ITN input distribution, and predicted use (for checks)
interventions <- interventions |>
  dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping, "year")))) |>
  dplyr::mutate(
    itn_input_dist = netz::usage_to_model_distribution(
      usage = itn_use,
      usage_timesteps = (year - 2000) * 365 + 183,
      distribution_timesteps = (year - 2000) * 365 + 1,
      mean_retention = mean_retention
    ),
    predicted_use = netz::model_distribution_to_usage(
      usage_timesteps = (year - 2000) * 365 + 183,
      distribution = itn_input_dist,
      distribution_timesteps = (year - 2000) * 365 + 1,
      mean_retention = mean_retention
    ),
    .by = dplyr::all_of(grouping)
  ) |>
  dplyr::mutate(mean_retention = mean_retention)

# Assign an itn distribution day (currently associated with admin name length)
interventions <- interventions |>
  dplyr::mutate(
    itn_distribution_day = round((365 / 12) * (nchar(name_1) %% 12) + 1),
    .by = name_1
  )

## Link with pyrethroid resistance
interventions <- interventions |>
  dplyr::left_join(pyrethroid_resistance, by = c(grouping[grouping != "urban_rural"], "year"))

## Link with net type and net efficacy
new_net_introductions <- read.csv("vectors/new_net_introductions.csv")
interventions <- interventions |>
  dplyr::mutate(net_type = ifelse(year == 2000, "pyrethroid_only", NA)) |>
  dplyr::left_join(new_net_introductions, by = c("iso3c", "name_1", "year")) |>
  dplyr::mutate(net_type = ifelse(is.na(type), net_type, type)) |>
  dplyr::select(-type) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
  tidyr::fill(net_type) |>
  dplyr::ungroup()


## Add net efficacy | resistance and net type
net_efficacy_parameters <- read.csv("vectors/net_efficacy_adjusted.csv")
interventions <- interventions |>
  dplyr::mutate(pyrethroid_resistance = round(pyrethroid_resistance, 2)) |>
  dplyr::left_join(net_efficacy_parameters, by = c("pyrethroid_resistance", "net_type"))
# ------------------------------------------------------------------------------

# Prevalence -------------------------------------------------------------------
prevalence <- spatial |>
  dplyr::summarise(
    pfpr = weighted.mean2(pfpr, par_pf),
    pvpr = weighted.mean2(pvpr, par_pv),
    .by = dplyr::all_of(c(grouping, "year"))
  )
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
  dplyr::filter(
    year == vector_year
  ) |>
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
      dplyr::all_of(vector_columns), \(x) weighted.mean2(x, par, na.rm =)
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
  dplyr::mutate(prop = ifelse(is.na(prop) | sum(prop) == 0, 1/3, prop)) |>
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
  dplyr::summarise(
    pfpr = sum(pfpr, na.rm = TRUE),
    pvpr = sum(pvpr, na.rm = TRUE),
    .by = dplyr::all_of(grouping)
  )

pf_eir <- prevalence_summary |>
  dplyr::filter(pfpr > 0) |>
  dplyr::select(dplyr::all_of(grouping)) |>
  dplyr::mutate(
    sp = "pf",
    eir = NA
  )
pv_eir <- prevalence_summary |>
  dplyr::filter(pvpr > 0) |>
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
site_file$version = version
site_file$admin_level = grouping

site_file$sites = sites

site_file$shape = shape

site_file$cases_deaths = cases_deaths

site_file$prevalence = prevalence

site_file$interventions = interventions

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
  fourier_prediction = seasonal_curve
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


