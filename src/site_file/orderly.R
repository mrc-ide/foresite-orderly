# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Site file",
  long = "Processes and formats data into the site file configuration"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "BFA",
  admin_level = 1,
  urban_rural = TRUE
)

orderly2::orderly_resource(
  files = "site_file_utils.R"
)

orderly2::orderly_dependency(
  name = "spatial",
  query = "latest(parameter:version_name == this:version_name && parameter:iso3c ==  this:iso3c)",
  files = c("spatial.rds")
)

orderly2::orderly_dependency(
  name = "population",
  query = "latest(parameter:version_name == this:version_name && parameter:iso3c ==  this:iso3c)",
  files = c("population.rds", "population_age.rds")
)

orderly2::orderly_artefact(
  description = "Site file",
  files = "site.rds"
)
# ------------------------------------------------------------------------------

# Load inputs ------------------------------------------------------------------
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

# Interventions ----------------------------------------------------------------
# TODO: RTSS cov (historical) - in spatial
# TODO: R21 cov (0) - in spatial
interventions <- spatial |>
  dplyr::summarise(
    tx_cov = weighted.mean(tx_cov, par),
    itn_use = weighted.mean(itn_use, par),
    irs_cov = weighted.mean(irs_cov, par),
    r21_cov = 0,
    dplyr::across(dplyr::contains("smc"), \(x) weighted.mean(x, par)),
    .by = dplyr::all_of(c(grouping, "year"))
  )

## Overwrite SMC, as we cannot currently use the new MAP estimates widely
smc_overwrite <- interventions
smc_overwrite$smc <- ifelse(rowSums(interventions[,paste0("smc_", 1:12)]) > 0, 1, 0)
smc_overwrite <- smc_overwrite |>
  dplyr::summarise(
    smc_cov = ifelse(mean(smc) > 0.5, 0.9, 0),
    .by = dplyr::all_of(c(grouping[1:3], "year"))
  )
interventions <- interventions |>
  dplyr::select(-dplyr::contains("smc")) |>
  dplyr::left_join(
    smc_overwrite,
    by = c(grouping[1:3], "year")
  )

## ITN half-life to mean rentention conversion
## Match our exponential mean retention as closely as possible to the MAP 
## function with given half life:
hl_data <- netz::get_halflife_data()
if(iso3c %in% hl_data$iso3){
  hl <- hl_data |>
    dplyr::filter(iso3 == {{iso3c}}) |>
    dplyr::pull(half_life)
} else {
  hl <- median(hl_data$half_life)
}

mean_retention <- optimise(
  net_loss_match_objective, lower = 1, upper = 365 * 10, half_life = hl
)$minimum

## Add in ITN input distribution, and predicted use (for checks)
interventions <- interventions |>
  dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping, "year")))) |>
  dplyr::mutate(
    itn_input_dist = netz::fit_usage_sequential(
      target_usage = itn_use,
      target_usage_timesteps = (year - 2000) * 365 + 183,
      distribution_timesteps = (year - 2000) * 365 + 1,
      mean_retention = mean_retention
    ),
    predicted_use = netz::population_usage_t(
      timesteps = (year - 2000) * 365 + 183,
      distribution = itn_input_dist,
      distribution_timesteps = (year - 2000) * 365 + 1,
      mean_retention = mean_retention
    ),
    .by = dplyr::all_of(grouping)
  ) |>
  dplyr::mutate(mean_retention = mean_retention)

# ------------------------------------------------------------------------------

# Prevalence -------------------------------------------------------------------
prevalence <- spatial |>
  dplyr::summarise(
    pfpr = weighted.mean(pfpr, par_pf),
    pvpr = weighted.mean(pvpr, par_pv),
    par_pv = sum(par_pv),
    pv = sum(pvpr),
    .by = dplyr::all_of(c(grouping, "year"))
  )
# ------------------------------------------------------------------------------

# Seasonality ------------------------------------------------------------------
rainfall <- spatial |>
  dplyr::summarise(
    dplyr::across(paste0("rainfall_", 1:12), \(x) weighted.mean(x, w = par)),
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

seasonality_plot <- ggplot2::ggplot() +
  ggplot2::geom_jitter(data = rainfall, ggplot2::aes(x = t, y = rainfall, colour = name_1, group = urban_rural), alpha = 0.2) +
  ggplot2::geom_line(data = seasonal_curve, ggplot2::aes(x = t, y = profile, colour = name_1, group = urban_rural)) +
  ggplot2::facet_wrap(~ name_1) +
  ggplot2::scale_x_continuous(breaks = middle_days$t, labels = middle_days$month_name) +
  ggplot2::xlab("Day of year") +
  ggplot2::ylab("Rainfall") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")
seasonality_plot
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
      vector_columns, \(x) ifelse(is.na(x), 0, x)
    )
  ) |>
  dplyr::summarise(
    dplyr::across(
      vector_columns, \(x) weighted.mean(x, par, na.rm =)
    ),
    .by = dplyr::all_of(grouping)
  ) |>
  tidyr::pivot_longer(
    -grouping, names_to = "vector",  values_to = "prop"
  ) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
  dplyr::mutate(rank = rank(-prop, ties = "first")) |>
  dplyr::filter(rank <= 3) |>
  dplyr::select(-rank) |>
  dplyr::mutate(prop = prop / sum(prop)) |>
  dplyr::ungroup() |>
  dplyr::mutate(vector = stringr::str_replace(vector, "occurrence_", ""))

vector_pd <- vectors
grouping2 <- grouping[-(1:2)]
vector_pd$group <- apply(vector_pd[, grouping2], 1, function(x){paste0(x, collapse = "\n")})
vector_plot <- ggplot2::ggplot(data = vector_pd, ggplot2::aes(x = "", y = prop, fill = vector)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::xlab("") +
  ggplot2::facet_wrap(~ group, scales = "free_y") +
  ggplot2::ylab("Vector proportion") +
  ggplot2::theme_bw()
# ------------------------------------------------------------------------------



# Create the site file ---------------------------------------------------------
site_file <- list()

site_file$country = unique(spatial$country)
site_file$version = version_name
site_file$admin_level = grouping

site_file$sites = 1

site_file$spatial = 1

site_file$cases_deaths = 1

site_file$prevalence = prevalence

site_file$intervetions = 1

site_file$population = list(
  population_total = population,
  population_by_age = population_age
)

site_file$demography = 1

site_file$vectors = list(
  vector_species = vectors,
  pyrethroid_resistance = 1
)

site_file$seasonality = list(
  seasonality_parameters = seasonal_parameters,
  monthly_rainfall = rainfall,
  fourier_prediction = seasonal_curve
)

site_file$blood_disorders = 1

site_file$accessibility = 1

format(object.size(site_file), "Mb")
# ------------------------------------------------------------------------------
