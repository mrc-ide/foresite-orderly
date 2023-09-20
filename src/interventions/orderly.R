# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Create site file interventions ",
  long = "Collates all available interventions data"
)

orderly2::orderly_parameters(
  iso3c = "SDN",
  admin_level = 1
)

orderly2::orderly_dependency(
  name = "process_data",
  query = "latest(parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level)",
  files = c(
    "gadm_df.rds",
    "proportion_act.rds",
    "proportion_public.rds"
  )
)

orderly2::orderly_dependency(
  name = "aggregation",
  query = "latest(parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level)",
  files = c(
    "aggregated_interventions.rds",
    "aggregated_population.rds",
    "aggregated_prevalence.rds"
  )
)

orderly2::orderly_artefact(
  description = "site file interventions", 
  files = "interventions.rds"
)
# ------------------------------------------------------------------------------

gadm_df <- readRDS("gadm_df.rds")
aggregated_interventions <- readRDS("aggregated_interventions.rds")

# ITNs (Outside of SSA) --------------------------------------------------------
if(gadm_df$continent[1] != "Africa"){
  itn_baseline_prevalence_threshold <- 0.01
  nominal_itn_coverage <- 0.1
  
  targeted_itns <- readRDS("aggregated_prevalence.rds") |>
    dplyr::filter(year == 2000, pfpr > itn_baseline_prevalence_threshold | pvpr > itn_baseline_prevalence_threshold) |>
    dplyr::select(ID, urban_rural) |>
    dplyr::mutate(bednet_use = 0.1)
  
  aggregated_interventions <- aggregated_interventions |>
    dplyr::select(-bednet_use) |>
    dplyr::left_join(targeted_itns,by = c("ID", "urban_rural"))
  
  # TODO:
  ## Take the average nets distributed over last 3 years.
  ## Estimates the equilibrium use | dist and par
  ## Rescale bednet use to match 
  ## Linearly interpolate use back to year 2000
  
}
# ------------------------------------------------------------------------------

# Create interventions data.frame ----------------------------------------------



interventions <- gadm_df |>
  dplyr::left_join(aggregated_interventions, by = c("id" = "ID"))

# TODO:
# MASS campaign
# Random distribution throughout year?

# ------------------------------------------------------------------------------