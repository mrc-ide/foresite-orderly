# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Calibration",
  long = "Calibrates baseline EIR to match prevalence"
)

orderly2::orderly_parameters(
  boundary = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL,
  version = NULL
)

orderly2::orderly_resource(
  files = "calibration_utils.R"
)

orderly2::orderly_dependency(
  name = "site_file",
  query = "latest(parameter:boundary == this:boundary && parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural && parameter:version == this:version)",
  files = c("site.rds")
)

orderly2::orderly_artefact(
  description = "Raw list calibration output",
  files = "calibration_output_raw.rds"
)

orderly2::orderly_artefact(
  description = "Raw list calibration output",
  files = "calibration_output_raw.rds"
)

orderly2::orderly_artefact(
  description = "Diagnostics rates",
  files = "diagnostic_epi.rds"
)

orderly2::orderly_artefact(
  description = "Diagnostics prevalence",
  files = "diagnostic_prev.rds"
)

orderly2::orderly_artefact(
  description = "Diagnostics rates aggregated",
  files = "national_epi.rds"
)

orderly2::orderly_artefact(
  description = "Calibrated site",
  files = "calibrated_scaled_site.rds"
)
# ------------------------------------------------------------------------------

# Initial set up ---------------------------------------------------------------
library(sf)
site <- readRDS("site.rds")
source("calibration_utils.R")
# ------------------------------------------------------------------------------

# Calibration ------------------------------------------------------------------
parallel <- TRUE
human_population <- c(5000, 10000, 100000)
diagnostic_burnin <- 20 
max_attempts <- 30

# Split out individual jobs
eirs <- site$eir
eirs <- split(eirs, 1:nrow(eirs))

if(parallel){
  cores <- Sys.getenv("CCP_NUMCPUS")
  cluster <- parallel::makeCluster(as.integer(cores))
  invisible(parallel::clusterCall(cluster, ".libPaths", .libPaths()))
  parallel::clusterCall(cluster, function() {
    library(sf)
    library(site)
    library(postie)
    library(cali)
    library(knitr)
    library(dplyr)
    library(ggplot2)
    library(quarto)
    source("calibration_utils.R")
    TRUE
  })
  
  calibration_output <- parallel::parLapply(
    cl = cluster,
    X = eirs,
    fun = calibrate_site,
    site = site,
    human_population = human_population,
    diagnostic_burnin = diagnostic_burnin,
    max_attempts = max_attempts
  )
  parallel::stopCluster(cl = cluster)
} else {
  calibration_output <- lapply(
    X = eirs,
    FUN = calibrate_site,
    site = site,
    human_population = human_population,
    diagnostic_burnin = diagnostic_burnin,
    max_attempts = max_attempts
  )
}

saveRDS(calibration_output, "calibration_output_raw.rds")
# ------------------------------------------------------------------------------

# Collate EIR ------------------------------------------------------------------
eir_estimates <- 
  lapply(calibration_output, "[[", 1) |>
  dplyr::bind_rows()

site$eir <- eir_estimates
#saveRDS(site, "calibrated_site.rds")
# ------------------------------------------------------------------------------

# Format raw output ------------------------------------------------------------
# Collate diagnostic epi outputs
group_names <- site$metadata$admin_level[!site$metadata$admin_level %in% c("country", "iso3c", "year")]
breaks <- c(-Inf, 4, 14, Inf)
labels <- c("0-5", "5-15", "15+")

pop <- site$population$population_by_age |>
  dplyr::mutate(
    age_group = cut(age_lower, breaks = breaks, labels = labels, right = TRUE)
  ) |>
  dplyr::summarise(
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = c(site$metadata$admin_level, "age_group", "year")
  )

diagnostic_epi <-  lapply(calibration_output, "[[", 2) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    age_group = cut(age_lower, breaks = breaks, labels = labels, right = TRUE)
  ) |>
  dplyr::left_join(pop, by = c(site$metadata$admin_level, "age_group", "year")) |>
  dplyr::mutate(
    severe = ifelse(is.na(severe), 0, severe),
    mortality = ifelse(is.na(mortality), 0, mortality)   
  ) |>
  dplyr::mutate(
    par = ifelse(sp == "pf", par_pf, par_pv)
  ) |>
  dplyr::mutate(
    cases = clinical * par,
    deaths = mortality * par
  ) 

diagnostic_epi$name <- apply(diagnostic_epi[,group_names], 1, paste, collapse = " | ")
saveRDS(diagnostic_epi, "diagnostic_epi.rds")

# Collate diagnostic prevalence outputs
prev_pop <- site$population$population_by_age |>
  dplyr::summarise(
    par_2_10 = sum(par[age_lower >= 2 & age_lower < 10]),
    par_1_100 = sum(par[age_lower >= 1 & age_lower < 100]),
    .by = c(site$metadata$admin_level, "year")
  )

diagnostic_prev <-  lapply(calibration_output, "[[", 3) |>
  dplyr::bind_rows() |>
  dplyr::left_join(prev_pop, by = c(site$metadata$admin_level, "year"))


diagnostic_prev$name <- apply(diagnostic_prev[,group_names, drop = FALSE], 1, paste, collapse = " | ")
saveRDS(diagnostic_prev, "diagnostic_prev.rds")

national_epi <- diagnostic_epi |>
  dplyr::summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    par = mean(par),
    .by = dplyr::all_of(c(group_names, "age_group", "year", "time"))
  ) |>
  dplyr::summarise(
    clinical = weighted.mean(cases / par, par) * 365,
    mortality = weighted.mean(deaths / par, par) * 365,
    cases = sum(cases),
    deaths = sum(deaths),
    par = sum(par) / 365,
    .by = "year"
  )
saveRDS(national_epi, "national_epi.rds")
# ------------------------------------------------------------------------------

# Bias corrections -------------------------------------------------------------
bias_correction <- national_epi |>
  dplyr::left_join(site$cases_deaths, by = "year") |>
  dplyr::mutate(
    case_bias_correction = wmr_cases / cases,
    death_bias_correction = wmr_deaths / deaths
  ) |>
  dplyr::select(
    year, case_bias_correction, death_bias_correction
  )
site$bias_correction <- bias_correction

saveRDS(site, "calibrated_scaled_site.rds")
# ------------------------------------------------------------------------------