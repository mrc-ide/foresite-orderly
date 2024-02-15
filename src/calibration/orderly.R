# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Calibration",
  long = "Calibrates baseline EIR to match prevalence"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "BFA",
  admin_level = 1,
  urban_rural = TRUE
)

orderly2::orderly_resource(
  files = "calibration_utils.R"
)

orderly2::orderly_dependency(
  name = "site_file",
  query = "latest(parameter:version_name == this:version_name && parameter:iso3c ==  this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural)",
  files = c("site.rds")
)
# ------------------------------------------------------------------------------

# Initial set up ---------------------------------------------------------------
library(sf)
site <- readRDS("site.rds")
source("calibration_utils.R")
# ------------------------------------------------------------------------------

# Calibration ------------------------------------------------------------------
parallel <- FALSE
# Split out individual jobs
eirs <- site$eir[10,]
eirs <- split(eirs, 1:nrow(eirs))

if(parallel){
  cores <- 1
  cluster <- parallel::makeCluster(cores)
  lp <- parallel::clusterEvalQ(cluster, library("sf"))
  lp <- parallel::clusterEvalQ(cluster, source("calibration_utils.R"))
  
  calibration_output <- parallel::parLapply(
    cl = cluster,
    X = eirs,
    fun = calibrate,
    site = site,
    human_population = c(1000, 10000, 100000),
    burnin = 0,
    max_attempts = 10,
    eir_limits = c(0, 1000)
  )
  parallel::stopCluster(cl = cluster)
} else {
  calibration_output <- lapply(
    X = eirs,
    FUN = calibrate,
    site = site,
    human_population = c(1000, 10000, 100000),
    burnin = 0,
    max_attempts = 10,
    eir_limits = c(0, 1000)
  )
}

# Collate EIR
eir_estimates <- 
  lapply(calibration_output, "[[", 1) |>
  dplyr::bind_rows()

# Collate diagnostic epi outputs
group_names <- site$admin_level[!site$admin_level %in% c("country", "iso3c", "year")]
breaks <- c(-Inf, 4, 14, Inf)
labels <- c("0-5", "5-15", "15+")

pop <- site$population$population_by_age |>
  dplyr::mutate(
    age_group = cut(age_lower, breaks = breaks, labels = labels, right = TRUE)
  ) |>
  dplyr::summarise(
    par = sum(par),
    .by = c(site$admin_level, "age_group", "year")
  )

diagnostic_epi <-  lapply(calibration_output, "[[", 2) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    age_group = cut(age_lower, breaks = breaks, labels = labels, right = TRUE)
  ) |>
  dplyr::left_join(pop, by = c(site$admin_level, "age_group", "year")) |>
  dplyr::mutate(
    cases = clinical * par,
    deaths = mortality * par
  )

diagnostic_epi$name <- apply(diagnostic_epi[,group_names], 1, paste, collapse = " | ")

# Collate diagnostic prevalence outputs
prev_pop <- site$population$population_by_age |>
  dplyr::summarise(
    par_2_10 = sum(par[age_lower >= 2 & age_lower < 10]),
    par_1_100 = sum(par[age_lower >= 1 & age_lower < 100]),
    .by = c(site$admin_level, "year")
  )

diagnostic_prev <-  lapply(calibration_output, "[[", 3) |>
  dplyr::bind_rows() |>
  dplyr::left_join(prev_pop, by = c(site$admin_level, "year"))


diagnostic_prev$name <- apply(diagnostic_prev[,group_names], 1, paste, collapse = " | ")

# TODO: update site EIR
# site$eir <- eir_estimates
# ------------------------------------------------------------------------------

# Prevalence diagnostic plots --------------------------------------------------

## MAP prevalence estimates
map_prev <- site$prevalence |>
  dplyr::left_join(prev_pop) 
map_prev$name <- apply(map_prev[,group_names], 1, paste, collapse = " | ")

## Subnational prevalence - Pf
calibration_fit_pf <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = dplyr::filter(diagnostic_prev, sp == "pf"),
    ggplot2::aes(x = time, y = prevalence_2_10),
    col = "grey50"
  ) +
  ggplot2::geom_point(
    data = map_prev,
    ggplot2::aes(x = year + 0.5, y = pfpr),
    col = "darkred"
  ) +
  ggplot2::facet_wrap(~ name, ncol = 4) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::annotate("rect", xmin = 2015, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
  ggplot2::ylab("PfPr_2_10") +
  ggplot2::theme_bw()

## Subnational prevalence - Pv
calibration_fit_pv <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = dplyr::filter(diagnostic_prev, sp == "pv"),
    ggplot2::aes(x = time, y = prevalence_1_100),
    col = "grey50"
  ) +
  ggplot2::geom_point(
    data = map_prev,
    ggplot2::aes(x = year + 0.5, y = pvpr),
    col = "darkred"
  ) +
  ggplot2::facet_wrap(~ name, ncol = 4) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::annotate("rect", xmin = 2015, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
  ggplot2::ylab("PvPr_1_100") +
  ggplot2::theme_bw()

diagnostic_prev_national <- diagnostic_prev |>
  dplyr::summarise(
    prevalence_2_10 = weighted.mean(prevalence_2_10, par_2_10),
    prevalence_1_100 = weighted.mean(prevalence_1_100, par_1_100),
    .by = c("year", "month", "week", "day", "time")
  )
map_prev_national <- map_prev |>
  dplyr::summarise(
    pfpr = weighted.mean(pfpr, par_2_10),
    pvpr = weighted.mean(pvpr, par_1_100),
    .by = "year"
  )

national_prev_pf_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = dplyr::filter(diagnostic_prev, sp == "pf"),
    ggplot2::aes(x = time, y = prevalence_2_10),
    col = "grey50"
  ) +
  ggplot2::geom_point(
    data = map_prev_national,
    ggplot2::aes(x = year + 0.5, y = pfpr),
    col = "darkred",
    size = 2
  ) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Pf prevalence 2-10") +
  ggplot2::annotate("rect", xmin = 2015, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
  ggplot2::theme_bw()

national_prev_pv_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = dplyr::filter(diagnostic_prev, sp == "pv"),
    ggplot2::aes(x = time, y = prevalence_1_100),
    col = "grey50"
  ) +
  ggplot2::geom_point(
    data = map_prev_national,
    ggplot2::aes(x = year + 0.5, y = pvpr),
    col = "darkred",
    size = 2
  ) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Pv prevalence 1-100") +
  ggplot2::annotate("rect", xmin = 2015, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
  ggplot2::theme_bw()
# ------------------------------------------------------------------------------

# Epi diagnostic plots ---------------------------------------------------------


# ------------------------------------------------------------------------------