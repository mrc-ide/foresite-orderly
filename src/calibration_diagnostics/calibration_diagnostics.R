# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Calibration diagnostics",
  long = "Report to assess calibration performance"
)

orderly2::orderly_parameters(
  version = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL
)

orderly2::orderly_resource(
  files = "calibration_report.qmd"
)

orderly2::orderly_dependency(
  name = "calibration",
  query = "latest(parameter:version == this:version && parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural)",
  files = c("calibrated_site.rds", "calibration_output_raw.rds")
)

orderly2::orderly_artefact(
  description = "Calibrated site with case and death scaler",
  files = "calibrated_scaled_site.rds"
)


orderly2::orderly_artefact(
  description = "Calibration plots",
  files = "calibration_plots.rds"
)

orderly2::orderly_artefact(
  description = "HTML calibration report",
  files = "calibration_report.html"
)
# ------------------------------------------------------------------------------

# Load inputs ------------------------------------------------------------------
site <- readRDS("calibrated_site.rds")
calibration_output <- readRDS("calibration_output_raw.rds")
# ------------------------------------------------------------------------------

# Format raw output ------------------------------------------------------------
# Collate diagnostic epi outputs
group_names <- site$admin_level[!site$admin_level %in% c("country", "iso3c", "year")]
breaks <- c(-Inf, 4, 14, Inf)
labels <- c("0-5", "5-15", "15+")

# TODO: Save diagnostic data as output

pop <- site$population$population_by_age |>
  dplyr::mutate(
    age_group = cut(age_lower, breaks = breaks, labels = labels, right = TRUE)
  ) |>
  dplyr::summarise(
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = c(site$admin_level, "age_group", "year")
  )

diagnostic_epi <-  lapply(calibration_output, "[[", 2) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    age_group = cut(age_lower, breaks = breaks, labels = labels, right = TRUE)
  ) |>
  dplyr::left_join(pop, by = c(site$admin_level, "age_group", "year")) |>
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
  ggplot2::facet_wrap(~ name, ncol = 2) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::annotate("rect", xmin = 2014, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
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
  ggplot2::facet_wrap(~ name, ncol = 2) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::annotate("rect", xmin = 2014, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
  ggplot2::ylab("PvPr_1_100") +
  ggplot2::theme_bw()

diagnostic_prev_national <- diagnostic_prev |>
  dplyr::summarise(
    prevalence_2_10 = weighted.mean(prevalence_2_10, par_2_10),
    prevalence_1_100 = weighted.mean(prevalence_1_100, par_1_100),
    .by = c("year", "month", "week", "day", "time", "sp")
  )
map_prev_national <- map_prev |>
  dplyr::summarise(
    pfpr = weighted.mean(pfpr, par_2_10),
    pvpr = weighted.mean(pvpr, par_1_100),
    .by = "year"
  )

national_prev_pf_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = dplyr::filter(diagnostic_prev_national, sp == "pf"),
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
  ggplot2::annotate("rect", xmin = 2014, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
  ggplot2::theme_bw()

national_prev_pv_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = dplyr::filter(diagnostic_prev_national, sp == "pv"),
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
  ggplot2::annotate("rect", xmin = 2014, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "deeppink") +
  ggplot2::theme_bw()
# ------------------------------------------------------------------------------

# Epi diagnostic plots ---------------------------------------------------------
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

case_deaths_years <- 2014:2018
scaler <- national_epi |>
  # dplyr::filter(year %in% case_deaths_years) |>
  dplyr::left_join(site$cases_deaths, by = "year") |>
  dplyr::mutate(
    case_scaler = wmr_cases / cases,
    death_scaler = wmr_deaths / deaths
    ) |>
  dplyr::select(
    year, case_scaler, death_scaler
  )
site$scaler <- scaler
saveRDS(site, "calibrated_scaled_site.rds")

cs <- mean(scaler$case_scaler[scaler$year %in% 2010:2018])
ds <- mean(scaler$death_scaler[scaler$year %in% 2010:2018])
national_rescaled_epi <- national_epi |>
  dplyr::mutate(
    rescaled_cases = cases * cs,
    rescaled_clinical = clinical * cs,
    rescaled_deaths = deaths * ds,
    rescaled_mortality = mortality * ds
  )

national_inc_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = national_epi,
    ggplot2::aes(x = year, y = clinical, colour = "Raw"),
    linewidth = 1
  ) +
  ggplot2::geom_line(
    data  = national_rescaled_epi,
    ggplot2::aes(x = year, y = rescaled_clinical, colour = "Rescaled"),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = site$cases_deaths,
    ggplot2::aes(x = year, y = wmr_incidence, colour = "WMR"),
    size = 2
  ) +
  ggplot2::scale_color_manual(values = c("Raw" = "black", "Rescaled" = "seagreen", "WMR" = "darkred"), name = "") +
  ggplot2::geom_errorbar(
    data = site$cases_deaths,
    ggplot2::aes(x = year , ymin = wmr_incidence_l, ymax = wmr_incidence_u),
    col = "darkred"
  ) +
  ggplot2::ylim(0, NA) +
  ggplot2::ylab("Clinical incidence\n(per person, per year") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw()


national_cases_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = national_epi,
    ggplot2::aes(x = year, y = cases, col = "Raw"),
    linewidth = 1
  ) +
  ggplot2::geom_line(
    data  = national_rescaled_epi,
    ggplot2::aes(x = year, y = rescaled_cases, col = "Rescaled"),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = site$cases_deaths,
    ggplot2::aes(x = year, y = wmr_cases, col = "WMR"),
    size = 2
  ) +
  ggplot2::scale_color_manual(values = c("Raw" = "black", "Rescaled" = "seagreen", "WMR" = "darkred"), name = "") +
  ggplot2::geom_errorbar(
    data = site$cases_deaths,
    ggplot2::aes(x = year , ymin = wmr_cases_l, ymax = wmr_cases_u),
    col = "darkred"
  ) +
  ggplot2::ylim(0, NA) +
  ggplot2::ylab("Clinical cases") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw()

national_mortality_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = national_epi,
    ggplot2::aes(x = year, y = mortality, colour = "Raw"),
    linewidth = 1
  ) +
  ggplot2::geom_line(
    data  = national_rescaled_epi,
    ggplot2::aes(x = year, y = rescaled_mortality, colour = "Rescaled"),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = site$cases_deaths,
    ggplot2::aes(x = year, y = wmr_mortality, colour = "WMR"),
    size = 2
  ) +
  ggplot2::scale_color_manual(values = c("Raw" = "black", "Rescaled" = "seagreen", "WMR" = "darkred"), name = "") +
  ggplot2::geom_errorbar(
    data = site$cases_deaths,
    ggplot2::aes(x = year, ymin = wmr_mortality_l, ymax = wmr_mortality_u),
    col = "darkred"
  ) +
  ggplot2::ylim(0, NA) +
  ggplot2::ylab("Mortality rate\n(per person, per year") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw()

national_deaths <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = national_epi,
    ggplot2::aes(x = year, y = deaths, colour = "Raw"),
    linewidth = 1
  ) +
  ggplot2::geom_line(
    data  = national_rescaled_epi,
    ggplot2::aes(x = year, y = rescaled_deaths, colour = "Rescaled"),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = site$cases_deaths,
    ggplot2::aes(x = year + 0.5, y = wmr_deaths, colour = "WMR"),
    col = "darkred",
    size = 2
  ) +
  ggplot2::scale_color_manual(values = c("Raw" = "black", "Rescaled" = "seagreen", "WMR" = "darkred"), name = "") +
  ggplot2::geom_errorbar(
    data = site$cases_deaths,
    ggplot2::aes(x = year + 0.5, ymin = wmr_deaths_l, ymax = wmr_deaths_u),
    col = "darkred"
  ) +
  ggplot2::ylim(0, NA) +
  ggplot2::ylab("Deaths") +
  ggplot2::theme_bw()

calibration_plots <- list(
  calibration_fit_pf = calibration_fit_pf, 
  calibration_fit_pv = calibration_fit_pv,
  national_prev_pf_plot = national_prev_pf_plot,
  national_prev_pv_plot = national_prev_pv_plot,
  national_inc_plot = national_inc_plot, 
  national_cases_plot = national_cases_plot,
  national_mortality_plot = national_mortality_plot,
  national_deaths = national_deaths
)
saveRDS(calibration_plots, "calibration_plots.rds")
# ------------------------------------------------------------------------------

# Make report ------------------------------------------------------------------
quarto::quarto_render(
  input = "calibration_report.qmd",
  execute_params = list(
    iso3c = iso3c,
    country = site$country,
    admin_level = admin_level,
    version = site$version,
    n_sites = nrow(site$sites)
  )
)
# ------------------------------------------------------------------------------
