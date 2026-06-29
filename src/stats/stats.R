# Orderly set-up ---------------------------------------------------------------
p <- orderly::orderly_parameters(
  boundary = NULL,
  admin_level = NULL,
  urban_rural = NULL,
  version = NULL
)

orderly::orderly_artefact(
  description = "Calibration summary plot",
  files = "calibration_summary.png"
)

orderly::orderly_artefact(
  description = "Calibration summary unadjusted plot",
  files = "calibration_summary_unadjusted.png"
)
# ------------------------------------------------------------------------------

# Extract outputs --------------------------------------------------------------
# Pull out diagnsotics from calibrations
prev <- list()
epi <- list()

for(iso in isos){
  print(iso)

 orderly::orderly_dependency(
    name = "calibration",
    query = "latest(parameter:boundary == this:boundary && parameter:iso3c == environment:iso && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural && parameter:version == this:version)",
    files = c("diagnostic_prev_${iso}.rds" = "diagnostic_prev.rds")
  )
 
 orderly::orderly_dependency(
   name = "calibration",
   query = "latest(parameter:boundary == this:boundary && parameter:iso3c == environment:iso && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural && parameter:version == this:version)",
   files = c("national_epi_${iso}.rds" = "national_epi.rds")
 )
  
  orderly::orderly_dependency(
    name = "calibration",
    query = "latest(parameter:boundary == this:boundary && parameter:iso3c == environment:iso && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural && parameter:version == this:version)",
    files = c("calibrated_scaled_site_${iso}.rds" = "calibrated_scaled_site.rds")
  )
  
  sitefile <- readRDS(paste0("calibrated_scaled_site_", iso, ".rds"))
  
  map <- sitefile$prevalence |>
    tidyr::pivot_longer(cols = c(pfpr, pvpr), names_to = "sp", values_to = "map_lm_prevalence") |>
    dplyr::mutate(sp = stringr::str_replace_all(string = sp, "pr", ""))
  
  groups <- names(map)[!names(map) == "map_lm_prevalence"]
  
  pp <- readRDS(paste0("diagnostic_prev_", iso, ".rds")) |>
    dplyr::summarise(
      lm_prevalence = mean(lm_prevalence), 
      time = mean(time),
      .by = dplyr::all_of(groups)
    )
  
  # Epi
  
  scaler <- sitefile$bias_correction
  cs <- mean(scaler$case_bias_correction[scaler$year %in% 2010:2024])
  ds <- mean(scaler$death_bias_correction[scaler$year %in% 2010:2024])
  national_epi <- readRDS(paste0("national_epi_", iso, ".rds")) |>
    dplyr::mutate(
      rescaled_cases = cases * cs,
      rescaled_clinical = clinical * cs,
      rescaled_deaths = deaths * ds,
      rescaled_mortality = mortality * ds
    )
  
  epi[[iso]] <- sitefile$cases_deaths |>
    dplyr::left_join(national_epi, by = "year")
  
  prev[[iso]] <- dplyr::left_join(map, pp, by = groups)
}

prev <- dplyr::bind_rows(prev)
epi <- dplyr::bind_rows(epi)
# ------------------------------------------------------------------------------

# Diagnostic plots -------------------------------------------------------------
navy   <- "#1B2A6C"
accent <- "#D55E00"   # identity line — warm contrast to navy, colourblind-safe

# 1. Prevalence ---------------------------------------------------------------
prevalence_fit <- ggplot2::ggplot(prev[prev$year >= 2010 & prev$year <= 2024, ],
                ggplot2::aes(x = map_lm_prevalence, y = lm_prevalence)) +
  ggplot2::geom_point(alpha = 0.2, size = 1.6, colour = navy, stroke = 0) +
  ggplot2::geom_abline(intercept = 0, slope = 1,
                       linetype = 2, colour = accent, linewidth = 0.8) +
  ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  ggplot2::labs(x = "MAP prevalence estimate",
                y = "Calibrated model estimate",
                title = "Model v MAP prevalence\n(2010 - 2024)") +
  site::theme_site(aspect.ratio = 1)

# 2. Incidence ----------------------------------------------------------------
incidence_fit <- ggplot2::ggplot(epi[epi$year >= 2010 & epi$year <= 2024& epi$iso3c != "GNB", ],
                ggplot2::aes(x = wmr_incidence, y = rescaled_clinical,
                             xmin = wmr_incidence_l, xmax = wmr_incidence_u)) +
  ggplot2::geom_linerange(alpha = 0.2, colour = navy) +
  ggplot2::geom_point(alpha = 0.2, size = 1.6, colour = navy, stroke = 0) +
  ggplot2::geom_abline(intercept = 0, slope = 1,
                       linetype = 2, colour = accent, linewidth = 0.8) +
  ggplot2::labs(x = "WHO incidence estimate",
                y = "Calibrated & bias adjusted model incidence",
                title = "Model v WHO incidence\n(2010 - 2024)") +
  site::theme_site(aspect.ratio = 1)

incidence_fit_unadjusted <- ggplot2::ggplot(epi[epi$year >= 2010 & epi$year <= 2024& epi$iso3c != "GNB", ],
                                 ggplot2::aes(x = wmr_incidence, y = clinical,
                                              xmin = wmr_incidence_l, xmax = wmr_incidence_u)) +
  ggplot2::geom_linerange(alpha = 0.2, colour = navy) +
  ggplot2::geom_point(alpha = 0.2, size = 1.6, colour = navy, stroke = 0) +
  ggplot2::geom_abline(intercept = 0, slope = 1,
                       linetype = 2, colour = accent, linewidth = 0.8) +
  ggplot2::labs(x = "WHO incidence estimate",
                y = "Calibrated model incidence",
                title = "Model v WHO incidence\n(2010 - 2024)") +
  site::theme_site(aspect.ratio = 1)

# 3. Mortality ----------------------------------------------------------------
mortality_fit <- ggplot2::ggplot(epi[epi$year >= 2010 & epi$year <= 2024 & epi$iso3c != "GNB", ],
                ggplot2::aes(x = wmr_mortality, y = rescaled_mortality,
                             xmin = wmr_mortality_l, xmax = wmr_mortality_u)) +
  ggplot2::geom_linerange(alpha = 0.2, colour = navy) +
  ggplot2::geom_point(alpha = 0.2, size = 1.6, colour = navy, stroke = 0) +
  ggplot2::geom_abline(intercept = 0, slope = 1,
                       linetype = 2, colour = accent, linewidth = 0.8) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  ggplot2::labs(x = "WHO mortality estimate",
                y = "Calibrated & bias adjusted model mortality",
                title = "Model v WHO mortality\n(2010 - 2024)") +
  site::theme_site(aspect.ratio = 1)

mortality_fit_unadjusted <- ggplot2::ggplot(epi[epi$year >= 2010 & epi$year <= 2024 & epi$iso3c != "GNB", ],
                                 ggplot2::aes(x = wmr_mortality, y = mortality,
                                              xmin = wmr_mortality_l, xmax = wmr_mortality_u)) +
  ggplot2::geom_linerange(alpha = 0.2, colour = navy) +
  ggplot2::geom_point(alpha = 0.2, size = 1.6, colour = navy, stroke = 0) +
  ggplot2::geom_abline(intercept = 0, slope = 1,
                       linetype = 2, colour = accent, linewidth = 0.8) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  ggplot2::labs(x = "WHO mortality estimate",
                y = "Calibrated model mortality",
                title = "Model v WHO mortality\n(2010 - 2024)") +
  site::theme_site(aspect.ratio = 1)

library(patchwork)
calibration_summary <- prevalence_fit + incidence_fit + mortality_fit
calibration_summary_unadjusted <- prevalence_fit + incidence_fit_unadjusted + mortality_fit_unadjusted

ggplot2::ggsave("calibration_summary.png", calibration_summary, height = 5, width = 12)
ggplot2::ggsave("calibration_summary_unadjusted.png", calibration_summary_unadjusted, height = 5, width = 12)