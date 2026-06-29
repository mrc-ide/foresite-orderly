# Orderly set-up ---------------------------------------------------------------
orderly::orderly_description(
  display = "Site file diagnostics",
  long = "Take a daw site file and prepares a diagnostic report"
)

p <- orderly::orderly_parameters(
  boundary = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL,
  version = NULL,
  calibration = NULL
)

orderly::orderly_shared_resource("utils.R")

orderly::orderly_resource(
  files = c("malariaverse_wide.png")
)

if(!p$calibration){
  orderly::orderly_dependency(
    name = "site_file",
    query = "latest(parameter:boundary == this:boundary && parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural && parameter:version == this:version)",
    files = c("site.rds")
  )
}

orderly::orderly_artefact(
  description = "PDF diagnostic report",
  files = "diagnostic_report.pdf"
)

if(p$calibration){
  orderly::orderly_dependency(
    name = "calibration",
    query = "latest(parameter:boundary == this:boundary && parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural && parameter:version == this:version)",
    files = c("calibrated_scaled_site.rds", "diagnostic_prev.rds", "national_epi.rds")
  )
}

# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(sf)
library(qpdf)
source("utils.R")


# Load inputs ------------------------------------------------------------------
if(p$calibration){
  sitefile <- readRDS("calibrated_scaled_site.rds")
} else {
  sitefile <- readRDS("site.rds")
}
# Simplify spatial boundaries to increase mapping speed
sitefile$shape <- lapply(sitefile$shape, sf::st_simplify, dTolerance = 3000)
# Ordering for site reports
sitefile$sites <- sitefile$sites |>
  dplyr::arrange(dplyr::across(names(sitefile$sites)))

national_epi <- NULL
if(p$calibration){
  scaler <- sitefile$bias_correction
  cs <- mean(scaler$case_bias_correction[scaler$year %in% 2010:2024])
  ds <- mean(scaler$death_bias_correction[scaler$year %in% 2010:2024])
  national_epi <- readRDS("national_epi.rds") |>
    dplyr::mutate(
      rescaled_cases = cases * cs,
      rescaled_clinical = clinical * cs,
      rescaled_deaths = deaths * ds,
      rescaled_mortality = mortality * ds
    )
}

diagnostic_prev <- NULL
if(p$calibration){
  diagnostic_prev <- readRDS("diagnostic_prev.rds")
}
# ------------------------------------------------------------------------------

# Cases and deaths -------------------------------------------------------------
burden <- plot_burden(sitefile$cases_deaths, "Burden", national_epi)
# ------------------------------------------------------------------------------

# Maps -------------------------------------------------------------------------
pfpr_map <- plot_map(
  sitefile$prevalence,
  column_name = "pfpr",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "Pf parasite prevalence",
  viridis_option = "A",
  lims = c(0, NA)
)
pvpr_map <- plot_map(
  sitefile$prevalence,
  column_name = "pvpr",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "Pv parasite prevalence",
  viridis_option = "A",
  lims = c(0, NA)
)
tx_cov_map <- plot_map(
  sitefile$interventions$treatment$implementation,
  column_name = "tx_cov",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "Treatment coverage"
)
itn_use_map <- plot_map(
  sitefile$interventions$itn$use,
  column_name = "itn_use",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "ITN usage"
)
irs_cov_map <- plot_map(
  sitefile$interventions$irs$implementation,
  column_name = "irs_cov",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "IRS coverage"
)
smc_cov_map <- plot_map(
  sitefile$interventions$smc$implementation,
  column_name = "smc_cov",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "SMC coverage"
)
lsm_cov_map <- plot_map(
  sitefile$interventions$lsm$implementation,
  column_name = "lsm_cov",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "LSM coverage"
)
r21_cov_map <- plot_map(
  sitefile$interventions$vaccine$implementation,
  column_name = "r21_primary_cov",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "R21 vaccine coverage"
)
rtss_cov_map <- plot_map(
  sitefile$interventions$vaccine$implementation,
  column_name = "rtss_primary_cov",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "RTS,S vaccine coverage"
)
pmc_cov_map <- plot_map(
  data = sitefile$interventions$pmc$implementation,
  column_name = "pmc_cov",
  population = sitefile$population$population_total,
  shape = sitefile$shape[[1]],
  title = "PMC coverage"
)


maps1 <- pfpr_map /
  pvpr_map /
  tx_cov_map /
  itn_use_map /
  irs_cov_map

maps2 <- smc_cov_map /
  pmc_cov_map /
  rtss_cov_map /
  r21_cov_map /
  lsm_cov_map
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
pop_dat <- sitefile$population$population_by_age |>
  summarise(
    pop = sum(pop),
    par = sum(par),
    .by = c("year", "urban_rural", "age_lower", "age_upper")
  ) |>
  filter(year <= 2050)
pop_plot <- site::plot_age_distribution_stacked(pop_dat, title = "Population age distribution")

urban_rural_pop_dat <- sitefile$population$population_by_age |>
  summarise(
    pop = sum(pop),
    par = sum(par),
    .by = c("year", "urban_rural")
  ) |>
  filter(year <= 2050)
urban_rural_plot <- plot_urban_rural(pop_dat, title = "Population urbanicity")

pops <- (pop_plot / urban_rural_plot)
# ------------------------------------------------------------------------------

# Accessibility ----------------------------------------------------------------
accessibility_map <- plot_accessibility(sitefile$accessibility, sitefile$population$population_total)
# ------------------------------------------------------------------------------

# Blood disorders --------------------------------------------------------------
blood_disorder_map <- plot_blood_disorders(sitefile$blood_disorders, sitefile$population$population_total)
# ------------------------------------------------------------------------------

# Titles -----------------------------------------------------------------------
title <- sitefile$sites$country[1]
subtitle <- paste(
  "Version: ", p$version, "\n",
  "Admin level: ", p$admin_level, "\n",
  "Urban rural split: ", p$urban_rural, "\n",
  "Calibrated: ", p$calibration
)

logo <- png::readPNG("malariaverse_wide.png")
logo_grob <- grid::rasterGrob(logo, interpolate = TRUE)

title_p <- ggplot() +
  annotate("text", x = 0.5, y = 0.75, label = title, size = 12, fontface = "bold", colour = "navy") +
  annotation_custom(logo_grob, xmin = 0.05, xmax = 0.95, ymin = 0.05, ymax = 0.35) +
  annotate("text", x = 0.5, y = 0.5, label = subtitle, size = 6, colour = "navy", alpha = 0.6) +
  theme_void()  +
  xlim(0, 1) + ylim(0, 1)


single_site_title_p <- ggplot() +
  annotate("text", x = 0.5, y = 0.6, label = "Site reports", size = 12, fontface = "bold", colour = "navy") +
  theme_void() +
  xlim(0, 1) + ylim(0, 1)

# ------------------------------------------------------------------------------

# Combination ------------------------------------------------------------------
title_file <- tempfile(fileext = ".pdf")
burden_file <- tempfile(fileext = ".pdf")
maps1_file <- tempfile(fileext = ".pdf")
maps2_file <- tempfile(fileext = ".pdf")
pops_file <- tempfile(fileext = ".pdf")
accessibility_file <- tempfile(fileext = ".pdf")
blood_file <- tempfile(fileext = ".pdf")
single_site_title_file <- tempfile(fileext = ".pdf")

ggsave(title_file, title_p, width = 8.27, height = 8.27)
ggsave(burden_file, burden, height = 8, width = 15)
ggsave(maps1_file, maps1, width = 40, height = 15)
ggsave(maps2_file, maps2, width = 40, height = 15)
ggsave(pops_file, pops, width = 12, height = 8)
ggsave(accessibility_file, accessibility_map, width = 12, height = 8)
ggsave(blood_file, blood_disorder_map, width = 12, height = 8)
ggsave(single_site_title_file, single_site_title_p, width = 8.27, height = 8.27)

page_files <- c(title_file, burden_file, maps1_file, maps2_file, pops_file, accessibility_file, blood_file, single_site_title_file)

for(s in 1:nrow(sitefile$sites)){
  site <- site::subset_site(sitefile, sitefile$sites[s, ])
  site$interventions$itn$implementation$itn_input_dist <-
    site::site_usage_to_model_distribution(
      usage = site$interventions$itn$use$itn_use,
      usage_year = site$interventions$itn$use$year,
      usage_day_of_year = site$interventions$itn$use$usage_day_of_year,
      distribution_year = site$interventions$itn$implementation$year,
      distribution_day_of_year = site$interventions$itn$implementation$distribution_day_of_year,
      distribution_lower = site$interventions$itn$implementation$distribution_lower,
      distribution_upper = site$interventions$itn$implementation$distribution_upper,
      net_loss_function = netz::net_loss_map,
      half_life = site$interventions$itn$retention_half_life
    )
  diag <- plot_calibrated_site_diagnostic(site, diagnostic_prev = diagnostic_prev)
  site_file <- tempfile(fileext = ".pdf")
  ggsave(site_file, diag, height = 10, width = 12, scale = 1.2)
  page_files <- c(page_files, site_file)
}

pdf_combine(page_files, "diagnostic_report.pdf")
unlink(page_files)
