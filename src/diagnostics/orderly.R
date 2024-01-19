# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Site file diagnostics",
  long = "Take a daw site file and prepares a diagnostic report"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "BFA",
  admin_level = 1,
  urban_rural = TRUE
)

orderly2::orderly_resource(
  files = c("diagnostic_report.qmd")
)

orderly2::orderly_dependency(
  name = "site_file",
  query = "latest(parameter:version_name == this:version_name && parameter:iso3c ==  this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural)",
  files = c("site.rds")
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = c("un_wup.rds", "un_wpp.rds")
)

orderly2::orderly_artefact(
  description = "Raw list of diagnostic plots",
  files = "diagnostic_plots.rds"
)

orderly2::orderly_artefact(
  description = "HTML diagnostic report",
  files = "diagnostic_report.html"
)
# ------------------------------------------------------------------------------

# Load inputs ------------------------------------------------------------------
site <- readRDS("site.rds")

un_wpp <- readRDS("un_wpp.rds") |>
  dplyr::filter(
    iso3c == {{iso3c}},
    year >= 2000
  )

un_wup <- readRDS("un_wup.rds") |>
  dplyr::filter(
    iso3c == {{iso3c}},
    year >= 2000
  )
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------
p <- site$population$population_total
pa <- site$population$population_by_age

# Country level summary and comparison with UN numbers
population_country <- p |>
  dplyr::summarise(
    pop = sum(pop),
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = dplyr::all_of(c("iso3c", "year"))
  ) |>
  dplyr::mutate(type = "site file")

un_pop <- un_wpp |>
  dplyr::summarise(
    pop = sum(population),
    .by = c("iso3c", "year")
  ) |>
  dplyr::mutate(type = "UN")

country_population_pd <- 
  dplyr::bind_rows(population_country, un_pop)

country_population_plot <- ggplot2::ggplot(
  data = country_population_pd,
  ggplot2::aes(x = year, y = pop / 1e6, col = type, lty = type)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_colour_manual(values = c("black", "deeppink"), name = "Source") +
  ggplot2::scale_linetype_manual(values = c(1, 3), name = "Source") +
  ggplot2::ylab("Population (millions)") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle(paste0(iso3c, ": Country Population"))

# Comparison of Pop and Par at country level
population_at_risk_country_pd <- population_country |>
  dplyr::select(-type) |>
  tidyr::pivot_longer(-c(iso3c, year), names_to = "type", values_to = "y") |>
  dplyr::mutate(
    type = dplyr::case_when(
      type == "pop" ~ "Population",
      type == "par" ~ "Population at risk",
      type == "par_pf" ~ "Population at risk: Pf",
      type == "par_pv" ~ "Population at risk: Pv"
    )
  )

country_population_at_risk_plot <- ggplot2::ggplot(
  data = population_at_risk_country_pd,
  ggplot2::aes(x = year, y = y / 1e6, col = type, lty = type)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::ylab("Population (millions)") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::ggtitle(paste0(iso3c, ": Country Populations at risk"))

country_urban_rural_pd <- p |>
  dplyr::summarise(
    pop = sum(pop),
    .by = dplyr::all_of(c("iso3c", "urban_rural", "year"))
  )

country_population_urban_rural_plot <- ggplot2::ggplot(
  data = country_urban_rural_pd,
  ggplot2::aes(x = year, y = pop / 1e6, col = urban_rural)
) +
  ggplot2::geom_line() +
  ggplot2::ylab("Population (millions)") +
  ggplot2::xlab("Year") +
  ggplot2::scale_fill_manual(values = c("forestgreen", "steelblue"), name = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::ggtitle(paste0(iso3c, ": Country Populations urban and rural"))

country_prop_urban <- country_urban_rural_pd |>
  dplyr::mutate(
    proportion_urban = pop / sum(pop),
    .by = c("iso3c", "year")
  ) |>
  dplyr::select(-pop) |>
  dplyr::filter(
    urban_rural == "urban"
  ) |>
  dplyr::mutate(
    type = "site file"
  )

un_prop_urban <- un_wup |>
  dplyr::mutate(type = "UN")

country_prop_urban_pd <- dplyr::bind_rows(
  country_prop_urban,
  un_prop_urban
)

country_prop_urban_plot <- ggplot2::ggplot(
  data = country_prop_urban_pd,
  ggplot2::aes(x = year, y = proportion_urban, col = type, lty = type)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_colour_manual(values = c("black", "deeppink"), name = "Source") +
  ggplot2::scale_linetype_manual(values = c(1, 2), name = "Source") +
  ggplot2::ylab("Proportion urban") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle(paste0(iso3c, ": Country proportion of population that is urban"))


area_name_cols <- site$admin_level[!site$admin_level %in% c("country", "urban_rural")]
area_urban_rural_pd <- p |>
  dplyr::summarise(
    pop = sum(pop),
    .by = dplyr::all_of(c(site$admin_level, "year"))
  )

area_urban_rural_pd$name <- apply(area_urban_rural_pd[,area_name_cols], 1, paste, collapse = " | ")

area_urban_rural_population_plot <- ggplot2::ggplot(
  data = area_urban_rural_pd, ggplot2::aes(x = year, y = pop / 1e6, col = urban_rural)
) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ name, ncol = 2, scales = "free") +
  ggplot2::ylab("Population (millions)") +
  ggplot2::xlab("Year") +
  ggplot2::scale_fill_manual(values = c("forestgreen", "steelblue"), name = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(), 
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Area Populations urban and rural"))

age_dist_site <- pa |>
  dplyr::summarise(pop = sum(pop), .by = c("iso3c", "year", "age_lower", "age_upper")) |>
  dplyr::mutate(population_proportion = pop / sum(pop), .by = c("iso3c", "year")) |>
  dplyr::select(year, age_lower, population_proportion) |>
  dplyr::mutate(source = "site file")

age_dist_un <- un_wpp |>
  dplyr::select(year, age_lower, population_proportion) |>
  dplyr::mutate(source = "UN")

age_dist_pd <- dplyr::bind_rows(age_dist_site, age_dist_un)

age_dist_plot <- ggplot2::ggplot(data = age_dist_pd,
                                 ggplot2::aes(
                                   x = age_lower,
                                   y = population_proportion,
                                   fill = source,
                                   col = source,
                                   alpha = source
                                 )) +
  ggplot2::geom_bar(stat = "identity", position = "identity") +
  ggplot2::scale_fill_manual(values = c("darkblue", "white"), name = "Source") +
  ggplot2::scale_colour_manual(values = c("black", "deeppink"), name = "Source") +
  ggplot2::scale_alpha_manual(values = c(1, 0), name = "Source") +
  ggplot2::xlab("Age") +
  ggplot2::ylab("Proportion of the population") +
  ggplot2::facet_wrap(~ year, ncol = 2, scales = "free_x") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    panel.spacing.y = ggplot2::unit(2, "lines")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Age distribution"))
# ------------------------------------------------------------------------------

# Geographic areas -------------------------------------------------------------
levels <- length(site$shape)
facets <- site$shape[[1]] |>
  dplyr::rename(name = paste0("name_", levels - 1))

geom_graphic_areas_plot <-  geom_graphic_areas_plot <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = facets, ggplot2::aes(geometry = geom), fill = "deeppink", col = "grey50")

border_cols <- rev(paste0("grey", round(seq(1, 50, length.out = levels))))
for(i in 2:levels){
  geom_graphic_areas_plot <- geom_graphic_areas_plot +
    ggplot2::geom_sf(data = site$shape[[i]], ggplot2::aes(geometry = geom), col = border_cols[i], alpha = 0)
}

geom_graphic_areas_plot <- geom_graphic_areas_plot +
  ggplot2::facet_wrap(~ name, ncol = 2) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
# ------------------------------------------------------------------------------

# Vector species ---------------------------------------------------------------
vector_pd <- site$vectors$vector_species
vector_name_cols <- site$admin_level[!site$admin_level %in% c("country", "iso3c")]
vector_pd$name <- apply(vector_pd[,vector_name_cols], 1, paste, collapse = " | ")

vector_species_plot <- ggplot2::ggplot(data = vector_pd, ggplot2::aes(x = name, y = prop, fill = vector)) +
  ggplot2::geom_bar(stat = "identity", position = "stack", width = 1) +
  ggplot2::xlab("") +
  ggplot2::ylab("Proportion") +
  ggplot2::scale_fill_discrete(name = "Species") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggplot2::ggtitle(paste0(iso3c, ": Vector species proportions")) +
  ggplot2::coord_flip()

resistance_pd <- site$vectors$pyrethroid_resistance
resistance_plot <- ggplot2::ggplot(data = resistance_pd, ggplot2::aes(x = year, y = pyrethroid_resistance)) +
  ggplot2::geom_point() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Pyrethroid resistance") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle(paste0(iso3c, ": Pyrethroid resistance"))
# ------------------------------------------------------------------------------

# Seasonality ------------------------------------------------------------------
rainfall_pd <- site$seasonality$monthly_rainfall |>
  dplyr::mutate(time = year + (t / 365))
rainfall_name_cols <- site$admin_level[!site$admin_level %in% c("country", "iso3c")]
rainfall_pd$name <- apply(rainfall_pd[,rainfall_name_cols], 1, paste, collapse = " | ")

rainfall_plot <- ggplot2::ggplot(data = rainfall_pd, ggplot2::aes(x = time, y = rainfall, colour = name)) +
  ggplot2::geom_line() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Monthly rainfall (mm)") +
  ggplot2::facet_wrap(~ name, ncol = 1, scales = "free_x") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = ggplot2::element_rect(fill = "white"),
    panel.spacing.y = ggplot2::unit(2, "lines")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Rainfall timeseries"))

seasonal_curve_pd <- site$seasonality$fourier_prediction
seasonal_curve_pd$name <- apply(seasonal_curve_pd[,rainfall_name_cols], 1, paste, collapse = " | ")

labs <- unique(rainfall_pd[,c("t", "month_name")])

seasonality_plot <- ggplot2::ggplot() +
  ggplot2::geom_jitter(data = rainfall_pd, ggplot2::aes(x = t, y = rainfall, colour = name), alpha = 0.2) +
  ggplot2::geom_line(data = seasonal_curve_pd, ggplot2::aes(x = t, y = profile, colour = name)) +
  ggplot2::facet_wrap(~ name, ncol = 1, scales = "free_x") +
  ggplot2::scale_x_continuous(breaks = labs$t, labels = labs$month_name) +
  ggplot2::xlab("Month") +
  ggplot2::ylab("Rainfall (mm)") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = ggplot2::element_rect(fill = "white"),
    panel.spacing.y = ggplot2::unit(2, "lines")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Seasonality"))
# ------------------------------------------------------------------------------

# Interventions ----------------------------------------------------------------
interventions <- site$interventions
interventions_name_cols <- site$admin_level[!site$admin_level %in% c("country", "iso3c")]
interventions$name <- apply(interventions[,interventions_name_cols], 1, paste, collapse = " | ")
pop <- site$population$population_total
pop$name <- apply(pop[,interventions_name_cols], 1, paste, collapse = " | ")

interventions_country_plot <- scene::plot_interventions(
  interventions = interventions,
  population = pop,
  group_var = "iso3c",
  include = c("itn_use", "itn_input_dist", "predicted_use", "tx_cov", "irs_cov", "rtss_cov", "smc_cov"),
  labels = c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC")
) +
  ggplot2::ggtitle(paste0(iso3c, ": Intervention coverage"))

interventions_area_plot <- scene::plot_interventions(
  interventions = interventions,
  population = pop,
  group_var = "name",
  include = c("itn_use", "itn_input_dist", "predicted_use", "tx_cov", "irs_cov", "rtss_cov", "smc_cov"),
  labels = c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC")
) +
  ggplot2::ggtitle(paste0(iso3c, ": Intervention coverage by area"))

# TODO: Update scene plotting function so that ITN model usage is a different colour
## to ITN usage - we match so well in some cases you can't tell them apart.

# TODO: add in LSM
# TODO: add in PMC
# ------------------------------------------------------------------------------

# Accessibility ----------------------------------------------------------------
access <- site$accessibility
access_name_cols <- site$admin_level[!site$admin_level %in% c("country", "iso3c")]
access$name <- apply(access[,access_name_cols], 1, paste, collapse = " | ")
access_pd <- access |>
  tidyr::pivot_longer(cols = dplyr::contains("time"), names_to = "type", values_to = "time") |>
  dplyr::mutate(
    type = dplyr::case_when(
      type == "motor_travel_time_healthcare" ~ "Motor\nto\nhealthcare",
      type == "walking_travel_time_healthcare" ~ "Walking\nto\nhealthcare",
      type == "city_travel_time" ~ "To\ncity"
    )
  )

access_plot <- ggplot2::ggplot(data = access_pd, ggplot2::aes(x = type, y = time, col = type)) +
  ggplot2::geom_boxplot(col = "black") +
  ggplot2::geom_jitter() +
  ggplot2::xlab("") +
  ggplot2::ylab("Travel time (minutes)") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Accessibility"))
# ------------------------------------------------------------------------------

# Blood disorders --------------------------------------------------------------
blood <- site$blood_disorders
blood_name_cols <- site$admin_level[!site$admin_level %in% c("country", "iso3c")]
blood$name <- apply(blood[,blood_name_cols], 1, paste, collapse = " | ")
blood_pd <- blood |>
  tidyr::pivot_longer(cols =-c(site$admin_level, "name"), names_to = "type", values_to = "frequency") |>
  dplyr::mutate(
    type = dplyr::case_when(
      type == "sicklecell" ~ "Sickle\nHaemoglobin\nHbS Allele",
      type == "gdp6" ~ "G6PDd\nAllele",
      type == "hpc" ~ "Haemoglobin\nC (HbC)\nAllele",
      type == "duffy_negativity" ~ "Duffy\nNegativity\nPhenotype"
    )
  )

blood_plot <- ggplot2::ggplot(data = blood_pd, ggplot2::aes(x = type, y = frequency, col = type)) +
  ggplot2::geom_boxplot(col = "black") +
  ggplot2::geom_jitter() +
  ggplot2::xlab("") +
  ggplot2::ylab("Frequency") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Blood disorders"))
# ------------------------------------------------------------------------------

# Prevalence -------------------------------------------------------------------
prevalence_area_pd <- site$prevalence |>
  dplyr::left_join(
    site$population$population_total,
    by = c(site$admin_level, "year")
  ) |>
  dplyr::summarise(
    pfpr = weighted.mean(pfpr, par_pf),
    pvpr = weighted.mean(pvpr, par_pv),
    .by = c(site$admin_level[!site$admin_level == "urban_rural"], "year")
  ) |>
  dplyr::left_join(
    site$shape[[1]]
  )

pfpr_map_plot <- ggplot2::ggplot(data = prevalence_area_pd, ggplot2::aes(geometry = geom, fill = pfpr)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_viridis_c(limits = c(0, 1), option = "B") +
  ggplot2::facet_wrap(~ year, ncol = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": PfPr map"))

pvpr_map_plot <- ggplot2::ggplot(data = prevalence_area_pd, ggplot2::aes(geometry = geom, fill = pvpr)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_viridis_c(limits = c(0, 1), option = "B") +
  ggplot2::facet_wrap(~ year, ncol = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": PvPr map"))

country_prevalence_pd <- site$prevalence |>
  dplyr::left_join(
    site$population$population_total,
    by = c(site$admin_level, "year")
  ) |>
  dplyr::summarise(
    pfpr = weighted.mean(pfpr, par_pf),
    pvpr = weighted.mean(pvpr, par_pv),
    .by = "year"
  ) |>
  tidyr::pivot_longer(-year, names_to = "Species", values_to = "Prevalence")

country_prevalence_plot <- ggplot2::ggplot(data = country_prevalence_pd, ggplot2::aes(x = year, y = Prevalence, col = Species)) +
  ggplot2::geom_line() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Prevalence") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle(paste0(iso3c, ": Prevalence"))
# ------------------------------------------------------------------------------

# Cases and deaths -------------------------------------------------------------
burden_pd <- site$cases_deaths

cases_plot <- ggplot2::ggplot(
  data = burden_pd,
  ggplot2::aes(x = year, y = wmr_cases / 1e6, ymin = wmr_cases_l / 1e6, ymax = wmr_cases_u / 1e6)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(width = 0.5) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("WMR cases (millions)") +
  ggplot2::theme_bw()

incidence_plot <- ggplot2::ggplot(
  data = burden_pd,
  ggplot2::aes(x = year, y = wmr_incidence, ymin = wmr_incidence_l, ymax = wmr_incidence_u)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(width = 0.5) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("WMR clinical incidence (ppar, py)") +
  ggplot2::theme_bw()

deaths_plot <- ggplot2::ggplot(
  data = burden_pd,
  ggplot2::aes(x = year, y = wmr_deaths / 1000, ymin = wmr_deaths_l / 1000, ymax = wmr_deaths_u / 1000)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(width = 0.5) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("WMR deaths (thousands)") +
  ggplot2::theme_bw()

mortality_plot <- ggplot2::ggplot(
  data = burden_pd,
  ggplot2::aes(x = year, y = wmr_mortality, ymin = wmr_mortality_l, ymax = wmr_mortality_u)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(width = 0.5) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("WMR mortality rate (ppar, py)") +
  ggplot2::theme_bw()
# ------------------------------------------------------------------------------

splitFacet <- function(x){
  facet_vars <- names(x$facet$params$facets)         # 1
  x$facet    <- ggplot2::ggplot()$facet              # 2
  datasets   <- split(x$data, x$data[facet_vars])    # 3
  new_plots  <- lapply(datasets,function(new_data) { # 4
    x$data <- new_data
    x})
}  

# Save plots for reporting -----------------------------------------------------
diagnostic_plots <- list(
  # Geography
  geom_graphic_areas = geom_graphic_areas_plot,
  # WMR burden
  cases = cases_plot,
  incidence = incidence_plot,
  deaths = deaths_plot,
  mortality = mortality_plot,
  # MAP prevalence
  pfpr_map = pfpr_map_plot,
  pvpr_map = pvpr_map_plot,
  country_prevalence = country_prevalence_plot,
  # Population
  country_population = country_population_plot,
  country_par = country_population_at_risk_plot,
  country_urban_rural = country_population_urban_rural_plot,
  country_prop_urban = country_prop_urban_plot,
  area_urban_rural = area_urban_rural_population_plot,
  # Demography
  age_dist = age_dist_plot,
  # Seasonality
  rainfall = rainfall_plot,
  seasonality = seasonality_plot,
  # Vectors
  vectors = vector_species_plot,
  resistance = resistance_plot
)
saveRDS(diagnostic_plots, "diagnostic_plots.rds")
# ------------------------------------------------------------------------------

# Render diagnostics report ----------------------------------------------------
quarto::quarto_render(
  input = "diagnostic_report.qmd",
  execute_params = list(
    iso3c =  iso3c,
    country = site$country,
    version = site$version
  )
)
# ------------------------------------------------------------------------------
