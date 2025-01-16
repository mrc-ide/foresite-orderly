# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Site file diagnostics",
  long = "Take a daw site file and prepares a diagnostic report"
)

orderly2::orderly_parameters(
  boundary = NULL,
  iso3c = NULL,
  admin_level = NULL,
  urban_rural = NULL,
  version = NULL
)

orderly2::orderly_resource(
  files = c("diagnostic_report.qmd")
)

orderly2::orderly_shared_resource("utils.R")

orderly2::orderly_dependency(
  name = "site_file",
  query = "latest(parameter:boundary == this:boundary && parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural && parameter:version == this:version))",
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
library(knitr)
library(rmarkdown)
library(quarto)
source("utils.R")

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

# Comparison of Pop and Par at Admin 1


area_name_cols <- site$metadata$admin_level[!site$metadata$admin_level %in% c("country", "urban_rural")]
area_par_pd <- p 
area_par_pd$name <- apply(area_par_pd[,area_name_cols], 1, paste, collapse = " | ")
area_par_pd <- area_par_pd |>
  dplyr::summarise(
    pop = sum(pop),
    par = sum(par),
    par_pf = sum(par_pf),
    par_pv = sum(par_pv),
    .by = dplyr::all_of(c("iso3c", "year", "name"))
  ) |>
  tidyr::pivot_longer(-c(iso3c, year, name), names_to = "type", values_to = "y") |>
  dplyr::mutate(
    type = dplyr::case_when(
      type == "pop" ~ "Population",
      type == "par" ~ "Population at risk",
      type == "par_pf" ~ "Population at risk: Pf",
      type == "par_pv" ~ "Population at risk: Pv"
    )
  )

area_population_at_risk_plot <- ggplot2::ggplot(
  data = area_par_pd,
  ggplot2::aes(x = year, y = y / 1e6, col = type, lty = type)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::ylab("Population (millions)") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::ggtitle(paste0(iso3c, ": Sub-national Populations at risk")) +
  ggplot2::facet_wrap(~ name, ncol = 4, scales = "free_x")  +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(), 
    strip.background = ggplot2::element_rect(fill = "white"),
    aspect.ratio = 1
  ) 

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


area_name_cols <- site$metadata$admin_level[!site$metadata$admin_level %in% c("country", "urban_rural")]
area_urban_rural_pd <- p |>
  dplyr::summarise(
    pop = sum(pop),
    .by = dplyr::all_of(c(site$metadata$admin_level, "year"))
  )

area_urban_rural_pd$name <- apply(area_urban_rural_pd[,area_name_cols], 1, paste, collapse = " | ")

area_urban_rural_population_plot <- ggplot2::ggplot(
  data = area_urban_rural_pd, ggplot2::aes(x = year, y = pop / 1e6, col = urban_rural)
) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ name, ncol = 4, scales = "free") +
  ggplot2::ylab("Population (millions)") +
  ggplot2::xlab("Year") +
  ggplot2::scale_fill_manual(values = c("forestgreen", "steelblue"), name = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(), 
    strip.background = ggplot2::element_rect(fill = "white"),
    aspect.ratio = 1
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
  ggplot2::facet_wrap(~ year, ncol = 4, scales = "free_x") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    panel.spacing.y = ggplot2::unit(2, "lines")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Age distribution"))
# ------------------------------------------------------------------------------

# Geographic areas -------------------------------------------------------------
levels <- length(site$shape)

geo_name_cols <- site$metadata$admin_level[!site$metadata$admin_level %in% c("country", "iso3c", "urban_rural")]
site$shape[[1]]$name <- apply(sf::st_drop_geometry(site$shape[[1]])[,geo_name_cols], 1, paste, collapse = " | ")

geom_graphic_areas_plot <-  geom_graphic_areas_plot <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = site$shape[[1]], ggplot2::aes(geometry = geom), fill = "deeppink", col = "grey50")

border_cols <- rev(paste0("grey", round(seq(1, 50, length.out = levels))))
for(i in 2:(levels - 1)){
  geom_graphic_areas_plot <- geom_graphic_areas_plot +
    ggplot2::geom_sf(data = site$shape[[i]], ggplot2::aes(geometry = geom), col = border_cols[i], alpha = 0)
}

geom_graphic_areas_plot <- geom_graphic_areas_plot +
  ggplot2::facet_wrap(~ name, ncol = 4) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
# ------------------------------------------------------------------------------

# Vector species ---------------------------------------------------------------
vector_pd <- site$vectors$vector_species
vector_name_cols <- site$metadata$admin_level[!site$metadata$admin_level %in% c("country", "iso3c")]
vector_pd$name <- apply(vector_pd[,vector_name_cols], 1, paste, collapse = " | ")

vector_species_plot <- ggplot2::ggplot(data = vector_pd, ggplot2::aes(x = name, y = prop, fill = species)) +
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
rainfall_name_cols <- site$metadata$admin_level[!site$metadata$admin_level %in% c("country", "iso3c")]
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
interventions_name_cols <- site$metadata$admin_level[!site$metadata$admin_level %in% c("country", "iso3c")]
interventions$name <- apply(interventions[,interventions_name_cols], 1, paste, collapse = " | ")
pop <- site$population$population_total
pop$name <- apply(pop[,interventions_name_cols], 1, paste, collapse = " | ")

interventions_country_plot <- scene::plot_interventions(
  interventions = interventions,
  population = pop,
  group_var = "iso3c",
  include = c("itn_use", "itn_input_dist", "predicted_use", "tx_cov", "irs_cov", "rtss_cov", "smc_cov", "pmc_cov", "r21_cov"),
  labels = c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC", "R21")
) +
  ggplot2::ggtitle(paste0(iso3c, ": Intervention coverage"))

interventions_area_plot <- scene::plot_interventions(
  interventions = interventions,
  population = pop,
  group_var = "name",
  include = c("itn_use", "itn_input_dist", "predicted_use", "tx_cov", "irs_cov", "rtss_cov", "smc_cov", "pmc_cov", "r21_cov"),
  labels = c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC", "R21"),
  facet_rows = ceiling(length(unique(interventions$name)) / 4)
) +
  ggplot2::ggtitle(paste0(iso3c, ": Intervention coverage by area"))
# ------------------------------------------------------------------------------

# Accessibility ----------------------------------------------------------------
access_pd <- site$accessibility |>
  dplyr::left_join(
    site$population$population_total,
    by = c(site$metadata$admin_level)
  ) |>
  dplyr::summarise(
    motor_travel_time_healthcare = weighted.mean2(motor_travel_time_healthcare, pop),
    walking_travel_time_healthcare = weighted.mean2(walking_travel_time_healthcare, pop),
    city_travel_time = weighted.mean2(city_travel_time, pop),
    .by = c(site$metadata$admin_level[!site$metadata$admin_level == "urban_rural"])
  ) |>
  dplyr::left_join(
    site$shape[[1]]
  )

access_lims = c(
  0, 
  max(
    access_pd$motor_travel_time_healthcare,
    access_pd$walking_travel_time_healthcare,
    access_pd$city_travel_time
  )
)

motor_plot <- ggplot2::ggplot(
  data = access_pd,
  ggplot2::aes(geometry = geom, fill = motor_travel_time_healthcare)
) +
  ggplot2::geom_sf()  +
  ggplot2::scale_fill_viridis_c(direction =  -1, option = "B", name = "Time to\nhealthcare\n(minutes)\nmotorised\ntransport", limits = access_lims) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) 

walk_plot <- ggplot2::ggplot(
  data = access_pd,
  ggplot2::aes(geometry = geom, fill = walking_travel_time_healthcare)
) +
  ggplot2::geom_sf()  +
  ggplot2::scale_fill_viridis_c(direction =  -1, option = "B", name = "Time to\nhealthcare\n(minutes)\nwalking", limits = access_lims) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) 

city_plot <- ggplot2::ggplot(
  data = access_pd,
  ggplot2::aes(geometry = geom, fill = city_travel_time)
) +
  ggplot2::geom_sf()  +
  ggplot2::scale_fill_viridis_c(direction =  -1, option = "B", name = "Time to\nnearest\ncity\n(minutes)", limits = access_lims) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
# ------------------------------------------------------------------------------

# Blood disorders --------------------------------------------------------------
blood_pd <- site$blood_disorders  |>
  dplyr::left_join(
  site$population$population_total,
  by = c(site$metadata$admin_level)
) |>
  dplyr::summarise(
    sicklecell = weighted.mean2(sicklecell, par),
    gdp6 = weighted.mean2(gdp6, par),
    hpc = weighted.mean2(hpc, par),
    duffy_negativity = weighted.mean2(duffy_negativity, par),
    .by = c(site$metadata$admin_level[!site$metadata$admin_level == "urban_rural"])
  ) |>
  dplyr::left_join(
    site$shape[[1]]
  )

sicklecell_plot <- ggplot2::ggplot(
  data = blood_pd,
  ggplot2::aes(geometry = geom, fill = sicklecell)
) +
  ggplot2::geom_sf()  +
  ggplot2::scale_fill_viridis_c(direction =  -1,
                                option = "B",
                                name = "Sickle\nHaemoglobin\nHbS Allele\nFrequency",
                                limits = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) 

gdp6_plot <- ggplot2::ggplot(
  data = blood_pd,
  ggplot2::aes(geometry = geom, fill = gdp6)
) +
  ggplot2::geom_sf()  +
  ggplot2::scale_fill_viridis_c(direction =  -1,
                                option = "B",
                                name = "G6PDd\nAllele\nFrequency",
                                limits = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) 

hpc_plot <- ggplot2::ggplot(
  data = blood_pd,
  ggplot2::aes(geometry = geom, fill = hpc)
) +
  ggplot2::geom_sf()  +
  ggplot2::scale_fill_viridis_c(direction =  -1,
                                option = "B",
                                name = "Haemoglobin\nC (HbC)\nAllele\nFrequency",
                                limits = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) 

duffy_plot <- ggplot2::ggplot(
  data = blood_pd,
  ggplot2::aes(geometry = geom, fill = duffy_negativity)
) +
  ggplot2::geom_sf()  +
  ggplot2::scale_fill_viridis_c(direction =  -1,
                                option = "B",
                                name = "Duffy\nNegativity\nPhenotype\nFrequency",
                                limits = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) 

# ------------------------------------------------------------------------------

# Prevalence -------------------------------------------------------------------
prevalence_area_pd <- site$prevalence |>
  dplyr::left_join(
    site$population$population_total,
    by = c(site$metadata$admin_level, "year")
  ) |>
  dplyr::summarise(
    pfpr = weighted.mean2(pfpr, par_pf),
    pvpr = weighted.mean2(pvpr, par_pv),
    .by = c(site$metadata$admin_level[!site$metadata$admin_level == "urban_rural"], "year")
  ) |>
  dplyr::left_join(
    site$shape[[1]]
  )

pfpr_map_plot <- ggplot2::ggplot(data = prevalence_area_pd, ggplot2::aes(geometry = geom, fill = pfpr)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_viridis_c(limits = c(0, 1), option = "B", direction = -1) +
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
  ggplot2::scale_fill_viridis_c(limits = c(0, 1), option = "B", direction = -1) +
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
    by = c(site$metadata$admin_level, "year")
  ) |>
  dplyr::summarise(
    pfpr = weighted.mean2(pfpr, par_pf),
    pvpr = weighted.mean2(pvpr, par_pv),
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
  area_par = area_population_at_risk_plot,
  country_urban_rural = country_population_urban_rural_plot,
  country_prop_urban = country_prop_urban_plot,
  area_urban_rural = area_urban_rural_population_plot,
  # Demography
  age_dist = age_dist_plot,
  # Interventions
  interventions = interventions_country_plot,
  area_interventions = interventions_area_plot,
  # Seasonality
  rainfall = rainfall_plot,
  seasonality = seasonality_plot,
  # Vectors
  vectors = vector_species_plot,
  resistance = resistance_plot,
  # Blood disorders
  gdp6 = gdp6_plot,
  sickle = sicklecell_plot,
  duffy = duffy_plot,
  hpc = hpc_plot,
  # Access
  motor = motor_plot,
  walk = walk_plot,
  city = city_plot
)
saveRDS(diagnostic_plots, "diagnostic_plots.rds")
# ------------------------------------------------------------------------------

# Render diagnostics report ----------------------------------------------------
quarto::quarto_render(
  input = "diagnostic_report.qmd",
  execute_params = list(
    iso3c = iso3c,
    country = site$metadata$country,
    admin_level = admin_level,
    boundary = site$metadata$boundary,
    version = site$metadata$version
  )
)
# ------------------------------------------------------------------------------
