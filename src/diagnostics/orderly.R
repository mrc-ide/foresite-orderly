# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Site file diagnostics",
  long = "Take a daw site file and prepares a diagnostic report"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = NULL,
  admin_level = 1,
  urban_rural = TRUE
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
  data = area_urban_rural_pd, ggplot2::aes(x = year, y = pop / 1e6, fill = urban_rural)
) +
  ggplot2::geom_bar(stat = "identity", width = 1) +
  ggplot2::facet_wrap(~ name, ncol = 5, scales = "free_y") +
  ggplot2::ylab("Population (millions)") +
  ggplot2::xlab("Year") +
  ggplot2::scale_fill_manual(values = c("forestgreen", "steelblue"), name = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(), 
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Area Populations urban and rural"))

area_urban_rural_population_plot <- ggplot2::ggplot(
  data = area_urban_rural_pd, ggplot2::aes(x = year, y = pop / 1e6, col = urban_rural)
) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ name, ncol = 5, scales = "free_y") +
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
  dplyr::filter(year %% 10 == 0) |>
  dplyr::summarise(pop = sum(pop), .by = c("iso3c", "year", "age_lower", "age_upper")) |>
  dplyr::mutate(population_proportion = pop / sum(pop), .by = c("iso3c", "year")) |>
  dplyr::select(year, age_lower, population_proportion) |>
  dplyr::mutate(source = "site file")

age_dist_un <- un_wpp |>
  dplyr::filter(year %% 10 == 0)  |>
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
  ggplot2::facet_wrap(~ year, ncol = 5) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Age distribution"))
# ------------------------------------------------------------------------------

# Geographic areas -------------------------------------------------------------
geom_graphic_areas_plot <- ggplot2::ggplot()

levels <- length(site$shape)
for(i in levels:1){
  if(i > 1){
    geom_graphic_areas_plot <- geom_graphic_areas_plot +
      ggplot2::geom_sf(data = site$shape[[i]], ggplot2::aes(geometry = geom))
  } else {
    geom_graphic_areas_plot <- geom_graphic_areas_plot +
      ggplot2::geom_sf(data = site$shape[[i]], ggplot2::aes(geometry = geom), fill = "deeppink")
  }
}
geom_graphic_areas_plot <- geom_graphic_areas_plot +
  ggplot2::facet_wrap(as.formula(paste0("~ name_", levels - 1)), ncol = 5) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
# ------------------------------------------------------------------------------

# Vector species ---------------------------------------------------------------
vector_pd <- site$vectors$vector_species
vector_name_cols <- site$admin_level[!site$admin_level %in% c("country")]
vector_pd$name <- apply(vector_pd[,vector_name_cols], 1, paste, collapse = " | ")

vector_species_plot <- ggplot2::ggplot(data = vector_pd, ggplot2::aes(x = name, y = prop, fill = vector)) +
  ggplot2::geom_bar(stat = "identity", position = "stack", width = 1) +
  ggplot2::xlab("") +
  ggplot2::ylab("Proportion") +
  ggplot2::scale_fill_discrete(name = "Species") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggplot2::ggtitle(paste0(iso3c, ": Vector species proportions"))

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
  ggplot2::facet_wrap(~ name, ncol = 5) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::ggtitle(paste0(iso3c, ": Rainfall timeseries"))

seasonal_curve_pd <- site$seasonality$fourier_prediction
seasonal_curve_pd$name <- apply(seasonal_curve_pd[,rainfall_name_cols], 1, paste, collapse = " | ")

labs <- unique(rainfall_pd[,c("t", "month_name")])

seasonality_plot <- ggplot2::ggplot() +
  ggplot2::geom_jitter(data = rainfall_pd, ggplot2::aes(x = t, y = rainfall, colour = name), alpha = 0.2) +
  ggplot2::geom_line(data = seasonal_curve_pd, ggplot2::aes(x = t, y = profile, colour = name)) +
  ggplot2::facet_wrap(~ name, ncol = 5) +
  ggplot2::scale_x_continuous(breaks = labs$t, labels = labs$month_name) +
  ggplot2::xlab("Month") +
  ggplot2::ylab("Rainfall (mm)") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = ggplot2::element_rect(fill = "white")
  )
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


