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
  mutate(type = "site file")

un_pop <- un_wpp |>
  dplyr::summarise(
    pop = sum(population),
    .by = c("iso3c", "year")
  ) |>
  mutate(type = "UN")

country_population_pd <- 
  bind_rows(population_country, un_pop)

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
  ggplot2::aes(x = year, y = pop / 1e6, fill = urban_rural)
) +
  ggplot2::geom_bar(stat = "identity") +
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
  mutate(
    type = "site file"
  )

un_prop_urban <- un_wup |>
  mutate(type = "UN")

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


age_dist_pd <- pa |>
  summarise(pop = sum(pop), .by = c("iso3c", "year", "age_lower", "age_upper")) |>
  mutate(pop = pop / sum(pop), .by = c("iso3c", "year"))

age_dist_plot <- ggplot2::ggplot(data = age_dist_pd, ggplot2::aes(x = age_lower, y = pop, col = year, group = year)) +
  ggplot2::geom_line() +
  ggplot2::scale_colour_viridis_c(name = "Year", option = "D") +
  ggplot2::xlab("Age") +
  ggplot2::ylab("Proportion of the population") +
  ggplot2::theme_bw()
# ------------------------------------------------------------------------------