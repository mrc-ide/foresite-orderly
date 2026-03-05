collapse <- function(x, column, population, shape) {
  x |>
    left_join(population) |>
    summarise(
      "{column}" := weighted.mean(.data[[column]], .data[["par"]]),
      .by = any_of(c(
        "country",
        "iso3c",
        "name_1",
        "name_2",
        "name_3",
        "name_4",
        "year"
      ))
    ) |>
    left_join(shape)
}

plot_urban_rural <- function(pop_dat, title){
  years <- sort(unique(pop_dat$year))
  year_shading <- site:::year_shading_data(
    years,
    offset = "discrete",
    ymin = 0,
    ymax = Inf
  )
  
  ggplot2::ggplot(
    pop_dat,
    ggplot2::aes(x = .data$year, y = .data$par, fill = .data$urban_rural)
  ) +
    site:::year_shading_layer(year_shading) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      name = "Urban Rural",
      values = c("green", "brown")
    ) +
    ggplot2::scale_x_continuous(breaks = years) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = "Year", y = "Population", title = title) +
    site:::theme_site()
}

plot_burden <- function(burden_pd, title){
  years <- sort(unique(burden_pd$year))
  year_shading <- site:::year_shading_data(
    years,
    offset = "discrete",
    ymin = 0,
    ymax = Inf
  )
  
  cases_plot <- ggplot2::ggplot(
    data = burden_pd,
    ggplot2::aes(x = year, y = wmr_cases / 1e6, ymin = wmr_cases_l / 1e6, ymax = wmr_cases_u / 1e6)) +
    site:::year_shading_layer(year_shading) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(width = 0.5) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("WMR cases\n(millions)") +
    site::theme_site()
  
  incidence_plot <- ggplot2::ggplot(
    data = burden_pd,
    ggplot2::aes(x = year, y = wmr_incidence, ymin = wmr_incidence_l, ymax = wmr_incidence_u)) +
    site:::year_shading_layer(year_shading) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(width = 0.5) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("WMR clinical incidence\n(ppar, py)") +
    site::theme_site()
  
  deaths_plot <- ggplot2::ggplot(
    data = burden_pd,
    ggplot2::aes(x = year, y = wmr_deaths / 1000, ymin = wmr_deaths_l / 1000, ymax = wmr_deaths_u / 1000)) +
    site:::year_shading_layer(year_shading) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(width = 0.5) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("WMR deaths\n(thousands)") +
    site::theme_site()
  
  mortality_plot <- ggplot2::ggplot(
    data = burden_pd,
    ggplot2::aes(x = year, y = wmr_mortality, ymin = wmr_mortality_l, ymax = wmr_mortality_u)) +
    site:::year_shading_layer(year_shading) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(width = 0.5) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("WMR mortality rate\n(ppar, py)") +
    site::theme_site()
  
  burden <- ((cases_plot / incidence_plot) | (deaths_plot / mortality_plot)) + 
    plot_annotation(title = "Burden", theme = site::theme_site())
  
  return(burden)
}