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

plot_urban_rural <- function(pop_dat){
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
    ggplot2::labs(x = "Year", y = "Population", title = "Urbanicity") +
    site:::theme_site()
}