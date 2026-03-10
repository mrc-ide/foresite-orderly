extents_overlap <- function(raster, extent){
  #boundary <- terra::ext(boundary)
  overlap <- TRUE
  intersection <- terra::intersect(raster, extent)
  if(is.null(intersection)){
    overlap <- FALSE
  }
  return(overlap)
}

process_raster <- function(raster, extent, force_out = FALSE){
  has_info <- FALSE
  overlaps <- extents_overlap(raster, extent)
  if(overlaps){
    # TODO: Note to self - masking has been removed here as now using extent, not boundary
    raster <- terra::crop(raster, extent, extend = TRUE)
    has_info <- any(!is.na(terra::values(raster)))
  }
  if(has_info | force_out){
    return(raster)
  }
  NULL
}

weighted.mean2 <- function(x, w, na.rm = TRUE){
  out <- weighted.mean(x, w, na.rm = na.rm)
  if(sum(w) == 0){
    if(all(is.na(x))){
      out <- 0
    } else {
      out <- mean(x, na.rm = na.rm)
    }
  }
  return(out)
}

### Plotting ###################################################################
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
      values = c("#abb979", "#e78a58")
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

plot_map <- function(data, column_name, population, shape, title, title_size = 22, viridis_option = "C", lims = c(0, 1)){
  map_dat <- collapse(
    data,
    column_name,
    population,
    shape
  )
  ggplot(map_dat) +
    geom_sf(aes(geometry = geom, fill = .data[[column_name]])) +
    scale_fill_viridis_c(option = viridis_option, limits = lims, name = "") +
    facet_wrap(~year, nrow = 1) +
    coord_sf() +
    labs(title = title) +
    theme_void() +
    site::theme_site() +
    theme(
      plot.title = element_text(size = title_size),
      aspect.ratio = 1
    )
}
plot_blood_disorders <- function(data, population){
  d <- data |>
    left_join(population) |>
    pivot_longer(cols = c("sicklecell", "gdp6", "hpc", "duffy_negativity"), names_to = "Disorder", values_to = "freq") |>
    mutate(Disorder = case_when(
      Disorder == "sicklecell" ~ "Sicklecell",
      Disorder == "gdp6" ~ "GDP6",
      Disorder == "duffy_negativity" ~ "Duffy negativity",
      Disorder == "hpc" ~ "HPC"
    )) |>
    summarise(
      freq = weighted.mean2(freq, par),
      .by = any_of(c(
        "country",
        "iso3c",
        "name_1",
        "name_2",
        "name_3",
        "name_4",
        "year",
        "Disorder"
      )) 
    ) |>
    dplyr::left_join(sitefile$shape[[1]])
  
  
  ggplot(d) +
    geom_sf(aes(geometry = geom, fill = freq)) +
    scale_fill_viridis_c(option = "B", name = "") +
    facet_wrap(~Disorder) +
    coord_sf() +
    labs(title = "Blood disorders (frequency)") +
    theme_void() +
    site::theme_site() +
    theme(
      title = element_text(size = 22),
      aspect.ratio = 1,
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(colour = "navy")
    )
}

plot_accessibility <- function(data, population){
  d <- data |>
    left_join(population) |>
    pivot_longer(cols = c("motor_travel_time_healthcare", "walking_travel_time_healthcare", "city_travel_time"), names_to = "Mode", values_to = "Minutes") |>
    mutate(Mode = case_when(
      Mode == "motor_travel_time_healthcare" ~ "Motor to healthcare",
      Mode == "walking_travel_time_healthcare" ~ "Walking to healthcare",
      Mode == "city_travel_time" ~ "Time to city"
    )) |>
    summarise(
      Minutes = weighted.mean2(Minutes, par),
      .by = any_of(c(
        "country",
        "iso3c",
        "name_1",
        "name_2",
        "name_3",
        "name_4",
        "year",
        "Mode"
      )) 
    ) |>
    dplyr::left_join(sitefile$shape[[1]])
  
  
  ggplot(d) +
    geom_sf(aes(geometry = geom, fill = Minutes)) +
    scale_fill_viridis_c(option = "B", name = "") +
    facet_wrap(~Mode) +
    coord_sf() +
    labs(title = "Accessibility (travel minutes)") +
    theme_void() +
    site::theme_site() +
    theme(
      title = element_text(size = 22),
      aspect.ratio = 1,
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(colour = "navy")
    )
}
