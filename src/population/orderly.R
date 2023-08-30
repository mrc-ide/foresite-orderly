# Orderly set-up ----------------------------------------------------------------
orderly2::orderly_description(
  display = "Create site file population",
  long = "Processes population raster data by country for creation of site-file
  population elements"
)

orderly2::orderly_parameters(
  iso3c = NULL,
  admin_level = NULL
)

orderly2::orderly_dependency(
  name = "process_data",
  query = "latest(parameter:iso3c == this:iso3c && parameter:admin_level == this:admin_level)",
  files = c("population_pixel_values.RDS", "pfpr_pixel_values.RDS", "pvpr_pixel_values.RDS", "un_population.rds")
)

orderly2::orderly_artefact(
  description = "Demography data: Site file demography", 
  files = "population.rds"
)
# ------------------------------------------------------------------------------

# Limits of transmission -------------------------------------------------------
pfpr <- readRDS("pfpr_pixel_values.RDS")
pvpr <- readRDS("pvpr_pixel_values.RDS")

limits <- pfpr |>
  dplyr::left_join(
    pvpr,
    by = c("ID", "pixel", "year")
  ) |>
  dplyr::filter(
    year == 2000
  ) |>
  dplyr::mutate(
    pf_limits = ifelse(is.na(pfpr) | pfpr == 0, 0, 1),
    pv_limits = ifelse(is.na(pvpr) | pvpr <= 0, 0, 1),
    limits = ifelse(pf_limits == 1 | pv_limits == 1, 1, 0)
  ) |>
  dplyr::select(
    ID, pixel, pf_limits, pv_limits, limits
  )
# ------------------------------------------------------------------------------

# Population -------------------------------------------------------------------

urban_density_threshold <- 1500

un <- readRDS("un_population.RDS")

population_pixel <- readRDS("population_pixel_values.RDS")

population_scaler <- population_pixel |>
  dplyr::summarise(population = sum(population), .by = year) |>
  dplyr::left_join(un, by = "year") |>
  dplyr::mutate(
    scaler = pop / population
  ) |>
  dplyr::select(
    "year", "scaler"
  )

population <- population_pixel |>
  dplyr::left_join(population_scaler, by = "year") |>
  dplyr::mutate(population = population * scaler) |>
  dplyr::select(-scaler) |>
  dplyr::arrange(
    year, -population
  ) |>
  dplyr::mutate(cumsum_population = cumsum(population), .by = "year") |>
  dplyr::left_join(un, by = "year") |>
  dplyr::mutate(urban_rural = ifelse(
    cumsum_population <= (urban / (urban + rural)) * pop, "urban", "rural"
  ))

pd <- population |>
  dplyr::summarise(population = sum(population), .by = c("ID", "year", "urban_rural"))

pd2 <- population |>
  dplyr::summarise(population = sum(population), .by = c("year", "urban_rural"))

pd3 <- readRDS("un_population.RDS") |>
  dplyr::select(- "pop") |>
  tidyr::pivot_longer(cols = -c(iso3c, year), names_to = "urban_rural", values_to = "un_population")
  
  
ggplot() +
  geom_line(data = pd, aes(x = year, y = population, colour = urban_rural)) +
  facet_wrap(~ ID)

ggplot() +
  geom_line(data = pd2, aes(x = year, y = population, colour = urban_rural)) +
  geom_line(data = pd3, aes(x = year, y = un_population, colour = urban_rural), linetype = 2)


|>
  dplyr::mutate(
    urban_rural = ifelse(
      population < urban_density_threshold | is.na(population),
      "rural",
      "urban"
    )
  ) |>
  tidyr::complete(
    tidyr::nesting(
      ID, urban_rural, pixel
    ),
    year
  ) |>
  tidyr::replace_na(
    list(
      population = 0
    )
  ) 

un <- readRDS("un_population.RDS") |>
  dplyr::select(- "pop") |>
  tidyr::pivot_longer(cols = -c(iso3c, year), names_to = "urban_rural", values_to = "un_population")

population_scaler <- population |>
  dplyr::summarise(
    population = sum(population),
    .by = c("year", "urban_rural")
    ) |>
  dplyr::left_join(
    un, 
    by = c("year", "urban_rural")
    ) |>
  dplyr::mutate(
    scaler = un_population / population
    ) |>
  dplyr::select("year", "urban_rural", "scaler")



#|>
#  |>
#  dplyr::left_join(
#    limits,
#    by = c("ID", "pixel")
#  ) |>
#  dplyr::mutate(
    
#  )

