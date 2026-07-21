orderly::orderly_resource("data/")

orderly::orderly_shared_resource("utils.R")

orderly::orderly_dependency(
  name = "extents",
  query = "latest()",
  files = "extents.csv"
)

extents <- read.csv("extents.csv")
isos <- extents$iso3c

source("utils.R")

split <- function(raster, extent, iso, name, NAflag = NULL){
  raster <- process_raster(raster, extent)
  if(!is.null(raster)){
    address <- paste0("vectors/", iso, "/", name, ".tif")
    orderly::orderly_artefact(
      description = paste("Sinka", name, "raster"),
      files = address
    )
    if(is.null(NAflag)){
      terra::writeRaster(raster, address)
    } else {
      terra::writeRaster(raster, address, NAflag = NAflag)
    }
  }
}

relative_abundance_arabiensis <- terra::rast("data/vector_relative_abundance/relative_arabiensis.tif")
names(relative_abundance_arabiensis) <- 2016
relative_abundance_funestus <- terra::rast("data/vector_relative_abundance/relative_funestus.tif")
names(relative_abundance_funestus) <- 2016
relative_abundance_gambiae <- terra::rast("data/vector_relative_abundance/relative_gambiae.tif")
names(relative_abundance_gambiae) <- 2016

relative_occurence_files <- list.files("data/vector_occurrence", full.names = TRUE)
relative_occurence_raster <- lapply(relative_occurence_files, function(x){
  raster <- terra::rast(x)
  names(raster) <- 2016
  return(raster)
} )
relative_occurence_names <- gsub(".tif", "", list.files("data/vector_occurrence"))

library(sf)
dir.create("vectors/")
for(iso in isos){
  dir.create(paste0("vectors/", iso, "/"))
  extent <- terra::ext(unlist(extents[extents$iso3c == iso, 2:5]))
  split(relative_abundance_arabiensis, extent, iso, "relative_arabiensis")
  split(relative_abundance_funestus, extent, iso, "relative_funestus")
  split(relative_abundance_gambiae, extent, iso, "relative_gambiae")

  for(i in 1:length(relative_occurence_raster)){
    split(relative_occurence_raster[[i]], extent, iso, relative_occurence_names[[i]])
  }
}

new_net_introductions <- read.csv("data/alliance_malaria_prevention/new_net_introductions.csv")
address <- paste0("vectors/new_net_introductions.csv")
orderly::orderly_artefact(
  description = "New net introductions",
  files = address
)
write.csv(new_net_introductions, address, row.names = FALSE)

pyrethroid_resistance <- read.csv("data/pyrethroid_resistance/pyrethroid_resistance.csv")

# Fixed year chosen when most places had implemented IG2 (effective nets)
update_resistance_fit <- function(year, resistance, fix_year = 2025){
  A <- min(resistance[year <= fix_year]) # fixed lower asymptote
  K <- resistance[year == fix_year]      # fixed upper asymptote
  fit <- minpack.lm::nlsLM(
    resistance ~ A + (K - A) / (1 + exp(-k * (year - t0))),
    start = list(k = 0.1, t0 = median(year)),
    lower = c(k = 0,   t0 = min(year)),
    upper = c(k = Inf, t0 = max(year)),
    control = minpack.lm::nls.lm.control(maxiter = 200)
  )
  predict(fit)
}

pyrethroid_resistance <- pyrethroid_resistance |>
  dplyr::rename("pyrethroid_resistance_original" = "pyrethroid_resistance") |>
  dplyr::mutate(
    pyrethroid_resistance = update_resistance_fit(
      year = year - 2000,
      resistance = pyrethroid_resistance_original,
      fix_year = 2025 - 2000
    ),
    .by = c("iso3c", "X", "Y", "unit")
  )

resistance_compare_plot <- ggplot2::ggplot(
  data = pyrethroid_resistance
) +
  ggplot2::geom_point(
    ggplot2::aes(x = year, y = pyrethroid_resistance_original)
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = year, y = pyrethroid_resistance),
    colour = "deeppink"
  ) +
  ggplot2::facet_wrap(~ iso3c) +
  ggplot2::theme_bw()

pyrethroid_resistance <- pyrethroid_resistance |>
  dplyr::select(-pyrethroid_resistance_original)

address <- paste0("vectors/pyrethroid_resistance.csv")
orderly::orderly_artefact(
  description = "Pyrethroid resistance",
  files = address
)
write.csv(pyrethroid_resistance, address, row.names = FALSE)

irs_insecticide_parameters <- read.csv("data/insecticide_parameters/irs_insecticide_parameters.csv")
address <- "vectors/irs_insecticide_parameters.csv"
orderly::orderly_artefact(
  description = "irs insecticide parameters",
  files = address
)
write.csv(irs_insecticide_parameters, address, row.names = FALSE)

net_efficacy_adjusted <- read.csv("data/insecticide_parameters/net_efficacy_adjusted.csv")
address <- "vectors/net_efficacy_adjusted.csv"
orderly::orderly_artefact(
  description = "net efficacy parameters",
  files = address
)
write.csv(net_efficacy_adjusted, address, row.names = FALSE)

vector_bionomics <- read.csv("data/vector_bionomics/vector_bionomics.csv")
address <- "vectors/vector_bionomics.csv"
orderly::orderly_artefact(
  description = "Vector bionomics",
  files = address
)
write.csv(vector_bionomics, address, row.names = FALSE)
