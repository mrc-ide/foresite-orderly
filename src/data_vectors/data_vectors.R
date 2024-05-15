orderly2::orderly_parameters(
  version = NULL
)

orderly2::orderly_resource("data/")
orderly2::orderly_resource("README.md")

orderly2::orderly_shared_resource("utils.R")

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:version == this:version)",
  files = "extents.rds"
)

extents <- readRDS("extents.rds")
isos <- names(extents)

# TODO: Why does this not cleanup properly?
country_boundary_files <- paste0("boundaries/", version, "/", isos, "/", isos, "_0.RDS")
names(country_boundary_files) <- paste0("country_boundaries/", isos, "_0.RDS")

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:version == this:version)",
  files = country_boundary_files
)

source("utils.R")

split <- function(raster, boundary, iso, name, NAflag = NULL){
  raster <- process_raster(raster, boundary)
  if(!is.null(raster)){
    address <- paste0("vectors/", iso, "/", name, ".tif")
    orderly2::orderly_artefact(
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
  boundary <- readRDS(paste0("country_boundaries/", iso, "_0.RDS"))
  split(relative_abundance_arabiensis, boundary, iso, "relative_arabiensis")
  split(relative_abundance_funestus, boundary, iso, "relative_funestus")
  split(relative_abundance_gambiae, boundary, iso, "relative_gambiae")
  
  for(i in 1:length(relative_occurence_raster)){
    split(relative_occurence_raster[[i]], boundary, iso, relative_occurence_names[[i]])
  }
}

new_net_introductions <- read.csv("data/alliance_malaria_prevention/new_net_introductions.csv")
address <- paste0("vectors/new_net_introductions.csv")
orderly2::orderly_artefact(
  description = "New net introductions",
  files = address
)
write.csv(new_net_introductions, address, row.names = FALSE)

pyrethroid_resistance <- read.csv("data/pyrethroid_resistance/pyrethroid_resistance.csv")
address <- paste0("vectors/pyrethroid_resistance.csv")
orderly2::orderly_artefact(
  description = "Pyrethroid resistance",
  files = address
)
write.csv(pyrethroid_resistance, address, row.names = FALSE)

irs_insecticide_parameters <- read.csv("data/insecticide_parameters/irs_insecticide_parameters.csv")
address <- "vectors/irs_insecticide_parameters.csv"
orderly2::orderly_artefact(
  description = "irs insecticide parameters",
  files = address
)
write.csv(irs_insecticide_parameters, address, row.names = FALSE)

net_efficacy_adjusted <- read.csv("data/insecticide_parameters/net_efficacy_adjusted.csv")
address <- "vectors/net_efficacy_adjusted.csv"
orderly2::orderly_artefact(
  description = "net efficacy parameters",
  files = address
)
write.csv(net_efficacy_adjusted, address, row.names = FALSE)

vector_bionomics <- read.csv("data/vector_bionomics/vector_bionomics.csv")
address <- "vectors/vector_bionomics.csv"
orderly2::orderly_artefact(
  description = "Vector bionomics",
  files = address
)
write.csv(vector_bionomics, address, row.names = FALSE)
