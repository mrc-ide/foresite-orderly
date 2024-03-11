orderly2::orderly_parameters(
  boundary_version = "GADM_4.1.0"
)

orderly2::orderly_resource("data/")
orderly2::orderly_resource("README.md")

orderly2::orderly_dependency(
  name = "data_boundaries",
  query = "latest(parameter:boundary_version == this:boundary_version)",
  files = "extents.rds"
)

extents <- readRDS("extents.rds")
isos <- names(extents)

split <- function(raster, extent, iso, name, NAflag = NULL){
  if(extents_overlap(raster, extent)){
    address <- paste0("vectors/", iso, "/", name, ".tif")
    cropped <- terra::crop(raster, extent)
    orderly2::orderly_artefact(
      description = paste("Sinka", name, "raster"),
      files = address
    )
    if(is.null(NAflag)){
      terra::writeRaster(cropped, address)
    } else {
      terra::writeRaster(cropped, address, NAflag = NAflag)
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

dir.create("vectors/")
for(iso in isos){
  dir.create(paste0("vectors/", iso, "/"))
  extent <- extents[[iso]]
  split(relative_abundance_arabiensis, extent, iso, "relative_arabiensis")
  split(relative_abundance_funestus, extent, iso, "relative_funestus")
  split(relative_abundance_gambiae, extent, iso, "relative_gambiae")
  
  for(i in 1:length(relative_occurence_raster)){
    split(relative_occurence_raster[[i]], extent, iso, relative_occurence_names[[i]])
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
