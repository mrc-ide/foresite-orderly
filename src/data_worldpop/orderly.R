wordpop_datafiles <- list.files("data/", pattern = "*.csv", full.names = TRUE)
orderly2::orderly_resource(wordpop_datafiles)
orderly2::orderly_artefact(
  description = "WorldPop population rasters. These are unconstrained, 
  un adjusted, individual country populations 2000-2020 at 1km resolution",
  files = wordpop_datafiles
)
