orderly2::orderly_parameters(
  version = NULL
)
dhs_datafiles <- list.files("data/dhs/", full.names = TRUE)
orderly2::orderly_resource(dhs_datafiles)
orderly2::orderly_artefact(
  description = "DHS survey inputs",
  files = dhs_datafiles
)
