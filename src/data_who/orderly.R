who_datafiles <- list.files("data/who/", full.names = TRUE)
orderly2::orderly_resource(who_datafiles)
orderly2::orderly_artefact(
  description = "WHO WMR inputs",
  files = who_datafiles
)
