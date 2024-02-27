un_datafiles <- list.files("data/", pattern = "*.csv")
orderly2::orderly_resource(un_datafiles)
orderly2::orderly_artefact(
  description = "UN population, demography and urbanisation data",
  files = un_datafiles
)
