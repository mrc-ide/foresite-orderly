un_datafiles <- list.files("data/", pattern = "*.csv", full.names = TRUE)
orderly2::orderly_resource(un_datafiles)
orderly2::orderly_artefact(
  description = "UN population, demography and urbanisation data and UNICEF 
  neonatal mortality rates",
  files = un_datafiles
)
