un_datafiles <- paste0("data/", list.files("data/", pattern = "*.csv"))
orderly2::orderly_resource(un_datafiles)
orderly2::orderly_artefact(
  description = "UN population, demography and urbanisation data and UNICEF 
  neonatal mortality rates",
  files = un_datafiles
)
