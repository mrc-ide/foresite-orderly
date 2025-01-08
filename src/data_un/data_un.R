# TODO:
## To fix errors when running cleanup in these input data files we can
## Replace these lines:
### my_datafiles <- list.files("data/", full.names = TRUE)
### orderly2::orderly_resource(my_datafiles)
## With:
### orderly2::orderly_resource("data/")

un_datafiles <- list.files("data/", pattern = "*.csv", full.names = TRUE)
orderly2::orderly_resource(un_datafiles)
orderly2::orderly_artefact(
  description = "UN population, demography and urbanisation data and UNICEF 
  neonatal mortality rates",
  files = un_datafiles
)
