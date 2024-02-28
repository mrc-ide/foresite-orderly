vector_datafiles <- list.files("data/vectors/", full.names = TRUE)
orderly2::orderly_resource(vector_datafiles)
orderly2::orderly_artefact(
  description = "Vector distribution and bionomics",
  files = vector_datafiles
)
