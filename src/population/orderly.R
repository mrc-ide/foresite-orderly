# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Population projections",
  long = "Creates population and age-disaggregated population projections at
  the requested level of spatial aggregration"
)

orderly2::orderly_parameters(
  version_name = "testing",
  iso3c = "BFA",
  aggregation = NULL
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = c("un_wup.rds", "un_wpp.rds")
)

orderly2::orderly_dependency(
  name = "spatial",
  query = "latest(parameter:iso3c ==  this:iso3c)",
  files = c("spatial.rds")
)
# ------------------------------------------------------------------------------

# Load inputs ------------------------------------------------------------------
un_wpp <- readRDS("un_wpp.rds")
un_wup <- readRDS("un_wup.rds")
spatial <- readRDS("spatial.rds")
# ------------------------------------------------------------------------------