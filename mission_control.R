# Mission control --------------------------------------------------------------

# UN population and demography
orderly2::orderly_run(name = "un_wpp")

# Spatial processing
orderly2::orderly_run(
  name = "spatial",
  parameters = list(
    version_name = "testing",
    iso3c = "BFA"
  )
)