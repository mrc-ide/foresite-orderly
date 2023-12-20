# Mission control --------------------------------------------------------------

# UN population and demography
orderly2::orderly_run(name = "un_wpp")

# Demography adjustment - this would take days locally
orderly2::orderly_run(
  name = "demography"
)

# Spatial processing
orderly2::orderly_run(
  name = "spatial",
  parameters = list(
    version_name = "testing",
    iso3c = "IND"
  ),
  echo = FALSE
)

# Population projections
orderly2::orderly_run(
  name = "population",
  parameters = list(
    version_name = "testing",
    iso3c = "BFA"
  )
)


# Interventions & Resistance
## Load resistance
## Load spatial
## Aggregate
## Link VC parameters
## ITN input dist
## SMC replacement
