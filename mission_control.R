# Mission control --------------------------------------------------------------

# UN population and demography
orderly2::orderly_run(name = "un_wpp")

# Demography adjustment (on cluster)

# Spatial processing
orderly2::orderly_run(
  name = "spatial",
  parameters = list(
    version_name = "testing",
    iso3c = "BFA"
  )
)

# Population projections
## Load spatial
## Aggregate
## Convert spatial-pops to proportions
## Complete all missing year-place (historical) and add 0s
## complete all missing future years and extrapolate current proportion
## Load UN projections
## Multiply proportions by UN numbers
## Check this re-captures both UN POP and prop urban with diagnostic

# Interventions & Resistance
## Load resistance
## Load spatial
## Aggregate
## Fill VC outside of SSA
## Link VC parameters
## ITN input dist
## SMC replacement?
