# Mission control --------------------------------------------------------------

# UN population and demography
orderly2::orderly_run(
  name = "un_wpp"
)

# Demography adjustment - this would take days locally
orderly2::orderly_run(
  name = "demography"
)

isos <- c("BFA", "KEN", "IND")

# Spatial processing
for(iso in isos){
  orderly2::orderly_run(
    name = "spatial",
    parameters = list(
      version_name = "testing",
      iso3c = iso
    ),
    echo = FALSE
  )
}  

# Population projections
for(iso in isos){
  orderly2::orderly_run(
    name = "population",
    parameters = list(
      version_name = "testing",
      iso3c = iso
    ),
    echo = FALSE
  )
}

# Site file creation - admin unit 1
for(iso in isos){
  orderly2::orderly_run(
    name = "site_file",
    parameters = list(
      version_name = "testing",
      iso3c = iso,
      admin_level = 1,
      urban_rural = TRUE
    ),
    echo = FALSE
  )
}

# Site file creation - admin unit 2
for(iso in isos){
  orderly2::orderly_run(
    name = "site_file",
    parameters = list(
      version_name = "testing",
      iso3c = iso,
      admin_level = 2,
      urban_rural = TRUE
    ),
    echo = FALSE
  )
}

