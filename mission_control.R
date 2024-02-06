# Mission control --------------------------------------------------------------

# UN population and demography
orderly2::orderly_run(
  name = "un_wpp"
)


isos <- c("BFA", "NGA", "IND")
admins <- 1:2

# Demography adjustment - this would take days locally
for(iso in isos){
  orderly2::orderly_run(
    name = "demography",
    parameters = list(
      version_name = "testing",
      iso3c = iso
    )
  )
}

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

# Site file creation
for(iso in isos){
  for(admin in admins){
    orderly2::orderly_run(
      name = "site_file",
      parameters = list(
        version_name = "testing",
        iso3c = iso,
        admin_level = admin,
        urban_rural = TRUE
      ),
      echo = FALSE
    )
  }
}

# Diagnostics
for(iso in isos){
  for(admin in admins){
    orderly2::orderly_run(
      name = "diagnostics",
      parameters = list(
        version_name = "testing",
        iso3c = iso,
        admin_level = admin,
        urban_rural = TRUE
      ),
      echo = FALSE
    )
  }
}

