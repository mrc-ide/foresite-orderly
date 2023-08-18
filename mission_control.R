# Mission control --------------------------------------------------------------

# ---- Parameters  ---- #
isos <- c("NGA", "BEN")
start_year <- 2000

# ---- Data processing ---- #
for(iso in isos){
  orderly2::orderly_run(
    name = "process_data",
    parameters = list(
      iso3c = iso
    )
  )
}

# ---- Site file elements ---- #

# Demography
for(iso in isos){
  orderly2::orderly_run(
    name = "demography",
    parameters = list(
      iso3c = iso,
      start_year = start_year
    )
  )
}