# Mission control --------------------------------------------------------------

# ---- Parameters  ---- #
isos <- c("NGA", "BEN")
admin_level <- 1
start_year <- 2000
end_year <- 2030
current_year <- 2023

# ---- Data processing ---- #
for(iso in isos){
  orderly2::orderly_run(
    name = "process_data",
    parameters = list(
      iso3c = iso,
      admin_level = admin_level
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