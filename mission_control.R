# Mission control --------------------------------------------------------------

# ISO3c country codes to run
isos <- c("NGA", "BEN")

# Data processing
for(iso in isos){
  orderly2::orderly_run(
    name = "process_data",
    parameters = list(
      iso3c = iso
    )
  )
}