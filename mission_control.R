# Mission control --------------------------------------------------------------

# ---- Parameters  ---- #
isos <- c("NGA", "PNG")
admin_level <- 1
start_year <- 2000
end_year <- 2002
current_year <- 2023

# ---- UN WPP ---- # 
orderly2::orderly_run(
  name = "un_wpp",
  parameters = list(
    start_year = 2000,
    end_year = 2002
  ),
  echo = FALSE
)

# ---- Data processing / Country split ---- #
for(iso in isos){
  orderly2::orderly_run(
    name = "process_data",
    parameters = list(
      iso3c = iso,
      admin_level = admin_level
    ),
    echo = FALSE
  )
}

# ---- Site file elements ---- #

# Demography
for(iso in isos){
  orderly2::orderly_run(
    name = "demography",
    parameters = list(
      iso3c = iso,
      start_year = start_year,
      end_year = end_year
    ),
    echo = FALSE
  )
}