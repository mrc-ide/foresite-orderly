# Mission control --------------------------------------------------------------

# ---- Parameters  ---- #
isos <- c("SDN")
admin_level <- 1
start_year <- 2000
end_year <- 2050
current_year <- 2023

# ---- UN WPP ---- # 
orderly2::orderly_run(
  name = "un_wpp",
  parameters = list(
    start_year = start_year,
    end_year = end_year
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
    echo = TRUE
  )
}

# ---- Aggregation of spatial data ---- #

# This includes:
## population
## Populations at risk
## PfPr
## Pvpr
## ITN use (Africa)
## IRS coverage (Africa)
## Treatment coverage
for(iso in isos){
  orderly2::orderly_run(
    name = "aggregation",
    parameters = list(
      iso3c = iso,
      admin_level = admin_level,
      end_year = end_year,
      current_year = current_year
    ),
    echo = TRUE
  )
}

# ---- Interventions ---- #

# ---- Demography ---- #
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

# ---- Vectors ---- #


# ---- Insecticide resistance ---- #


# ---- Seasonality ---- #

# TODO: clean up
# Intervention columsn should match those read in my site
# ID should be id throughout and fix join by
