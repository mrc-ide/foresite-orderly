# Mission control --------------------------------------------------------------

# Data inputs
orderly2::orderly_run(
  name = "data_boundaries",
  parameters = list(
    boundary_version = "GADM_4.1.0"
  )
)
orderly2::orderly_run(name = "data_un")
orderly2::orderly_run(
  name = "data_map",
  parameters = list(
    boundary_version = "GADM_4.1.0"
  )
)
orderly2::orderly_run(name = "data_worldpop")
orderly2::orderly_run(
  name = "data_chirps",
  parameters = list(
    boundary_version = "GADM_4.1.0"
  )
)
orderly2::orderly_run(name = "data_dhs")
orderly2::orderly_run(name = "data_who")
orderly2::orderly_run(
  name = "data_vectors",
  parameters = list(
    boundary_version = "GADM_4.1.0"
  )
)

# UN population and demography
orderly2::orderly_run(
  name = "un_wpp"
)

# Run options ------------------------------------------------------------------
isos <- c("BFA")
admins <- 1
# ------------------------------------------------------------------------------

# Set up cluster ---------------------------------------------------------------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
# hipercow::hipercow_configuration()
# ------------------------------------------------------------------------------

# Demography adjustment - on cluster
demog_task_ids <- list()
for(iso in isos){
  demog_task_ids[[iso]] <- hipercow::task_create_expr(
    orderly2::orderly_run(
      name = "demography",
      parameters = list(
        version_name = "testing",
        iso3c = iso
      )
    ),
    parallel = hipercow::hipercow_parallel("parallel"),
    resources = hipercow::hipercow_resources(cores = 32)
  )
}

# Spatial processing
for(iso in isos){
  orderly2::orderly_run(
    name = "spatial",
    parameters = list(
      version_name = "testing",
      iso3c = iso,
      boundary_version = "GADM_4.1.0"
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
        urban_rural = TRUE,
        boundary_version = "GADM_4.1.0"
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

# Calibration
cali_task_ids <- list()
for(iso in isos){
  for(admin in admins){
    cali_task_ids[[paste0(iso, "_", admin)]] <- hipercow::task_create_expr(
      orderly2::orderly_run(
        name = "calibration",
        parameters = list(
          version_name = "testing",
          iso3c = iso,
          admin_level = admin,
          urban_rural = TRUE
        ),
        echo = FALSE
      ),
      parallel = hipercow::hipercow_parallel("parallel"),
      resources = hipercow::hipercow_resources(cores = 32)
    )
  }
}

hipercow::task_status(cali_task_ids$BFA_1)
