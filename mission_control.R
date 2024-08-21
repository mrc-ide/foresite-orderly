# Mission control --------------------------------------------------------------

# Run options ------------------------------------------------------------------
# A version identifier that must correspond to a boundaries folder in 
# data_boundaries/boundaries
version <- "GADM_4.1.0" 
isos <- list.files(paste0("src/data_boundaries/boundaries/", version))
admins <- 1
urban_rural <- TRUE
# ------------------------------------------------------------------------------

# Set up cluster ---------------------------------------------------------------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
# hipercow::hipercow_configuration()
# ------------------------------------------------------------------------------

# Data inputs
orderly2::orderly_run(
  name = "data_boundaries",
  parameters = list(
    version = version
  ),
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_un",
  parameters = list(
    version = version
  ),
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_map",
  parameters = list(
    version = version
  ),
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_worldpop",
  parameters = list(
    version = version
  ),
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_chirps",
  parameters = list(
    version = version
  ),
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_dhs",
  parameters = list(
    version = version
  ),
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_who",
  parameters = list(
    version = version
  ),
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_vectors",
  parameters = list(
    version = version
  ),
  echo = FALSE
)

# UN population and demography
orderly2::orderly_run(
  name = "un_wpp",
  parameters = list(
    version = version
  ),
  echo = FALSE
)

# Demography adjustment - on cluster
demog_task_ids <- list()
for(iso in isos){
  demog_task_ids[[iso]] <- hipercow::task_create_expr(
    orderly2::orderly_run(
      name = "demography",
      parameters = list(
        version = version,
        iso3c = iso
      )
    ),
    parallel = hipercow::hipercow_parallel("parallel"),
    resources = hipercow::hipercow_resources(cores = 16)
  )
}

# table(sapply(demog_task_ids, hipercow::task_status))
# hipercow::task_status(demog_task_ids$BFA)

# Spatial processing
for(iso in isos){
  orderly2::orderly_run(
    name = "spatial",
    parameters = list(
      version = version,
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
      version = version,
      iso3c = iso
    ),
    echo = FALSE
  )
}

iso_admin <-
  tidyr::expand_grid(
    iso = isos,
    admin = admins
  )
iso_admin$exists <- 
  apply(iso_admin, 1, function(x){
    boundary <- readRDS(
      paste0("src/data_boundaries/boundaries/", version, "/", x[1], "/", x[1], "_", x[2] ,".rds")
    )
    nrow(boundary) > 1
  })
iso_admin <- iso_admin[iso_admin$exists,]

# Site file creation
for(i in 1:nrow(iso_admin)){
  iso <- iso_admin[[i, "iso"]]
  admin <- iso_admin[[i, "admin"]]
  orderly2::orderly_run(
    name = "site_file",
    parameters = list(
      version = version,
      iso3c = iso,
      admin_level = admin,
      urban_rural = urban_rural
    ),
    echo = FALSE
  )
}

# Diagnostics
#iso_admin_diagnostics <- iso_admin # dplyr::filter(iso_admin, admin == 1)
for(i in 1:nrow(iso_admin)){
  iso <- iso_admin[[i, "iso"]]
  admin <- iso_admin[[i, "admin"]]
  orderly2::orderly_run(
    name = "diagnostics",
    parameters = list(
      version = version,
      iso3c = iso,
      admin_level = admin,
      urban_rural = urban_rural
    ),
    echo = FALSE
  )
}

# Calibration
# Check what site-admin combinations exist, and how many units they have
get_active_sites <- function(version, iso, admin_level, urban_rural){
  parameters <- list(
    version = version,
    iso3c = iso,
    admin_level = admin_level,
    urban_rural = urban_rural
  )
  path <- orderly2::orderly_search(
    "latest(parameter:version == this:version && parameter:iso3c ==  this:iso3c && parameter:admin_level == this:admin_level && parameter:urban_rural == this:urban_rural)",
    parameters = parameters,
    name = "site_file"
  )
  
  site <- readRDS(paste0("archive/site_file/", path, "/site.rds"))
  nrow(site$eir)
}

iso_admin$n_units <- 
  purrr::map2_dbl(iso_admin$iso, iso_admin$admin, .f = function(x, y){
    get_active_sites(version = version, iso = x, admin_level = y, urban_rural = urban_rural)
  })

cali_task_ids <- list()
for(i in 1:nrow(iso_admin)){
  iso <- iso_admin[[i, "iso"]]
  admin <- iso_admin[[i, "admin"]]
  cali_task_ids[[paste0(iso, "_", admin)]] <- hipercow::task_create_expr(
    orderly2::orderly_run(
      name = "calibration",
      parameters = list(
        version = version,
        iso3c = iso,
        admin_level = admin,
        urban_rural = urban_rural
      ),
      echo = FALSE
    ),
    parallel = hipercow::hipercow_parallel("parallel"),
    resources = hipercow::hipercow_resources(cores = max(2, min(32, iso_admin[[i, "n_units"]])))
  )
}
x <- hipercow::hipercow_bundle_create(ids = unlist(cali_task_ids), name = "2024_08_02")

table(hipercow::hipercow_bundle_status(x))
#hipercow::task_log_show(cali_task_ids$ECU_1)
#hipercow::task_status(cali_task_ids$COD_1)
#hipercow::task_log_show(cali_task_ids$CMR_1)

# Calibration diagnostic report
for(i in 1:nrow(iso_admin)){
  iso <- iso_admin[[i, "iso"]]
  admin <- iso_admin[[i, "admin"]]
  orderly2::orderly_run(
    name = "calibration_diagnostics",
    parameters = list(
      version = version,
      iso3c = iso,
      admin_level = admin,
      urban_rural = urban_rural
    ),
    echo = FALSE
  )
}
