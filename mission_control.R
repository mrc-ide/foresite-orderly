# Mission control --------------------------------------------------------------

# TODOs ------------------------------------------------------------------------
# TODO: Internal and external metadata and versioning
### TODO: Update UN inputs?
### TODO: Update DHS inputs
### TODO: Add MAP SMC into workflow when embargo lifted
# ------------------------------------------------------------------------------

# ISOs -------------------------------------------------------------------------
malaria_endemic_isos <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", 
  "TCD", "COM", "COG", "CIV", "COD", "GNQ", "ERI", "SWZ", "ETH", 
  "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LBR", "MDG", "MWI", 
  "MLI", "MRT", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", 
  "SLE", "ZAF", "SSD", "TGO", "UGA", "TZA", "ZMB", "ZWE", "ARG", 
  "BLZ", "BOL", "BRA", "COL", "CRI", "DOM", "ECU", "SLV", "GUF", 
  "GTM", "GUY", "HTI", "HND", "MEX", "NIC", "PAN", "PRY", "PER", 
  "SUR", "VEN", "AFG", "DJI", "EGY", "IRN", "IRQ", "MAR", "OMN", 
  "PAK", "SAU", "SOM", "SDN", "SYR", "ARE", "YEM", "ARM", "AZE", 
  "GEO", "KAZ", "KGZ", "TJK", "TUR", "TKM", "UZB", "BGD", "BTN", 
  "PRK", "IND", "IDN", "MMR", "NPL", "LKA", "THA", "TLS", "KHM", 
  "CHN", "LAO", "MYS", "PNG", "PHL", "KOR", "SLB", "VUT", "VNM"
)
# ------------------------------------------------------------------------------

# Set up cluster ---------------------------------------------------------------
hipercow::hipercow_init(driver = 'windows')
# hipercow::hipercow_provision()
# hipercow::hipercow_configuration()
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Data get, data prep ----------------------------------------------------------
# ------------------------------------------------------------------------------

# Data inputs
orderly2::orderly_run(
  name = "extents",
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_un",
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_worldpop",
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_dhs",
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_who",
  echo = FALSE
)
# UN population and demography
orderly2::orderly_run(
  name = "un_wpp",
  echo = FALSE
)
# Demography adjustment - on cluster
demog_task_ids <- list()
for(iso in malaria_endemic_isos){
  demog_task_ids[[iso]] <- hipercow::task_create_expr(
    orderly2::orderly_run(
      name = "demography",
      parameters = list(
        iso3c = iso
      )
    ),
    parallel = hipercow::hipercow_parallel("parallel"),
    resources = hipercow::hipercow_resources(cores = 16)
  )
}

orderly2::orderly_run(
  name = "data_map",
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_interventions_manual",
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_chirps",
  echo = FALSE
)
orderly2::orderly_run(
  name = "data_vectors",
  echo = FALSE
)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Parameterised site file creation ---------------------------------------------
# ------------------------------------------------------------------------------

# Run options ------------------------------------------------------------------
boundary <- "GADM_4.1.0" 
isos <- list.files(paste0("src/data_boundaries/boundaries/", boundary))
admin <- 1
urban_rural <- TRUE
name <- "malariaverse"
formatted_date <- format(Sys.Date(), "%m_%Y")
version <- paste(name, formatted_date, sep = "_")
# ------------------------------------------------------------------------------

# Check ISOs have appropriate admin level defined.
boundary_files <- paste0("src/data_boundaries/boundaries/", boundary, "/", isos, "/", isos, "_", admin, ".RDS")
admin_level_available <- file.exists(boundary_files)
if(!all((admin_level_available))){
  missing_isos <- paste0(isos[!admin_level_available], collapse = ", ")
  warning(missing_isos, " do not have requested admin level, dropping")
  `isos` <- isos[admin_level_available]
  boundary_files <- boundary_files[admin_level_available]
}
# Check number of sites (for parallelism resource requirement)
n_sites <- sapply(boundary_files, function(x, admin, urban_rural){
  sites <- nrow(readRDS(x))
  if(urban_rural) sites <- sites * 2
  return(sites)
}, admin = admin, urban_rural = urban_rural)
names(n_sites) <- isos

# Boundaries
orderly2::orderly_run(
  name = "data_boundaries",
  parameters = list(
    boundary = boundary
  ),
  echo = FALSE
)

# Spatial processing
for(iso in isos){
  orderly2::orderly_run(
    name = "spatial",
    parameters = list(
      boundary = boundary,
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
      boundary = boundary,
      iso3c = iso
    ),
    echo = FALSE
  )
}

# Site file creation
for(iso in isos){
  orderly2::orderly_run(
    name = "site_file",
    parameters = list(
      boundary = boundary,
      iso3c = iso,
      admin_level = admin,
      urban_rural = urban_rural,
      version = version
    ),
    echo = FALSE
  )
}

# Diagnostics
for(iso in isos){
  orderly2::orderly_run(
    name = "diagnostics",
    parameters = list(
      boundary = boundary,
      iso3c = iso,
      admin_level = admin,
      urban_rural = urban_rural,
      version = version
    ),
    echo = FALSE
  )
}

# Calibration
cali_task_ids <- list()
for(iso in isos){
  cali_task_ids[[paste0(iso, "_", admin)]] <- hipercow::task_create_expr(
    orderly2::orderly_run(
      name = "calibration",
      parameters = list(
        boundary = boundary,
        iso3c = iso,
        admin_level = admin,
        urban_rural = urban_rural,
        version = version
      ),
      echo = FALSE
    ),
    parallel = hipercow::hipercow_parallel("parallel"),
    resources = hipercow::hipercow_resources(cores = max(2, min(32, n_sites[iso])))
  )
}
x <- hipercow::hipercow_bundle_create(
  ids = unlist(cali_task_ids),
  name = paste0("Calibration_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
)
table(hipercow::hipercow_bundle_status(x))

# Calibration diagnostic report
for(iso in isos){
  orderly2::orderly_run(
    name = "calibration_diagnostics",
    parameters = list(
      boundary = boundary,
      iso3c = iso,
      admin_level = admin,
      urban_rural = urban_rural,
      version = version
    ),
    echo = FALSE
  )
}
