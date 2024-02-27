# Access
access_datafiles <- list.files("data/access/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(access_datafiles)
orderly2::orderly_artefact(
  description = "MAP access rasters",
  files = access_datafiles
)

# Blood disorders
blood_disorders_datafiles <- list.files("data/blood_disorders/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(blood_disorders_datafiles)
orderly2::orderly_artefact(
  description = "MAP blood disorder rasters",
  files = blood_disorders_datafiles
)

# IRS coverage
irs_datafiles <- list.files("data/irs/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(irs_datafiles)
orderly2::orderly_artefact(
  description = "MAP IRS rasters",
  files = irs_datafiles
)

# ITN usage
itn_datafiles <- list.files("data/itn/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(itn_datafiles)
orderly2::orderly_artefact(
  description = "MAP ITN rasters",
  files = itn_datafiles
)

# P. falciparum prevalence
pfpr_datafiles <- list.files("data/pfpr/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(pfpr_datafiles)
orderly2::orderly_artefact(
  description = "MAP PfPr rasters",
  files = pfpr_datafiles
)

# P. vivax prevalence
pvpr_datafiles <- list.files("data/pvpr/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(pvpr_datafiles)
orderly2::orderly_artefact(
  description = "MAP pvpr rasters",
  files = pvpr_datafiles
)

# SMC coverage
smc_datafiles <- list.files("data/smc/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(smc_datafiles)
orderly2::orderly_artefact(
  description = "MAP SMC rasters",
  files = smc_datafiles
)

# Tx coverage
tx_datafiles <- list.files("data/tx/", pattern = "*.tif", full.names = TRUE)
orderly2::orderly_resource(tx_datafiles)
orderly2::orderly_artefact(
  description = "MAP treatment rasters",
  files = tx_datafiles
)