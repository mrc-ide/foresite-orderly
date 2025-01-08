# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Process non-ratser intervention inputsd",
  long = "Take manually curated interventi0on coverage and create rasters"
)

orderly2::orderly_resource(
  files = "manual_coverage.csv"
)

orderly2::orderly_resource(
  files = "admin1_boundary_combined.RDS"
)

orderly2::orderly_resource(
  files = "pfpr_template_raster.tif"
)

orderly2::orderly_artefact(
  description = "SMC coverage raster stack",
  files = "smc_cov.tif"
)

orderly2::orderly_artefact(
  description = "PMC coverage raster stack",
  files = "pmc_cov.tif"
)

orderly2::orderly_artefact(
  description = "RTSS coverage raster stack",
  files = "rtss_cov.tif"
)

orderly2::orderly_artefact(
  description = "r21 coverage raster stack",
  files = "r21_cov.tif"
)
# ------------------------------------------------------------------------------

# Inputs -----------------------------------------------------------------------
manual_coverage <- read.csv("manual_coverage.csv")

# Vaccine resources:
# R21
## Cote d'ivoire and South Sudan 250,000 doses each (no targeting info):
## https://www.malariaconsortium.org/news-centre/r21-malaria-vaccine-rollouts-mark-new-era-in-global-fight-against-malaria-cote-divoire-and-south-sudan-lead-the-way.htm

library(sf)
boundary <- readRDS("admin1_boundary_combined.RDS")

sf_df <- boundary |>
  dplyr::left_join(manual_coverage, by = c("iso3c", "name_1"))

template <- terra::rast("pfpr_template_raster.tif") 
# ------------------------------------------------------------------------------

# Create raster stacks ---------------------------------------------------------
make_stack <- function(variable, sf_df, template){
  stack <- list()
  for(y in 2000:max(sf_df$year)){
    b <- dplyr::filter(sf_df, year == y)
    total_cov <- b |> 
      dplyr::pull(variable) |>
      sum()
    if(total_cov > 0){
      r <- terra::rasterize(b, template, field = variable)
      names(r) <- paste(y)
      stack[[paste(y)]] <- r
    }
  }
  stack <- terra::rast(stack)
  terra::varnames(stack) <- variable
  return(stack)
}

smc_cov <- make_stack("smc_cov", sf_df, template)
pmc_cov <- make_stack("pmc_cov", sf_df, template)
rtss_cov <- make_stack("rtss_cov", sf_df, template)
r21_cov <- make_stack("r21_cov", sf_df, template)

terra::writeRaster(smc_cov, "smc_cov.tif")
terra::writeRaster(pmc_cov, "pmc_cov.tif")
terra::writeRaster(rtss_cov, "rtss_cov.tif")
terra::writeRaster(r21_cov, "r21_cov.tif")
# ------------------------------------------------------------------------------