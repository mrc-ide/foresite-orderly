# Orderly set-up ---------------------------------------------------------------
orderly2::orderly_description(
  display = "Process non-ratser intervention inputsd",
  long = "Take manually curated interventi0on coverage and create rasters"
)

orderly2::orderly_resource(
  files = "README.md"
)


orderly2::orderly_resource(
  files = "data/chemoprevention_coverage.csv"
)

orderly2::orderly_resource(
  files = "data/rtss_trial.csv"
)

orderly2::orderly_resource(
  files = "data/vaccine_doses.csv"
)

orderly2::orderly_resource(
  files = "admin1_boundary_combined.RDS"
)

orderly2::orderly_resource(
  files = "pfpr_template_raster.tif"
)

orderly2::orderly_dependency(
  name = "extents",
  query = "latest()",
  files = "extents.csv"
)

orderly2::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = "un_wpp.rds"
)

orderly2::orderly_shared_resource("utils.R")
# ------------------------------------------------------------------------------

# Inputs -----------------------------------------------------------------------
chemoprevention_coverage <- read.csv("data/chemoprevention_coverage.csv")
rtss_trial <- read.csv("data/rtss_trial.csv")
vaccine_doses <- read.csv("data/vaccine_doses.csv")
un_wpp <- readRDS("un_wpp.rds") |>
  dplyr::filter(
    year > 2000,
    age_lower == 0
  ) |>
  dplyr::select(iso3c, year, population)

library(sf)
boundary <- readRDS("admin1_boundary_combined.RDS")

#sf_df <- boundary |>
#  dplyr::left_join(manual_coverage, by = c("iso3c", "name_1"))

template <- terra::rast("pfpr_template_raster.tif") 

extents <- read.csv("extents.csv")
# ------------------------------------------------------------------------------

# Process vaccine data ---------------------------------------------------------
## Note that currently due to lack of sub national targeting data we are just
## applying doses across the country
vaccine_coverage <- vaccine_doses |>
  dplyr::mutate(
    iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
    rtss_fvc = round(rtss_doses  / 3.8),
    r21_fvc = round(r21_doses  / 3.8)
    ) |>
  dplyr::left_join(un_wpp, by = c("iso3c", "year")) |>
  dplyr::mutate(
    rtss_cov = round(rtss_fvc / population, 2),
    r21_cov = round(r21_fvc / population, 2),
  ) |>
  dplyr::select(iso3c, year, rtss_cov, r21_cov)
  
sf_df <- boundary |>
  dplyr::cross_join(data.frame(year = 2000:2024)) |>
  dplyr::left_join(vaccine_coverage, by = c("iso3c", "year")) |>
  dplyr::left_join(rtss_trial, by = c("iso3c", "name_1", "year")) |>
  tidyr::replace_na(list(rtss_trial_cov = 0)) |>
  dplyr::mutate(rtss_cov = ifelse(rtss_trial_cov == 0.5, 0.5, rtss_cov)) |>
  dplyr::select(iso3c, name_1, year, rtss_cov, r21_cov) |>
  tidyr::replace_na(list(rtss_cov = 0, r21_cov = 0))
# ------------------------------------------------------------------------------

# Process chemoprevention data -------------------------------------------------
sf_df <- sf_df |>
  dplyr::left_join(chemoprevention_coverage, by = c("iso3c", "name_1", "year")) |>
  dplyr::select(iso3c, name_1, year, rtss_cov, r21_cov, smc_cov, pmc_cov) |>
  tidyr::replace_na(list(smc_cov = 0, pmc_cov = 0))
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

source("utils.R")
split <- function(raster, extent, iso, name, NAflag = NULL){
  raster <- process_raster(raster, extent)
  if(!is.null(raster)){
    address <- paste0("manual/", iso, "/", name, ".tif")
    orderly2::orderly_artefact(
      description = paste("manual", name, "raster"),
      files = address
    )
    if(is.null(NAflag)){
      terra::writeRaster(raster, address)
    } else {
      terra::writeRaster(raster, address, NAflag = NAflag)
    }
  }
}

isos <- unique(sf_df$iso3c)
dir.create("manual/")
paths <- paste0("manual/", isos, "/")
make <- sapply(paths, dir.create)

for(iso in isos){
  extent <- terra::ext(unlist(extents[extents$iso3c == iso, 2:5]))
  
  split(smc_cov, extent, iso, "smc")
  split(pmc_cov, extent, iso, "pmc")
  split(rtss_cov, extent, iso, "rtss")
  split(r21_cov, extent, iso, "r21")
}
# ------------------------------------------------------------------------------