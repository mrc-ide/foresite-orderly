# Orderly set-up ---------------------------------------------------------------
orderly::orderly_description(
  display = "Process non-ratser intervention inputs",
  long = "Take manually curated interventi0on coverage and create rasters"
)

orderly::orderly_resource(
  files = "data/chemoprevention_coverage.csv"
)

orderly::orderly_resource(
  files = "data/rtss_trial.csv"
)

orderly::orderly_resource(
  files = "data/vaccine_doses.csv"
)

orderly::orderly_resource(
  files = "admin1_boundary_combined.RDS"
)

orderly::orderly_resource(
  files = "pfpr_template_raster.tif"
)

orderly::orderly_dependency(
  name = "extents",
  query = "latest()",
  files = "extents.csv"
)

orderly::orderly_dependency(
  name = "un_wpp",
  query = "latest()",
  files = "un_wpp.rds"
)

orderly::orderly_shared_resource("utils.R")
orderly::orderly_artefact(
  description = "vaccine_delivery",
  files = "vaccine_delivery.csv"
)
# ------------------------------------------------------------------------------

# Inputs -----------------------------------------------------------------------
chemoprevention_coverage <- read.csv("data/chemoprevention_coverage.csv")
rtss_trial <- read.csv("data/rtss_trial.csv")
vaccine_doses <- read.csv("data/vaccine_doses.csv") |>
  dplyr::mutate(
    iso3c = countrycode::countrycode(country, "country.name", "iso3c")
  )
write.csv(vaccine_doses, "vaccine_delivery.csv", row.names = FALSE)
un_wpp <- readRDS("un_wpp.rds") |>
  dplyr::filter(
    year > 2000,
    age_lower == 0
  ) |>
  dplyr::select(iso3c, year, population)

library(sf)
boundary <- readRDS("admin1_boundary_combined.RDS")

template <- terra::rast("pfpr_template_raster.tif")

extents <- read.csv("extents.csv")
# ------------------------------------------------------------------------------

# Process vaccine data ---------------------------------------------------------
# Note: no sub-national targeting data, so doses are applied uniformly across
# each country.
vaccine_coverage <- vaccine_doses |>
  dplyr::summarise(doses_delivered = sum(doses_delivered),
                   .by = c("iso3c", "year", "vaccine")) |>
  dplyr::mutate(
    # ~3.8 doses per fully vaccinated child (fvc)
    fvc = round(doses_delivered / 3.8)
  ) |>
  dplyr::left_join(un_wpp, by = c("iso3c", "year")) |>
  dplyr::mutate(
    vx_cov = pmin(0.8, round(fvc / population, 2))
  ) |>
  dplyr::select(iso3c, year, vaccine, vx_cov) |>
  tidyr::pivot_wider(id_cols = c("iso3c", "year"), names_from = "vaccine", values_from = "vx_cov") |>
  dplyr::rename("r21_cov" = "r21", "rtss_cov" = "rtss")

sf_df <- boundary |>
  dplyr::cross_join(data.frame(year = 2000:max(vaccine_coverage$year))) |>
  dplyr::left_join(vaccine_coverage, by = c("iso3c", "year")) |>
  dplyr::left_join(rtss_trial, by = c("iso3c", "name_1", "year")) |>
  tidyr::replace_na(list(vx_cov_trial = 0)) |>
  dplyr::mutate(rtss_cov = ifelse(rtss_cov_trial == 0.5, 0.5, rtss_cov)) |>
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
# Rasterise one coverage variable onto the template grid, one layer per year,
# keeping only years that have non-zero coverage.
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

# Clip a coverage raster to one country's extent and write it as an orderly
# artefact; countries the raster does not cover are skipped.
split <- function(raster, extent, iso, name, NAflag = NULL){
  raster <- process_raster(raster, extent)
  if(!is.null(raster)){
    address <- paste0("manual/", iso, "/", name, ".tif")
    orderly::orderly_artefact(
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
