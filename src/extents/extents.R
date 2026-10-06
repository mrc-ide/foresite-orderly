# Publishes the hand-curated per-country bounding boxes (extents.csv) that
# downstream reports use to clip global rasters to each country. Originally
# derived from GADM, but the format is provider-agnostic.
orderly::orderly_resource("extents.csv")
orderly::orderly_shared_resource("malaria_endemic_isos.R")

# Check extents.csv covers exactly the malaria-endemic ISOs --------------------
source("malaria_endemic_isos.R")
extent_isos <- read.csv("extents.csv")$iso3c
missing <- setdiff(malaria_endemic_isos(), extent_isos)
extra <- setdiff(extent_isos, malaria_endemic_isos())
if(length(missing) > 0 || length(extra) > 0){
  stop(
    "extents.csv does not match malaria_endemic_isos(). ",
    "Missing: ", paste(missing, collapse = ", "), ". ",
    "Extra: ", paste(extra, collapse = ", "), "."
  )
}
