# Publishes the hand-curated per-country bounding boxes (extents.csv) that
# downstream reports use to clip global rasters to each country. Originally
# derived from GADM, but the format is provider-agnostic.
orderly::orderly_resource("extents.csv")
