# Malaria-endemic ISOs ---------------------------------------------------------
# The countries the pipeline builds global data for. Single source of truth:
# used by mission_control.R (demography loop), download_worldpop.R, and the
# extents report (which checks extents.csv matches this list).
malaria_endemic_isos <- function(){
  c(
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
}
