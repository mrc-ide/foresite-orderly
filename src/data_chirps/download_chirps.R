# Download Chirps data
years <- 2000:2023
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

ym <- tidyr::expand_grid(
  years,
  months
)

urls <- paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/chirps-v2.0.", ym$years, ".", ym$months, ".tif.gz")
destination_files <- paste0(
  "data/rainfall/",
  ym$years, "_", ym$months, ".tif.gz"
)

for(i in 1:length(urls)){
  print(i / length(urls))
  umbrella::download_raster(urls[i], destination_files[i])
}

