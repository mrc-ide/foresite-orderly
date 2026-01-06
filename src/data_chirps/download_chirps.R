# Download Chirps data
years <- 2000:2024
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

ym <- tidyr::expand_grid(
  years,
  months
)

urls <- paste0("https://data.chc.ucsb.edu/products/CHIRPS/v3.0/monthly/global/tifs/chirps-v3.0.", ym$years, ".", ym$months, ".tif")
destination_files <- paste0(
  "src/data_chirps/data_site-2601/",
  ym$years, "_", ym$months, ".tif"
)

for(i in 1:length(urls)){
  print(i / length(urls))
  download.file(urls[i], destination_files[i])
}

