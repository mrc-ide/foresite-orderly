# Adds to rasters associated with years, such that rasters for all_years are
# present. Missing years before data are given empty values (NA), missing years
# after data are given replicates of the last raster
pad_raster <- function(raster, years, all_years, forward_empty = FALSE){
  missing <- setdiff(all_years, years)
  if(length(missing) == 0){
    return(raster)
  }
  
  missing_pre <- missing[missing < min(years)]
  if(length(missing_pre) > 0){
    empty <- raster[[1]]
    terra::values(empty) <- NA
    pre_padding <- replicate(length(missing_pre), empty, simplify = FALSE) |>
      terra::rast()
    raster <- c(pre_padding, raster)
  }
  
  
  missing_post <- missing[missing > max(years)]
  if(length(missing_post) > 0){
    if(forward_empty){
      last <- raster[[1]]
      terra::values(last) <- NA
    } else {
      last <- raster[[dim(raster)[3]]]
    }
    post_paddding <- replicate(length(missing_post), last, simplify = FALSE) |>
      terra::rast()
    raster <- c(raster, post_paddding)
  }
}

# Create a binary classification for the spatial limits of transmission
create_limits <- function(baseline_prevalence_raster){
  limits <- baseline_prevalence_raster
  v <- terra::values(baseline_prevalence_raster)
  terra::values(limits) <- ifelse(v == 0 | is.na(v), 0, 1)
  terra::values(limits) <- as.logical(terra::values(limits))
  return(limits)
}

# Create an urban rural population raster
create_urban_rural_raster <- function(population_raster, urban_population){
  terra::values(population_raster) <- terra::values(population_raster) / sum(terra::values(population_raster), na.rm = TRUE)
  urban_rural <- population_raster
  v <- terra::values(urban_rural)
  r <- rank(-v)
  threshold_rank <- which(
    cumsum(
      sort(v, decreasing = TRUE
      )
    ) > urban_population)[1]
  terra::values(urban_rural) <- ifelse(r > threshold_rank, 0, 1)
  terra::values(urban_rural)[is.na(v)] <- NA
  return(urban_rural)
}

# Check if raster extents overlap
extents_overlap <- function(x, y){
  extent_x <- terra::ext(x)
  extent_y <- terra::ext(y)
  overlap <- TRUE
  if(is.null(terra::intersect(extent_x, extent_y))){
    overlap <- FALSE
  }
  return(overlap)
}

# Mean of positive values
mean_pos <- function(x){
  mean(x[x > 0])
}

# Extract raster values with possible NA replacement
raster_values <- function(x, na_replace = NULL){
  raster_input <- is(x)[1] == "SpatRaster"
  values <- NA
  if(raster_input){
    values <- terra::values(x, mat = FALSE)
    replace_na = !is.null(na_replace)
    if(replace_na){
      values[is.na(values)] <- na_replace
    }
  }
  return(values)
}
