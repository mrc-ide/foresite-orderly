# Adds to rasters associated with years, such that rasters for all_years are
# present. Missing years before data are given empty values (NA), missing years
# after data are given replicates of the last raster
pad_raster <- function(raster, all_years, forward_empty = FALSE){
  years <- as.integer(names(raster))
  
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
    names(pre_padding) <- missing_pre
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
    post_padding <- replicate(length(missing_post), last, simplify = FALSE) |>
      terra::rast()
    names(post_padding) <- missing_post
    raster <- c(raster, post_padding)
  }
  
  return(raster)
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

access_to_crop2 <- function(access, type = "loess", hybrid = TRUE){
  if(any(access < 0 | access > 1, na.rm = TRUE)){
    stop("access must be between 0 and 1")
  }
  if(!type %in% c("loess", "loess_extrapolate", "linear", "hybrid")){
    stop("type must be one of: loess, loess_extrapolate, linear or hybrid")
  }
  
  smooth <- netz::npc_fits[[type]]
  pred <- unname(stats::predict(smooth, newdata = data.frame(access_mean = access)))
  pred[access == 0] <- 0
  
  if(hybrid){
    smooth2 <- netz::npc_fits[["linear"]]
    pred2 <- unname(stats::predict(smooth2, newdata = data.frame(access_mean = access)))
    pred2[access == 0] <- 0
    pred[access <= 0.5] <- pred2[access <= 0.5]
  }
  return(pred)
}

crop_to_access2 <- function(crop, type = "loess", hybrid = TRUE){
  if(!type %in% c("loess", "loess_extrapolate", "linear")){
    stop("type must be one of: loess, loess_extrapolate or linear")
  }
  smooth <- netz::npc_fits[[type]]
  access <- seq(0, 1, 0.001)
  pred <- access_to_crop2(access, type, hybrid)
  access_out <- stats::approx(x = pred, y = access, xout = crop)$y
  access_out[crop == 0] <- 0
  return(access_out)
}

# Reformat a Year X month raster into a list of months by year
monthify <- function(raster, sep = "\\."){
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  month_labels <- paste0(sep, months)
  raster_list <- lapply(month_labels, function(x){
    subset <- raster[[grepl(x, names(raster))]]
    subset_years <- as.integer(gsub(x, "", names(subset)))
    names(subset) <- subset_years
    return(subset)
  })
  names(raster_list) <- 1:12
  return(raster_list)
}