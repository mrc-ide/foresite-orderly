extents_overlap <- function(raster, extent){
  #boundary <- terra::ext(boundary)
  overlap <- TRUE
  intersection <- terra::intersect(raster, extent)
  if(is.null(intersection)){
    overlap <- FALSE
  }
  return(overlap)
}

process_raster <- function(raster, extent){
  has_info <- FALSE
  overlaps <- extents_overlap(raster, extent)
  if(overlaps){
    # TODO: Note to self - masking has been removed here as now using extent, not boundary
    raster <- terra::crop(raster, extent, extend = TRUE)
    has_info <- any(!is.na(terra::values(raster)))
  }
  if(has_info){
    return(raster)
  }
  NULL
}

weighted.mean2 <- function(x, w, na.rm = TRUE){
  out <- weighted.mean(x, w, na.rm = na.rm)
  if(sum(w) == 0){
    if(all(is.na(x))){
      out <- 0
    } else {
      out <- mean(x, na.rm = na.rm)
    }
  }
  return(out)
}