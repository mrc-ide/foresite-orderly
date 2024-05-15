extents_overlap <- function(raster, boundary){
  boundary <- terra::ext(boundary)
  overlap <- TRUE
  intersection <- terra::intersect(raster, boundary)
  if(is.null(intersection)){
    overlap <- FALSE
  }
  return(overlap)
}

process_raster <- function(raster, boundary){
  has_info <- FALSE
  overlaps <- extents_overlap(raster, boundary)
  if(overlaps){
    raster <- terra::crop(raster, boundary, mask = TRUE, extend = TRUE)
    has_info <- any(!is.na(terra::values(raster)))
  }
  if(has_info){
    return(raster)
  }
  NULL
}