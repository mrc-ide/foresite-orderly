# x is a spatraster
# y can be a vector of extents (xmin, xmax, ymin, ymax) or a SpatRaster
extents_overlap <- function(raster, y){
  if(is.numeric(y)){
    y <- terra::ext(y)
  }
  overlap <- TRUE
  intersection <- terra::intersect(raster, y)
  if(is.null(intersection)){
    overlap <- FALSE
  }
  return(overlap)
}
