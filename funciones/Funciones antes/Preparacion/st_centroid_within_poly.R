st_centroid_within_poly <- function (poly) {
  
  # check if centroid is in polygon
  ctrd <- st_centroid(poly, of_largest_polygon = TRUE)
  in_poly <- diag(st_within(ctrd, poly, sparse = F))
  
  # replace geometries that are not within polygon with st_point_on_surface()
  st_geometry(ctrd[!in_poly,]) <- st_geometry(st_point_on_surface(poly[!in_poly,]))
  
  ctrd
}