# function to extract attributes from road network
add_nearest_edge_attributes <- function(
    pts_sf,
    edges_sf,
    attrs,
    edge_id_col = NULL,
    max_dist_m = 50,
    prefix = NULL,
    keep_dist = TRUE
) {
  
  stopifnot(inherits(pts_sf, "sf"), inherits(edges_sf, "sf"))
  
  library(sf)
  library(dplyr)
  
  # check attributes exist
  missing_attrs <- setdiff(attrs, names(edges_sf))
  if (length(missing_attrs) > 0) {
    stop("These attributes are missing from edges_sf: ",
         paste(missing_attrs, collapse = ", "))
  }
  
  if (!is.null(edge_id_col) && !edge_id_col %in% names(edges_sf)) {
    stop("edge_id_col not found in edges_sf")
  }
  
  # ensure same CRS
  pts_m <- pts_sf
  edges_m <- edges_sf
  
  if (st_crs(pts_m) != st_crs(edges_m)) {
    pts_m <- st_transform(pts_m, st_crs(edges_m))
  }
  
  # nearest edge index
  idx <- st_nearest_feature(pts_m, edges_m)
  
  # distance to nearest edge
  d <- st_distance(st_geometry(pts_m), st_geometry(edges_m)[idx], by_element = TRUE)
  d_m <- as.numeric(d)
  
  # within threshold
  ok <- d_m <= max_dist_m
  
  out <- pts_sf
  
  # add attributes
  for (a in attrs) {
    
    new_name <- if (is.null(prefix)) {
      a
    } else {
      paste0(prefix, "_", a)
    }
    
    out[[new_name]] <- NA
    
    out[[new_name]][ok] <- edges_m[[a]][idx[ok]]
  }
  
  # add matched edge id
  if (!is.null(edge_id_col)) {
    
    id_name <- if (is.null(prefix)) {
      "matched_edge_id"
    } else {
      paste0(prefix, "_edge_id")
    }
    
    out[[id_name]] <- NA
    out[[id_name]][ok] <- edges_m[[edge_id_col]][idx[ok]]
  }
  
  # add distance
  if (keep_dist) {
    
    dist_name <- if (is.null(prefix)) {
      "match_dist_m"
    } else {
      paste0(prefix, "_dist_m")
    }
    
    out[[dist_name]] <- d_m
  }
  
  message("Matched ", sum(ok), " / ", nrow(out), " points within ", max_dist_m, " m")
  
  return(out)
}