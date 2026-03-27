# function to extract attributes from road network
add_nearest_edge_attributes <- function(
    pts_sf,
    edges_sf,
    attrs,
    edge_id_col = NULL,
    max_dist_m = 50,
    prefix = NULL,
    keep_dist = TRUE,
    id_name = NULL
) {
  
  stopifnot(inherits(pts_sf, "sf"), inherits(edges_sf, "sf"))
  
  # check attributes exist
  missing_attrs <- setdiff(attrs, names(edges_sf))
  if (length(missing_attrs) > 0) {
    stop("These attributes are missing from edges_sf: ",
         paste(missing_attrs, collapse = ", "))
  }
  
  if (!is.null(edge_id_col) && !edge_id_col %in% names(edges_sf)) {
    stop("edge_id_col not found in edges_sf: ", edge_id_col)
  }
  
  # ensure same CRS
  pts_m <- pts_sf
  edges_m <- edges_sf
  
  if (sf::st_crs(pts_m) != sf::st_crs(edges_m)) {
    pts_m <- sf::st_transform(pts_m, sf::st_crs(edges_m))
  }
  
  # nearest edge index
  idx <- sf::st_nearest_feature(pts_m, edges_m)
  
  # distance to nearest edge
  d <- sf::st_distance(sf::st_geometry(pts_m), sf::st_geometry(edges_m)[idx], by_element = TRUE)
  d_m <- as.numeric(d)
  
  # within threshold
  ok <- d_m <= max_dist_m
  
  out <- pts_sf
  
  # add joined attributes
  for (a in attrs) {
    
    new_name <- if (is.null(prefix)) {
      a
    } else if (!is.null(edge_id_col) && a == edge_id_col && !is.null(id_name)) {
      id_name
    } else if (!is.null(edge_id_col) && a == edge_id_col && is.null(id_name)) {
      paste0(prefix, "_id")
    } else {
      paste0(prefix, "_", a)
    }
    
    out[[new_name]] <- NA
    out[[new_name]][ok] <- edges_m[[a]][idx[ok]]
  }
  
  # add matched edge id separately if requested and not already included in attrs
  if (!is.null(edge_id_col) && !edge_id_col %in% attrs) {
    
    edge_id_name <- if (!is.null(id_name)) {
      id_name
    } else if (is.null(prefix)) {
      "matched_edge_id"
    } else {
      paste0(prefix, "_id")
    }
    
    out[[edge_id_name]] <- NA
    out[[edge_id_name]][ok] <- edges_m[[edge_id_col]][idx[ok]]
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