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

# -----------------------------
# Line-to-line matching via spatial overlap (shared corridor logic)
# -----------------------------
# Goal:
# Match attributes from a source network (e.g., Strava Metro, Replica)
# to a target network (OSM-derived edges) in a way that is robust to
# differences in segmentation and minor geometric offsets.
#
# Why this is needed:
# - Different datasets often represent the same real-world corridor using
#   different segment lengths and splits (e.g., OSM edges split at intersections,
#   Strava edges not).
# - A simple nearest-edge join can produce incorrect matches, especially where
#   perpendicular or nearby segments are closer than the true corresponding edge.
#
# Approach:
# 1. Buffer each target line slightly (e.g., 3 m) to allow for small spatial offsets.
# 2. Intersect buffered target lines with source lines.
# 3. Compute the length of overlap between each target–source pair.
# 4. For each target edge, select the source edge with the greatest overlap.
#
# Quality controls:
# - Minimum overlap length (min_overlap_m) filters out trivial intersections.
# - Overlap ratio (overlap_len / target_length) ensures that the match represents
#   a meaningful portion of the target edge, not just a small touch point.
#
# Outputs:
# - Prefixed source attributes (e.g., strava_total_trip_count)
# - overlap length (prefix_overlap_len_m)
# - overlap ratio (prefix_overlap_ratio)
# - matched source ID (prefix_id)
#
# Notes:
# - This method prioritizes "shared corridor" over "nearest geometry".
# - It is more reliable than nearest-edge matching for coincident networks.
# - Still assumes that the majority of the corridor geometry aligns between datasets.
# - For unmatched edges, a fallback method (e.g., nearest-edge) can be applied.
# -----------------------------

add_best_overlap_attributes <- function(
    target_lines,
    source_lines,
    attrs,
    target_id_col,
    source_id_col,
    buffer_m = 3,
    min_overlap_m = 25,
    min_overlap_ratio = 0.25,
    prefix = "src"
) {
  
  stopifnot(inherits(target_lines, "sf"), inherits(source_lines, "sf"))
  
  missing_attrs <- setdiff(attrs, names(source_lines))
  if (length(missing_attrs) > 0) {
    stop("Missing attrs in source_lines: ", paste(missing_attrs, collapse = ", "))
  }
  
  required_target <- target_id_col
  required_source <- source_id_col
  
  if (!required_target %in% names(target_lines)) {
    stop("target_id_col not found in target_lines: ", target_id_col)
  }
  
  if (!required_source %in% names(source_lines)) {
    stop("source_id_col not found in source_lines: ", source_id_col)
  }
  
  if (sf::st_crs(target_lines) != sf::st_crs(source_lines)) {
    source_lines <- sf::st_transform(source_lines, sf::st_crs(target_lines))
  }
  
  target_lengths <- target_lines %>%
    dplyr::mutate(
      target_len_m = as.numeric(sf::st_length(.))
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      target_id = dplyr::all_of(target_id_col),
      target_len_m
    )
  
  target_buf <- target_lines %>%
    dplyr::select(target_id = dplyr::all_of(target_id_col)) %>%
    sf::st_buffer(buffer_m)
  
  source_keep <- source_lines %>%
    dplyr::select(
      source_id = dplyr::all_of(source_id_col),
      dplyr::all_of(attrs)
    )
  
  overlaps <- suppressWarnings(
    sf::st_intersection(target_buf, source_keep)
  ) %>%
    dplyr::mutate(
      overlap_len_m = as.numeric(sf::st_length(.))
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(target_lengths, by = "target_id") %>%
    dplyr::mutate(
      overlap_ratio = overlap_len_m / target_len_m
    ) %>%
    dplyr::filter(
      overlap_len_m >= min_overlap_m,
      overlap_ratio >= min_overlap_ratio
    )
  
  best <- overlaps %>%
    dplyr::group_by(target_id) %>%
    dplyr::slice_max(overlap_len_m, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      !!paste0(prefix, "_id") := source_id,
      !!paste0(prefix, "_overlap_len_m") := overlap_len_m,
      !!paste0(prefix, "_overlap_ratio") := overlap_ratio
    )
  
  # prefix source attributes, but avoid double-prefixing
  new_attr_names <- ifelse(
    startsWith(attrs, paste0(prefix, "_")),
    attrs,
    paste0(prefix, "_", attrs)
  )
  
  attr_rename <- stats::setNames(attrs, new_attr_names)
  
  best <- best %>%
    dplyr::rename(dplyr::all_of(attr_rename))
  
  out <- target_lines %>%
    dplyr::left_join(
      best,
      by = stats::setNames("target_id", target_id_col)
    )
  
  out
}