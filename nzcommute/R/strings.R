#' Compress Consecutive Duplicate IDs
#'
#' Removes consecutive repeated polygon IDs from a route string.
#'
#' @param x Integer vector of polygon IDs.
#' @return Integer vector with consecutive duplicates removed.
#' @export
compress_consecutive <- function(x) {
  if (!length(x)) return(x)
  x[c(TRUE, diff(x) != 0)]
}

#' Fill Missing Polygon IDs
#'
#' Fills missing IDs in a polygon sequence using neighbouring non-missing IDs.
#'
#' @param x Integer vector.
#' @return Integer vector with missing values filled where possible.
#' @export
fill_na_ids <- function(x) {
  if (!length(x)) return(x)
  for (i in seq_along(x)) {
    if (is.na(x[i]) && i > 1) x[i] <- x[i - 1]
  }
  if (all(is.na(x))) return(integer(0))
  for (i in length(x):1) {
    if (is.na(x[i]) && i < length(x)) x[i] <- x[i + 1]
  }
  x
}

#' Resolve Polygon Hits
#'
#' Converts a list of polygon intersection candidates into one polygon ID per
#' route point, preferring continuity with the previous polygon.
#'
#' @param hit_list List of integer vectors returned by spatial intersection.
#' @return Integer vector of selected polygon IDs.
#' @export
resolve_polygon_hits <- function(hit_list) {
  n <- length(hit_list)
  if (n == 0) return(integer(0))

  out <- rep(NA_integer_, n)
  for (i in seq_len(n)) {
    h <- hit_list[[i]]
    if (length(h) == 0) {
      out[i] <- NA_integer_
    } else if (length(h) == 1) {
      out[i] <- h[1]
    } else if (i > 1 && !is.na(out[i - 1]) && out[i - 1] %in% h) {
      out[i] <- out[i - 1]
    } else {
      out[i] <- h[1]
    }
  }

  out <- fill_na_ids(out)
  out <- out[!is.na(out)]
  out
}

#' Process One Route into a Polygon String
#'
#' Converts a road-network route into a sequence of intersected polygon IDs.
#'
#' @param route_mat Route matrix returned by ghroute.
#' @param geo Polygon geometry object.
#' @param geo_crs CRS of `geo`.
#' @param max_segment Maximum segment length used for densifying route geometry.
#' @return Integer vector of polygon IDs, or `NULL`.
#' @export
process_one_route_to_string <- function(route_mat, geo, geo_crs, max_segment = 100) {
  if (is.null(route_mat) || is.character(route_mat) || !length(route_mat)) return(NULL)

  route_mat <- as.matrix(route_mat)
  if (nrow(route_mat) < 2 || ncol(route_mat) < 2) return(NULL)

  line_ll <- sf::st_sfc(sf::st_linestring(route_mat[, 2:1]), crs = 4326)
  line_nz <- sf::st_transform(line_ll, geo_crs)

  cand_rel <- sf::st_intersects(line_nz, geo, sparse = TRUE)[[1]]
  if (!length(cand_rel)) return(NULL)

  cand_geo <- geo[cand_rel]
  seg_nz <- sf::st_segmentize(line_nz, dfMaxLength = max_segment)
  pts_mat <- as.matrix(seg_nz[[1]])
  if (is.null(dim(pts_mat)) || nrow(pts_mat) == 0) return(NULL)

  pts_sf <- sf::st_as_sf(
    data.frame(x = pts_mat[, 1], y = pts_mat[, 2]),
    coords = c("x", "y"),
    crs = geo_crs
  )

  hits_rel <- sf::st_intersects(pts_sf, cand_geo, sparse = TRUE)
  hits_abs <- lapply(hits_rel, function(idx) {
    if (!length(idx)) integer(0) else cand_rel[idx]
  })

  chosen <- resolve_polygon_hits(hits_abs)
  chosen <- compress_consecutive(chosen)
  if (!length(chosen)) return(NULL)
  chosen
}
