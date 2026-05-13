#' Make Directional Road Segments from One Route
#'
#' Splits a route into consecutive directed coordinate segments.
#'
#' @param route_mat Route matrix returned by ghroute.
#' @param route_id Route identifier.
#' @param weight Route weight.
#' @param direction Direction label, either `"H2W"` or `"W2H"`.
#' @param digits Number of decimal digits used when rounding coordinates.
#' @return Data frame of directional road segments.
#' @export
make_directional_segments <- function(route_mat,
                                      route_id,
                                      weight,
                                      direction,
                                      digits = 5) {
  direction <- check_direction(direction)
  if (is.null(route_mat)) return(NULL)
  if (is.character(route_mat)) return(NULL)
  if (is.na(weight) || weight <= 0) return(NULL)

  route_mat <- as.data.frame(route_mat)
  if (nrow(route_mat) < 2) return(NULL)
  if (!all(c("lat", "lon") %in% names(route_mat))) return(NULL)

  lat <- round(route_mat[, "lat"], digits)
  lon <- round(route_mat[, "lon"], digits)

  x1 <- lon[-length(lon)]
  y1 <- lat[-length(lat)]
  x2 <- lon[-1]
  y2 <- lat[-1]

  keep <- !(x1 == x2 & y1 == y2)
  x1 <- x1[keep]
  y1 <- y1[keep]
  x2 <- x2[keep]
  y2 <- y2[keep]

  if (length(x1) == 0) return(NULL)

  seg_key <- paste(direction, x1, y1, x2, y2, sep = "_")

  data.frame(
    direction = direction,
    route_id = route_id,
    weight = weight,
    x1 = x1,
    y1 = y1,
    x2 = x2,
    y2 = y2,
    seg_key = seg_key
  )
}

#' Split Routes to Directional Segments
#'
#' Applies `make_directional_segments()` to a route list and transition table.
#'
#' @param routes List of route matrices.
#' @param transitions Transition table containing `route_weight`.
#' @param direction Direction label.
#' @return Data frame of route segments.
#' @export
split_routes_to_segments <- function(routes, transitions, direction) {
  if (length(routes) != nrow(transitions)) {
    stop("routes and transitions are not aligned", call. = FALSE)
  }
  if (!"route_weight" %in% names(transitions)) {
    stop("transitions must contain route_weight", call. = FALSE)
  }

  weights <- transitions$route_weight
  weights[is.na(weights) | weights < 0] <- 0

  is_ok <- !sapply(routes, is.null) & !sapply(routes, is.character) & weights > 0

  dplyr::bind_rows(
    lapply(which(is_ok), function(i) {
      make_directional_segments(
        route_mat = routes[[i]],
        route_id = i,
        weight = weights[i],
        direction = direction
      )
    })
  )
}

#' Aggregate Segment Traffic
#'
#' Aggregates directional road segments into traffic estimates.
#'
#' @param segments Data frame produced by `split_routes_to_segments()`.
#' @return Aggregated segment traffic data frame.
#' @export
aggregate_segment_traffic <- function(segments) {
  if (nrow(segments) == 0) stop("No valid road segments produced", call. = FALSE)

  segments |>
    dplyr::group_by(.data$direction, .data$seg_key, .data$x1, .data$y1, .data$x2, .data$y2) |>
    dplyr::summarise(
      route_count = dplyr::n_distinct(.data$route_id),
      commuter_traffic = sum(.data$weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$commuter_traffic)) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      log_traffic = log1p(.data$commuter_traffic)
    )
}

#' Convert Traffic Segments to sf
#'
#' Converts an aggregated traffic segment data frame into an sf line object.
#'
#' @param traffic_df Aggregated segment traffic data frame.
#' @param crs Coordinate reference system. Default is WGS84.
#' @return sf object of traffic segments.
#' @export
traffic_segments_to_sf <- function(traffic_df, crs = 4326) {
  geom <- lapply(seq_len(nrow(traffic_df)), function(i) {
    sf::st_linestring(matrix(
      c(traffic_df$x1[i], traffic_df$y1[i], traffic_df$x2[i], traffic_df$y2[i]),
      ncol = 2,
      byrow = TRUE
    ))
  })

  sf::st_sf(traffic_df, geometry = sf::st_sfc(geom, crs = crs))
}
