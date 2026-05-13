#' Assign Probability-Based Route Weights
#'
#' Distributes each OD pair's commuter count across sampled routes according to
#' the product of origin and destination meshblock probabilities.
#'
#' @param routes_df Expanded route table containing `commuter_count`,
#' `original_od_id`, `o.mb_prob`, and `d.mb_prob`.
#' @return Route table with `raw_route_prob`, `route_prob_sum`, and `route_weight`.
#' @export
assign_route_weights <- function(routes_df) {
  required <- c("commuter_count", "original_od_id", "o.mb_prob", "d.mb_prob")
  missing <- setdiff(required, names(routes_df))
  if (length(missing) > 0) {
    stop("routes_df is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  routes_df |>
    dplyr::mutate(raw_route_prob = .data$o.mb_prob * .data$d.mb_prob) |>
    dplyr::group_by(.data$original_od_id) |>
    dplyr::mutate(
      route_prob_sum = sum(.data$raw_route_prob, na.rm = TRUE),
      route_weight = ifelse(
        .data$route_prob_sum > 0,
        .data$commuter_count * .data$raw_route_prob / .data$route_prob_sum,
        .data$commuter_count / dplyr::n()
      )
    ) |>
    dplyr::ungroup()
}

#' Run GraphHopper Routing for Transition Rows
#'
#' Runs ghroute for origin and destination coordinates in a transition table.
#'
#' @param transitions Data frame with origin/destination coordinate columns.
#' @param osm_file Path to OSM PBF file.
#' @param cache_dir GraphHopper cache directory.
#' @param threads Number of routing threads.
#' @param origin_lat Origin latitude column.
#' @param origin_lon Origin longitude column.
#' @param dest_lat Destination latitude column.
#' @param dest_lon Destination longitude column.
#' @return A list of route matrices.
#' @export
run_ghroute_for_transitions <- function(transitions,
                                        osm_file = "osm/new-zealand-latest.osm.pbf",
                                        cache_dir = paste0("osm/gh/routing-graph-cache-", Sys.info()[["user"]]),
                                        threads = 8,
                                        origin_lat = "o.lat",
                                        origin_lon = "o.lon",
                                        dest_lat = "d.lat",
                                        dest_lon = "d.lon") {
  if (!requireNamespace("ghroute", quietly = TRUE)) {
    stop("The ghroute package is required for routing.", call. = FALSE)
  }
  if (!file.exists(osm_file)) {
    stop("Cannot find OSM file: ", osm_file, call. = FALSE)
  }
  required <- c(origin_lat, origin_lon, dest_lat, dest_lon)
  missing <- setdiff(required, names(transitions))
  if (length(missing) > 0) {
    stop("transitions is missing coordinate columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  ghroute::router(osm_file, cache_dir)
  route_matrix <- as.matrix(transitions[, c(origin_lat, origin_lon, dest_lat, dest_lon)])
  ghroute::rts2list(ghroute::route(route_matrix, threads = threads), nrow(transitions))
}

#' Make Route Geometry Signature
#'
#' Creates a text signature from a route geometry so duplicate routes can be
#' identified within an OD pair.
#'
#' @param route_mat Route matrix returned by ghroute.
#' @param digits Number of decimal digits used when rounding coordinates.
#' @return A character route signature, or `NA` for invalid routes.
#' @export
make_route_signature <- function(route_mat, digits = 5) {
  if (is.null(route_mat)) return(NA_character_)
  if (is.character(route_mat)) return(NA_character_)

  route_df <- as.data.frame(route_mat)
  if (nrow(route_df) < 2) return(NA_character_)
  if (!all(c("lat", "lon") %in% names(route_df))) return(NA_character_)

  lat <- round(route_df$lat, digits)
  lon <- round(route_df$lon, digits)
  paste(paste(lat, lon, sep = ","), collapse = "|")
}

#' Re-normalise Successful Routes
#'
#' After failed GraphHopper routes are removed, redistributes the OD commuter
#' total across successful routes only.
#'
#' @param routes_df Route transition table after removing failed routes.
#' @return Route transition table with re-normalised route weights.
#' @export
renormalise_successful_routes <- function(routes_df) {
  routes_df |>
    dplyr::group_by(.data$original_od_id) |>
    dplyr::mutate(
      successful_weight_sum = sum(.data$route_weight, na.rm = TRUE),
      route_weight = ifelse(
        .data$successful_weight_sum > 0,
        .data$route_weight * .data$commuter_count / .data$successful_weight_sum,
        .data$route_weight
      )
    ) |>
    dplyr::ungroup()
}

#' Merge Duplicate Routes Within OD Pairs
#'
#' Keeps one representative geometry for identical route signatures within the
#' same OD pair and sums their route weights.
#'
#' @param routes List of route matrices.
#' @param transitions Transition table aligned with `routes`.
#' @param signature_digits Digits used for route signatures.
#' @return A list with `routes` and `transitions` after filtering/merging.
#' @export
merge_duplicate_routes <- function(routes, transitions, signature_digits = 5) {
  if (length(routes) != nrow(transitions)) {
    stop("routes and transitions are not aligned", call. = FALSE)
  }

  transitions$route_signature <- vapply(routes, make_route_signature, character(1), digits = signature_digits)
  transitions$route_index_original <- seq_len(nrow(transitions))

  valid_route <- !is.na(transitions$route_signature) &
    !is.na(transitions$route_weight) &
    transitions$route_weight > 0

  tr_valid <- transitions[valid_route, ]
  tr_valid <- renormalise_successful_routes(tr_valid)

  tr_merged <- tr_valid |>
    dplyr::group_by(.data$original_od_id, .data$route_signature) |>
    dplyr::summarise(
      direction = dplyr::first(.data$direction),
      route_weight = sum(.data$route_weight, na.rm = TRUE),
      commuter_count = dplyr::first(.data$commuter_count),
      home_sa2 = dplyr::first(.data$home_sa2),
      work_sa2 = dplyr::first(.data$work_sa2),
      origin_sa2 = dplyr::first(.data$origin_sa2),
      dest_sa2 = dplyr::first(.data$dest_sa2),
      n_samples = dplyr::first(.data$n_samples),
      o.lon = dplyr::first(.data$o.lon),
      o.lat = dplyr::first(.data$o.lat),
      d.lon = dplyr::first(.data$d.lon),
      d.lat = dplyr::first(.data$d.lat),
      o.mb_code = dplyr::first(.data$o.mb_code),
      d.mb_code = dplyr::first(.data$d.mb_code),
      o.mb_prob = dplyr::first(.data$o.mb_prob),
      d.mb_prob = dplyr::first(.data$d.mb_prob),
      raw_route_prob = sum(.data$raw_route_prob, na.rm = TRUE),
      route_prob_sum = dplyr::first(.data$route_prob_sum),
      merged_n = dplyr::n(),
      representative_route_index = dplyr::first(.data$route_index_original),
      .groups = "drop"
    ) |>
    dplyr::mutate(sample_id = dplyr::row_number())

  list(
    routes = routes[tr_merged$representative_route_index],
    transitions = tr_merged,
    n_failed = sum(!valid_route),
    n_merged = nrow(tr_valid) - nrow(tr_merged)
  )
}
