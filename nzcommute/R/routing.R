#' Assign Probability-Based Route Weights
#'
#' Distributes each OD pair's commuter count across sampled routes according to
#' the product of origin and destination meshblock probabilities.
#'
#' @param routes_df Expanded route table containing `commuter_count`,
#' `original_od_id`, `o.mb_prob`, and `d.mb_prob`.
#' @param flow_col Flow-count column.
#' @param od_id_col OD group ID column.
#' @param origin_prob_col Origin probability column.
#' @param dest_prob_col Destination probability column.
#' @return Route table with `raw_route_prob`, `route_prob_sum`, and `route_weight`.
#' @export
assign_route_weights <- function(routes_df,
                                 flow_col = "commuter_count",
                                 od_id_col = "original_od_id",
                                 origin_prob_col = "o.mb_prob",
                                 dest_prob_col = "d.mb_prob") {
  required <- c(flow_col, od_id_col, origin_prob_col, dest_prob_col)
  missing <- setdiff(required, names(routes_df))
  if (length(missing) > 0) {
    stop("routes_df is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  routes_df |>
    dplyr::mutate(raw_route_prob = .data[[origin_prob_col]] * .data[[dest_prob_col]]) |>
    dplyr::group_by(.data[[od_id_col]]) |>
    dplyr::mutate(
      route_prob_sum = sum(.data$raw_route_prob, na.rm = TRUE),
      route_weight = ifelse(
        .data$route_prob_sum > 0,
        .data[[flow_col]] * .data$raw_route_prob / .data$route_prob_sum,
        .data[[flow_col]] / dplyr::n()
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
                                        osm_file,
                                        cache_dir = file.path(tempdir(), paste0("ghroute-cache-", Sys.info()[["user"]])),
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
  routes <- ghroute::rts2list(ghroute::route(route_matrix, threads = threads), nrow(transitions))
  aligned_routes <- vector("list", nrow(transitions))

  for (route in routes) {
    if (is.null(route) || is.character(route)) next
    route_df <- as.data.frame(route)
    if (!"index" %in% names(route_df) || nrow(route_df) == 0) next

    request_index <- as.integer(route_df$index[1])
    if (!is.na(request_index) && request_index >= 1 && request_index <= length(aligned_routes)) {
      aligned_routes[[request_index]] <- route
    }
  }

  aligned_routes
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
#' @param flow_col Flow-count column.
#' @param od_id_col OD group ID column.
#' @return Route transition table with re-normalised route weights.
#' @export
renormalise_successful_routes <- function(routes_df,
                                          flow_col = "commuter_count",
                                          od_id_col = "original_od_id") {
  required <- c(flow_col, od_id_col, "route_weight")
  missing <- setdiff(required, names(routes_df))
  if (length(missing) > 0) {
    stop("routes_df is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  routes_df |>
    dplyr::group_by(.data[[od_id_col]]) |>
    dplyr::mutate(
      successful_weight_sum = sum(.data$route_weight, na.rm = TRUE),
      route_weight = ifelse(
        .data$successful_weight_sum > 0,
        .data$route_weight * .data[[flow_col]] / .data$successful_weight_sum,
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
#' @param flow_col Flow-count column.
#' @param od_id_col OD group ID column.
#' @return A list with `routes` and `transitions` after filtering/merging.
#' @export
merge_duplicate_routes <- function(routes,
                                   transitions,
                                   signature_digits = 5,
                                   flow_col = "commuter_count",
                                   od_id_col = "original_od_id") {
  if (length(routes) != nrow(transitions)) {
    stop("routes and transitions are not aligned", call. = FALSE)
  }
  required <- c(flow_col, od_id_col, "route_weight")
  missing <- setdiff(required, names(transitions))
  if (length(missing) > 0) {
    stop("transitions is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  transitions$route_signature <- vapply(routes, make_route_signature, character(1), digits = signature_digits)
  transitions$route_index_original <- seq_len(nrow(transitions))

  valid_route <- !is.na(transitions$route_signature) &
    !is.na(transitions$route_weight) &
    transitions$route_weight > 0

  tr_valid <- transitions[valid_route, ]
  tr_valid <- renormalise_successful_routes(tr_valid, flow_col = flow_col, od_id_col = od_id_col)
  first_cols <- setdiff(
    names(tr_valid),
    c(od_id_col, "route_signature", "route_weight", "raw_route_prob", "route_index_original", "successful_weight_sum")
  )

  tr_merged <- tr_valid |>
    dplyr::group_by(.data[[od_id_col]], .data$route_signature) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(first_cols), dplyr::first),
      route_weight = sum(.data$route_weight, na.rm = TRUE),
      raw_route_prob = sum(.data$raw_route_prob, na.rm = TRUE),
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
