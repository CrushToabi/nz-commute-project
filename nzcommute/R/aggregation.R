#' Make Full Route-Level Transition Matrix
#'
#' Converts a list of SA2 strings into route-level directed transition rows.
#'
#' @param strings List of polygon ID sequences.
#' @param route_weights Numeric vector of route weights aligned with `strings`.
#' @return Data frame with `route`, `from`, `to`, and `count`.
#' @export
make_full_transition_matrix <- function(strings, route_weights) {
  if (length(strings) != length(route_weights)) {
    stop("strings and route_weights must have the same length", call. = FALSE)
  }

  ps <- lapply(seq_along(strings), function(i) {
    si <- strings[[i]]
    if (inherits(si, "route_error")) return(NULL)
    if (is.null(si) || !length(si)) return(NULL)

    si <- si[!is.na(si)]
    if (length(si) > 1) si <- compress_consecutive(si)

    if (is.numeric(si) && length(si) > 1 && route_weights[i] > 0) {
      data.frame(
        route = i,
        from = si[-length(si)],
        to = si[-1],
        count = route_weights[i]
      )
    } else {
      NULL
    }
  })

  out <- dplyr::bind_rows(ps)
  if (nrow(out) == 0) stop("No valid polygon transitions produced", call. = FALSE)
  out
}

#' Aggregate SA2 Transitions
#'
#' Aggregates route-level polygon transitions into a directed transition matrix.
#'
#' @param strings List of polygon ID sequences.
#' @param route_weights Numeric vector of route weights aligned with `strings`.
#' @return Data frame with `from`, `to`, and `count`.
#' @export
aggregate_sa2_transitions <- function(strings, route_weights) {
  full <- make_full_transition_matrix(strings, route_weights)
  full |>
    dplyr::group_by(.data$from, .data$to) |>
    dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$count))
}
