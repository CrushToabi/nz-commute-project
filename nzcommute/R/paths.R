#' Check Direction Label
#'
#' Validates that a commute direction is either home-to-work (`"H2W"`) or
#' work-to-home (`"W2H"`).
#'
#' @param direction Direction label, either `"H2W"` or `"W2H"`.
#' @return The validated direction label.
#' @export
check_direction <- function(direction) {
  if (!direction %in% c("H2W", "W2H")) {
    stop("direction must be either 'H2W' or 'W2H'", call. = FALSE)
  }
  direction
}

#' Build Direction-Specific Artifact Paths
#'
#' Creates standardised file paths for routes, transitions, strings, matrices,
#' and traffic-segment outputs used in the directional commute workflow.
#'
#' @param year Census/shapefile year. Default is 2023.
#' @param direction Direction label, either `"H2W"` or `"W2H"`.
#' @param artifacts_dir Directory where intermediate and final artifacts are stored.
#' @return A named list of file paths.
#' @export
build_artifact_paths <- function(year = 2023,
                                 direction = "H2W",
                                 artifacts_dir = "artifacts") {
  direction <- check_direction(direction)

  list(
    transitions = file.path(
      artifacts_dir,
      paste0("transitions-", year, "-pop-weighted-points-", direction, ".rds")
    ),
    routes = file.path(
      artifacts_dir,
      paste0("routes-car-", year, "-pop-weighted-points-", direction, ".rds")
    ),
    strings = file.path(
      artifacts_dir,
      paste0("strings-", year, "-pop-weighted-points-", direction, ".rds")
    ),
    strings_parts_dir = file.path(
      artifacts_dir,
      paste0("strings-", year, "-pop-weighted-points-", direction, "-parts")
    ),
    full_matrix = file.path(
      artifacts_dir,
      paste0("full-matrix-", year, "-pop-weighted-points-", direction, ".rds")
    ),
    trans_matrix = file.path(
      artifacts_dir,
      paste0("trans-matrix-", year, "-pop-weighted-points-", direction, ".rds")
    ),
    traffic_segments_rds = file.path(
      artifacts_dir,
      paste0("commuter-traffic-segments-", year, "-pop-weighted-points-", direction, ".rds")
    ),
    traffic_segments_csv = file.path(
      artifacts_dir,
      paste0("commuter-traffic-segments-", year, "-pop-weighted-points-", direction, "-ranked.csv")
    ),
    sample_rule_rds = file.path(
      artifacts_dir,
      paste0("recommended-sample-size-rule-", year, ".rds")
    ),
    sample_rule_csv = file.path(
      artifacts_dir,
      paste0("recommended-sample-size-rule-", year, ".csv")
    ),
    sample_rule_comparison_csv = file.path(
      artifacts_dir,
      paste0("sample-size-rule-comparison-", year, ".csv")
    ),
    commuter_summary_csv = file.path(
      artifacts_dir,
      paste0("commuter-count-summary-", year, ".csv")
    ),
    od_sample_size_csv = file.path(
      artifacts_dir,
      paste0("od-sample-size-", year, ".csv")
    )
  )
}

#' Find Boundary Shapefile
#'
#' Looks for a New Zealand boundary shapefile using common project file names.
#'
#' @param prefix Boundary file prefix, such as `"statistical-area-2"`.
#' @param year Shapefile year.
#' @param data_dir Data directory.
#' @return Path to the first matching shapefile.
#' @export
find_boundary_shapefile <- function(prefix, year, data_dir = "data") {
  candidates <- file.path(
    data_dir,
    c(
      paste0(prefix, "-", year, "-generalised.shp"),
      paste0(prefix, "-", year, "-clipped-generalised.shp")
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("No shapefile found. Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  existing[1]
}

#' Find SA2 Shapefile
#'
#' Convenience wrapper for finding Statistical Area 2 shapefiles.
#'
#' @param year Shapefile year.
#' @param data_dir Data directory.
#' @return Path to the SA2 shapefile.
#' @export
find_sa2_shapefile <- function(year = 2023, data_dir = "data") {
  find_boundary_shapefile("statistical-area-2", year = year, data_dir = data_dir)
}
