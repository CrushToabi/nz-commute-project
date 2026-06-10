#' Check Direction Label
#'
#' Validates that a direction label is a non-empty string and, optionally, a
#' member of an allowed set.
#'
#' @param direction Direction label.
#' @param allowed Optional character vector of allowed direction labels.
#' @return The validated direction label.
#' @export
check_direction <- function(direction, allowed = NULL) {
  if (length(direction) != 1 || is.na(direction) || !nzchar(direction)) {
    stop("direction must be one non-empty string", call. = FALSE)
  }
  if (!is.null(allowed) && !direction %in% allowed) {
    stop("direction must be one of: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  direction
}

#' Build Direction-Specific Artifact Paths
#'
#' Creates standardised file paths for routes, transitions, strings, matrices,
#' and traffic-segment outputs used in the directional commute workflow.
#'
#' @param year Census/shapefile year. Default is 2023.
#' @param direction Direction label.
#' @param artifacts_dir Directory where intermediate and final artifacts are stored.
#' @param analysis_tag Analysis tag used in file names. Defaults to `year`.
#' @param method_tag Method tag used in file names.
#' @param route_prefix Prefix for route output files.
#' @param traffic_prefix Prefix for traffic-segment output files.
#' @return A named list of file paths.
#' @export
build_artifact_paths <- function(year = 2023,
                                 direction = "H2W",
                                 artifacts_dir = "artifacts",
                                 analysis_tag = as.character(year),
                                 method_tag = "pop-weighted-points",
                                 route_prefix = "routes-car",
                                 traffic_prefix = "commuter-traffic-segments") {
  direction <- check_direction(direction)
  run_tag <- paste(analysis_tag, method_tag, direction, sep = "-")

  list(
    transitions = file.path(
      artifacts_dir,
      paste0("transitions-", run_tag, ".rds")
    ),
    routes = file.path(
      artifacts_dir,
      paste0(route_prefix, "-", run_tag, ".rds")
    ),
    strings = file.path(
      artifacts_dir,
      paste0("strings-", run_tag, ".rds")
    ),
    strings_parts_dir = file.path(
      artifacts_dir,
      paste0("strings-", run_tag, "-parts")
    ),
    full_matrix = file.path(
      artifacts_dir,
      paste0("full-matrix-", run_tag, ".rds")
    ),
    trans_matrix = file.path(
      artifacts_dir,
      paste0("trans-matrix-", run_tag, ".rds")
    ),
    traffic_segments_rds = file.path(
      artifacts_dir,
      paste0(traffic_prefix, "-", run_tag, ".rds")
    ),
    traffic_segments_csv = file.path(
      artifacts_dir,
      paste0(traffic_prefix, "-", run_tag, "-ranked.csv")
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

#' Find NZ 2023 Use-Case SA2 Shapefile
#'
#' Convenience wrapper for the NZ Journey-to-Work case-study scripts.
#'
#' @param year Shapefile year.
#' @param data_dir Data directory.
#' @return Path to the SA2 shapefile.
#' @export
find_sa2_shapefile <- function(year = 2023, data_dir = "data") {
  find_boundary_shapefile("statistical-area-2", year = year, data_dir = data_dir)
}

#' Find NZ Use-Case Meshblock Electoral-Population Shapefile
#'
#' Convenience wrapper for the Stats NZ meshblock-level electoral-population
#' shapefile used by the case-study scripts.
#'
#' @param year Meshblock boundary year. Default is 2025.
#' @param data_dir Data directory.
#' @return Path to the first matching shapefile.
#' @export
find_meshblock_population_shapefile <- function(year = 2025, data_dir = "data") {
  candidates <- file.path(
    data_dir,
    c(
      paste0("2023-census-electoral-population-at-meshblock-level-", year, "-mes.shp"),
      paste0("2023-census-electoral-population-at-meshblock-level-", year, "-meshblock.shp")
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("No meshblock population shapefile found. Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  existing[1]
}
