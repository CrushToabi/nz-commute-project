local({
  source_files <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else frame$ofile
  }, character(1))
  cmd_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  script_file <- tail(stats::na.omit(c(source_files, cmd_file)), 1)
  if (length(script_file) > 0) {
    project_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
    setwd(project_dir)
    local_lib <- file.path(project_dir, ".r-lib")
    if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))
    for (r_file in list.files(file.path(project_dir, "nzcommute", "R"), pattern = "[.]R$", full.names = TRUE)) {
      source(r_file, local = .GlobalEnv)
    }
  }
})

library(sf)
library(nzcommute)

## =========================
## 0. Settings
## =========================

if (!exists("direction")) direction <- "H2W"
year <- 2023
paths <- build_artifact_paths(year = year, direction = direction)

## =========================
## 1. Read data
## =========================

if (!file.exists(paths$routes)) stop("Cannot find routes file: ", paths$routes)
if (!file.exists(paths$transitions)) stop("Cannot find transitions file: ", paths$transitions)

routes <- readRDS(paths$routes)
transitions <- readRDS(paths$transitions)
stopifnot(length(routes) == nrow(transitions))

## =========================
## 2. Split, aggregate, convert to sf, and save
## =========================

segments <- split_routes_to_segments(routes, transitions, direction = direction)
traffic_df <- aggregate_segment_traffic(segments)
traffic_sf <- traffic_segments_to_sf(traffic_df, crs = 4326)

saveRDS(traffic_sf, paths$traffic_segments_rds)
utils::write.csv(sf::st_drop_geometry(traffic_sf), paths$traffic_segments_csv, row.names = FALSE)

cat("\nDone.\n")
cat("Direction:", direction, "\n")
cat("Traffic segments saved:", paths$traffic_segments_rds, "\n")
cat("Traffic CSV saved:", paths$traffic_segments_csv, "\n")
cat("Unique segments:", nrow(traffic_sf), "\n")
