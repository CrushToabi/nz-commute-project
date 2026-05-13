library(sf)
library(parallel)
library(nzcommute)

sf_use_s2(FALSE)

## =========================
## 0. Settings
## =========================

if (!exists("direction")) direction <- "H2W"
year <- 2023
paths <- build_artifact_paths(year = year, direction = direction)

data_dir <- "data"
max_segment <- 100
chunk_size <- 500
mc_cores <- 10
overwrite_parts <- TRUE

## =========================
## 1. Read data
## =========================

if (!file.exists(paths$routes)) stop("Cannot find routes file: ", paths$routes)
if (!file.exists(paths$transitions)) stop("Cannot find transitions file: ", paths$transitions)

routes <- readRDS(paths$routes)
transitions <- readRDS(paths$transitions)
stopifnot(length(routes) == nrow(transitions))

shp <- st_read(find_sa2_shapefile(year, data_dir = data_dir), quiet = TRUE) |>
  st_make_valid()

geo <- st_geometry(shp)
geo_crs <- st_crs(geo)

## =========================
## 2. Process routes by chunks
## =========================

if (dir.exists(paths$strings_parts_dir) && overwrite_parts) {
  unlink(paths$strings_parts_dir, recursive = TRUE)
}
dir.create(paths$strings_parts_dir, recursive = TRUE, showWarnings = FALSE)

route_id <- seq_along(routes)
chunks <- split(route_id, ceiling(route_id / chunk_size))

for (k in seq_along(chunks)) {
  idx <- chunks[[k]]
  out_file <- file.path(
    paths$strings_parts_dir,
    paste0("strings-", year, "-pop-weighted-points-", direction, "-part-", sprintf("%03d", k), ".rds")
  )

  cat("Chunk", k, "of", length(chunks), "\n")

  chunk_res <- mclapply(
    idx,
    function(i) {
      tryCatch(
        process_one_route_to_string(routes[[i]], geo, geo_crs, max_segment = max_segment),
        error = function(e) {
          structure(
            list(route_index = i, message = conditionMessage(e)),
            class = "route_error"
          )
        }
      )
    },
    mc.cores = mc_cores
  )

  names(chunk_res) <- as.character(idx)
  saveRDS(chunk_res, out_file)
  if (k %% 5 == 0) gc()
}

## =========================
## 3. Combine chunks and save
## =========================

part_files <- list.files(
  paths$strings_parts_dir,
  pattern = paste0("^strings-", year, "-pop-weighted-points-", direction, "-part-[0-9]{3}\\.rds$"),
  full.names = TRUE
)
part_files <- sort(part_files)
if (!length(part_files)) stop("No string chunk files were produced")

strings <- unlist(lapply(part_files, readRDS), recursive = FALSE)
strings <- strings[order(as.integer(names(strings)))]
stopifnot(length(strings) == length(routes))

attr(strings, "year") <- year
attr(strings, "direction") <- direction
attr(strings, "method") <- "directional_population_weighted_meshblock_points"
attr(strings, "kept_route_alignment") <- TRUE

saveRDS(strings, paths$strings)

cat("\nDone.\n")
cat("Direction:", direction, "\n")
cat("Strings saved:", paths$strings, "\n")
cat("Number of strings:", length(strings), "\n")
cat("Successful string routes:", sum(sapply(strings, function(x) is.numeric(x) && length(x) > 1)), "\n")
