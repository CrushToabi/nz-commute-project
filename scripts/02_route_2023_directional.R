options(java.parameters = "-Xmx8g")

library(sf)
library(dplyr)
library(parallel)
library(ghroute)
library(nzcommute)

set.seed(123)
sf_use_s2(FALSE)

## =========================
## 0. Settings
## =========================

if (!exists("direction")) direction <- "H2W"
year <- 2023
paths <- build_artifact_paths(year = year, direction = direction)

data_dir <- "data"
jtw_file <- file.path(data_dir, "2023-census-main-means-of-travel-to-work-by-statistical-area.csv")
pop_file <- file.path(data_dir, "2023-census-electoral-population-at-meshblock-level-2025-meshblock-data.csv")
mb_file  <- file.path(data_dir, "meshblock-2025.csv")
osm_file <- "osm/new-zealand-latest.osm.pbf"

if (!file.exists(osm_file)) stop("Run: make -C osm")
dir.create("artifacts", showWarnings = FALSE)

## =========================
## 1. Read sampling rule
## =========================

if (file.exists(paths$sample_rule_rds)) {
  sample_rule <- readRDS(paths$sample_rule_rds)
  min_samples <- sample_rule$min_samples[1]
  max_samples <- sample_rule$max_samples[1]
  commuters_per_sample <- sample_rule$commuters_per_sample[1]
} else {
  min_samples <- 1
  max_samples <- 15
  commuters_per_sample <- 50
}

cat("Direction:", direction, "\n")
cat("Sampling rule:", commuters_per_sample, "commuters/sample; max", max_samples, "\n")

## =========================
## 2. Read SA2 and OD data
## =========================

shp <- st_read(find_sa2_shapefile(year, data_dir = data_dir), quiet = TRUE) |>
  st_make_valid()

sa2_col <- "SA22023_V1"
shp[[sa2_col]] <- as.character(shp[[sa2_col]])
valid_sa2 <- shp[[sa2_col]]

jtw <- read_jtw_file(jtw_file)

tr_base <- prepare_jtw_od_data(
  jtw = jtw,
  valid_sa2 = valid_sa2,
  direction = direction
)

cat("Valid OD pairs:", nrow(tr_base), "\n")
cat("Total commuter count:", sum(tr_base$commuter_count), "\n")

## =========================
## 3. Prepare population-weighted meshblock sampler
## =========================

meshblock_sf <- prepare_meshblock_population(
  pop_file = pop_file,
  mb_file = mb_file,
  target_crs = st_crs(shp)
)

meshblock_sf <- assign_meshblocks_to_sa2(
  meshblock_sf = meshblock_sf,
  sa2_sf = shp,
  sa2_col = sa2_col
)

sample_point <- create_population_weighted_sampler(
  meshblock_sf = meshblock_sf,
  sa2_sf = shp,
  sa2_col = sa2_col
)

## =========================
## 4. Expand OD rows and sample points
## =========================

tr <- expand_od_samples(
  tr_base,
  min_samples = min_samples,
  max_samples = max_samples,
  commuters_per_sample = commuters_per_sample
)

cat("Expanded sampled routes:", nrow(tr), "\n")

origin_pts <- mclapply(
  tr$origin_sa2,
  sample_point,
  mc.cores = max(1, detectCores() - 1)
)

dest_pts <- mclapply(
  tr$dest_sa2,
  sample_point,
  mc.cores = max(1, detectCores() - 1)
)

origin_df <- do.call(rbind, origin_pts)
dest_df <- do.call(rbind, dest_pts)

tr$o.lon <- origin_df$lon
tr$o.lat <- origin_df$lat
tr$o.mb_code <- origin_df$mb_code
tr$o.mb_population <- origin_df$mb_population
tr$o.sa2_population <- origin_df$sa2_population
tr$o.mb_prob <- origin_df$mb_prob

tr$d.lon <- dest_df$lon
tr$d.lat <- dest_df$lat
tr$d.mb_code <- dest_df$mb_code
tr$d.mb_population <- dest_df$mb_population
tr$d.sa2_population <- dest_df$sa2_population
tr$d.mb_prob <- dest_df$mb_prob

## =========================
## 5. Assign route weights
## =========================

tr <- assign_route_weights(tr)

tr <- tr |>
  filter(
    !is.na(o.lon), !is.na(o.lat),
    !is.na(d.lon), !is.na(d.lat),
    !is.na(route_weight), route_weight > 0
  )

cat("Route weight before routing:", sum(tr$route_weight), "\n")

## =========================
## 6. Run routing
## =========================

rt <- run_ghroute_for_transitions(
  transitions = tr,
  osm_file = osm_file,
  threads = 8
)

## =========================
## 7. Remove failed routes, re-normalise, merge duplicates, and save
## =========================

merged <- merge_duplicate_routes(rt, tr, signature_digits = 5)

saveRDS(merged$transitions, paths$transitions)
saveRDS(merged$routes, paths$routes)

cat("\nDone.\n")
cat("Direction:", direction, "\n")
cat("Failed routed rows:", merged$n_failed, "\n")
cat("Duplicated rows merged:", merged$n_merged, "\n")
cat("Final route weight:", sum(merged$transitions$route_weight), "\n")
cat("Transitions saved:", paths$transitions, "\n")
cat("Routes saved:", paths$routes, "\n")
