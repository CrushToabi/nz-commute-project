library(sf)
library(dplyr)
library(nzcommute)

## =========================
## 0. Settings
## =========================

year <- 2023
paths <- build_artifact_paths(year = year, direction = "H2W")

data_dir <- "data"
jtw_file <- file.path(data_dir, "2023-census-main-means-of-travel-to-work-by-statistical-area.csv")

## =========================
## 1. Read SA2 and Journey-to-Work data
## =========================

shp <- st_read(find_sa2_shapefile(year, data_dir = data_dir), quiet = TRUE) |>
  st_make_valid()

sa2_col <- "SA22023_V1"
shp[[sa2_col]] <- as.character(shp[[sa2_col]])
valid_sa2 <- shp[[sa2_col]]

jtw <- read_jtw_file(jtw_file)

## Direction does not affect commuter-count distribution, so H2W is sufficient here
od <- prepare_jtw_od_data(
  jtw = jtw,
  valid_sa2 = valid_sa2,
  direction = "H2W"
)

## =========================
## 2. Analyse adaptive sampling rules
## =========================

analysis <- analyse_commuter_sample_rules(
  od_data = od,
  commuters_per_sample_values = c(50, 75, 100, 150, 200),
  max_samples_values = c(5, 10, 15, 20),
  min_samples = 1,
  target_routes = c(40000, 80000)
)

print(analysis$summary)
print(analysis$rules)
cat("\nRecommended sampling rule:\n")
print(analysis$recommended)

save_sample_rule(analysis, paths)

cat("\nDone. Saved sample-rule analysis to artifacts/.\n")
