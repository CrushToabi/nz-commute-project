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

if (!file.exists(paths$strings)) stop("Cannot find strings file: ", paths$strings)
if (!file.exists(paths$transitions)) stop("Cannot find transitions file: ", paths$transitions)

strings <- readRDS(paths$strings)
transitions <- readRDS(paths$transitions)
stopifnot(length(strings) == nrow(transitions))

route_weights <- transitions$route_weight

## =========================
## 2. Aggregate and save
## =========================

full_matrix <- make_full_transition_matrix(strings, route_weights)
aggr_matrix <- aggregate_sa2_transitions(strings, route_weights)

saveRDS(full_matrix, paths$full_matrix)
saveRDS(aggr_matrix, paths$trans_matrix)

cat("\nDone.\n")
cat("Direction:", direction, "\n")
cat("Full matrix saved:", paths$full_matrix, "\n")
cat("Aggregated matrix saved:", paths$trans_matrix, "\n")
cat("Total aggregated count:", sum(aggr_matrix$count), "\n")
