# nzcommute

`nzcommute` is a small local R package for commute OD routing workflows. It extracts reusable OD preparation, sampling, routing, route-string conversion, and aggregation tools from the NZ 2023 Journey-to-Work case study.

## Main idea

The package contains reusable tools for:

- adaptive sampling of OD pairs using commuter counts;
- population-weighted meshblock point sampling;
- direction-labelled routing for commute flows;
- route-to-SA2 string conversion;
- SA2 transition aggregation;
- road-segment traffic aggregation.

The scripts in `scripts/` demonstrate how these functions are applied to the 2023 NZ Journey-to-Work data. Dataset-specific choices such as `H2W` / `W2H`, Stats NZ field names, and electoral-population weighting live in those scripts rather than in the package core.

## Install

From the project root:

```r
install.packages(c("devtools", "roxygen2"))
devtools::document()
devtools::install()
```

Then load:

```r
library(nzcommute)
```

## Workflow

Run the sample-rule analysis once:

```r
source("scripts/01_sample_2023.R")
```

Then run the full workflow for one direction:

```r
direction <- "H2W"
source("scripts/02_route_2023_directional.R")
source("scripts/03_string_2023_directional.R")
source("scripts/04_aggr_2023_directional.R")
source("scripts/05_traffic_segments_2023_directional.R")
```

Run again for the reverse direction:

```r
direction <- "W2H"
source("scripts/02_route_2023_directional.R")
source("scripts/03_string_2023_directional.R")
source("scripts/04_aggr_2023_directional.R")
source("scripts/05_traffic_segments_2023_directional.R")
```

Or run both directions:

```r
source("scripts/run_all_2023.R")
```

## Key exported functions

```r
# Paths and validation
build_artifact_paths()
check_direction()
find_boundary_shapefile()
find_meshblock_population_shapefile()
find_sa2_shapefile()

# OD preparation and sampling
read_jtw_file()
prepare_od_data()
prepare_jtw_od_data()
choose_n_samples()
analyse_commuter_sample_rules()
save_sample_rule()
expand_od_samples()
prepare_meshblock_population()
assign_meshblocks_to_sa2()
create_population_weighted_sampler()

# Routing and route post-processing
assign_route_weights()
run_ghroute_for_transitions()
make_route_signature()
merge_duplicate_routes()
renormalise_successful_routes()

# Route strings and transition matrices
compress_consecutive()
fill_na_ids()
resolve_polygon_hits()
process_one_route_to_string()
make_full_transition_matrix()
aggregate_sa2_transitions()

# Road-segment traffic
make_directional_segments()
split_routes_to_segments()
aggregate_segment_traffic()
traffic_segments_to_sf()
```

## Project design

The package is the reusable toolkit. The `scripts/` workflow is the NZ 2023 case study and is intentionally responsible for input/output orchestration and dataset-specific field handling. This follows the supervisor's advice: generalise the core routing and aggregation functions, and use the NZ 2023 commute analysis only as a demonstration of the tools.
