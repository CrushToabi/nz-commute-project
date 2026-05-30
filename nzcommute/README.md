# nzcommute

`nzcommute` is a small local R package for the NZ commute-routing project. It extracts repeated code from the analysis scripts into reusable functions, while keeping the 2023 Journey-to-Work analysis as a clear use-case workflow.

## Main idea

The package contains reusable tools for:

- adaptive sampling of OD pairs using commuter counts;
- population-weighted meshblock point sampling;
- direction-specific routing for `H2W` and `W2H`;
- route-to-SA2 string conversion;
- SA2 transition aggregation;
- road-segment traffic aggregation.

The scripts in `scripts/` demonstrate how these functions are applied to the 2023 NZ Journey-to-Work data.

## Install

From the project root:

```r
install.packages(c(
  "devtools",
  "roxygen2",
  "dplyr",
  "sf",
  "readr",
  "ggplot2",
  "leaflet",
  "htmlwidgets",
  "scales"
))

# Install ghroute separately if it is not already available.
# See https://RForge.net/ghroute for the current installation instructions.

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
build_artifact_paths()
prepare_jtw_od_data()
analyse_commuter_sample_rules()
expand_od_samples()
prepare_meshblock_population()
assign_meshblocks_to_sa2()
create_population_weighted_sampler()
assign_route_weights()
run_ghroute_for_transitions()
merge_duplicate_routes()
process_one_route_to_string()
aggregate_sa2_transitions()
split_routes_to_segments()
aggregate_segment_traffic()
```

## Project design

The package is the reusable toolkit. The `scripts/` files are intentionally thin and mainly do input/output orchestration. This follows the supervisor's advice: generalise the core routing and aggregation functions, and use the NZ 2023 commute analysis only as a demonstration of the tools.
