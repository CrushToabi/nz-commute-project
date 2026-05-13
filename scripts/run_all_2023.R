## Run the full directional use-case workflow.
## First run H2W, then W2H.

source("use-case/01_sample_2023.R")

for (direction in c("H2W", "W2H")) {
  source("use-case/02_route_2023_directional.R")
  source("use-case/03_string_2023_directional.R")
  source("use-case/04_aggr_2023_directional.R")
  source("use-case/05_traffic_segments_2023_directional.R")
}
