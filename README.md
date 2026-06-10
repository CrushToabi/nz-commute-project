# NZ Commute Project  
### Population-Weighted Commuter Traffic Simulation using GraphHopper and New Zealand Census Data

---

## Overview

This project extends spatial routing workflows in R using New Zealand commuter traffic as a real-world use case.

The project combines:

- 2023 New Zealand Census Journey-to-Work data
- population-weighted meshblock sampling
- OpenStreetMap road networks
- GraphHopper routing
- stochastic representative route simulation
- weighted traffic aggregation

to estimate realistic commuter traffic flows across the New Zealand road network.

Unlike traditional centroid-to-centroid routing approaches, this project generates multiple representative commuter routes for each Origin–Destination (OD) pair using adaptive sampling and meshblock-level population weighting.

The result is a scalable framework for:

- commuter traffic estimation
- road-segment traffic aggregation
- mobility graph analysis
- directional commute modelling
- interactive traffic visualisation

---

# Key Features

- Population-weighted meshblock sampling
- Adaptive OD route sampling
- Probability-based route weighting
- Directional commute simulation (H2W / W2H)
- GraphHopper road-network routing
- Road-segment traffic aggregation
- Interactive Leaflet traffic maps
- Reusable `nzcommute` R package structure

---

# Core Methodology

The methodology combines three levels of weighting:

1. **OD-flow weighting**  
   Census commuter counts determine traffic intensity between SA2 regions and dynamically control the number of representative route samples.

2. **Population-weighted spatial sampling**  
   Meshblock population distributions determine where representative origin and destination points are sampled within each SA2.

3. **Probability-based route weighting**  
   Each sampled route receives a proportional traffic weight based on the product of origin and destination meshblock probabilities.

This separates:

```text
traffic intensity
from
spatial route representation
```

allowing realistic large-scale commuter traffic estimation across New Zealand.

---

# Methodology

---

## Step 1 — Construct Origin–Destination Commuter Flows

The workflow begins with the 2023 Census Journey-to-Work dataset.

Each row represents:

```text
Residence SA2 -> Workplace SA2
```

Car-based commuter counts are calculated as:

```text
commuter_count =
    private_car +
    company_car
```

Rows are retained only if:

- origin and destination SA2 are different;
- both SA2 codes exist in the 2023 SA2 shapefile;
- commuter count is greater than zero.

Example:

| Origin SA2 | Destination SA2 | commuter_count |
|---|---|---|
| A | B | 850 |

This means approximately 850 commuters travel daily from SA2 A to SA2 B.

---

## Step 2 — Population-Weighted Meshblock Sampling

Instead of routing between SA2 centroids, each SA2 is decomposed into meshblocks.

Each meshblock receives a sampling probability proportional to its population.

Example:

```text
SA2 A
 ├── MB1 population = 100
 ├── MB2 population = 500
 ├── MB3 population = 50
```

Sampling probabilities:

```text
P(MB1) = 100 / 650
P(MB2) = 500 / 650
P(MB3) =  50 / 650
```

Thus, densely populated meshblocks are more likely to generate commuter origins and destinations.

Within the selected meshblock polygon:

1. a random point is sampled;
2. if sampling fails, `st_point_on_surface()` is used as fallback.

This process is applied independently to both origins and destinations.

---

## Step 3 — Adaptive OD Route Sampling

Instead of generating a single route per OD pair, the workflow dynamically generates multiple representative routes.

The number of route samples depends on commuter volume:

```text
choose_n_samples(commuter_count)
```

Example:

| commuter_count | representative routes |
|---|---|
| 20 | 1 |
| 120 | 3 |
| 350 | 7 |
| 950 | 15 |

This creates richer spatial representation for large commuter flows while avoiding unnecessary computation for small flows.

Conceptually, this behaves like an adaptive Monte Carlo commuter simulation.

The workflow evolves from:

```text
one OD pair -> one centroid route
```

to:

```text
one OD pair -> multiple population-weighted representative routes
```

---

## Step 4 — Probability-Based Route Weighting

Each sampled route inherits probabilities from:

- origin meshblock population share;
- destination meshblock population share.

For route \(i\):

```text
raw_route_probability =
    origin_meshblock_probability
    *
    destination_meshblock_probability
```

The OD commuter count is redistributed proportionally:

```text
route_weight_i =
    commuter_count *
    route_probability_i /
    sum(all_route_probabilities)
```

This ensures:

- population-dense areas generate higher commuter traffic;
- total commuter volume remains consistent with Census data.

Importantly:

```text
sum(route_weight_i)
=
original commuter_count
```

Therefore, increasing route samples improves realism without artificially inflating traffic volume.

---

## Step 5 — Directional Routing with GraphHopper

The routing system supports:

```r
direction <- "H2W"   # home to work
direction <- "W2H"   # work to home
```

Routing uses:

```text
osm/new-zealand-latest.osm.pbf
```

through GraphHopper via the `ghroute` package.

Each sampled route becomes:

```text
sampled home point
        ->
sampled workplace point
```

using realistic road-network routing rather than straight-line geometry.

Outputs include:

```text
artifacts/routes-car-2023-pop-weighted-points-H2W.rds
artifacts/routes-car-2023-pop-weighted-points-W2H.rds
artifacts/transitions-2023-pop-weighted-points-H2W.rds
artifacts/transitions-2023-pop-weighted-points-W2H.rds
```

---

## Step 6 — Road-Segment Traffic Aggregation

All routes are decomposed into individual road segments.

Each segment inherits:

```text
route_weight
```

Traffic aggregation proceeds as:

```text
all weighted routes
    ->
road segments
    ->
weighted aggregation
```

Segments traversed by many high-weight routes accumulate larger estimated commuter traffic.

This produces a network-wide commuter traffic intensity estimate.

---

## Step 7 — Spatial Summaries and Visualisation

The weighted routing outputs support:

- interactive traffic maps
- SA2 transition matrices
- morning vs evening commute comparison

The final framework functions as a:

```text
population-weighted stochastic commute simulation system
```

for large-scale commuter traffic estimation.

---

# Repository Structure

```text
nz-commute-project/
│
├── artifacts/                 # Generated outputs; ignored except metadata
│
├── data/                      # Raw census and spatial datasets; ignored
│
├── nzcommute/                 # Reusable R package
│   ├── R/
│   ├── man/
│   ├── inst/
│   ├── DESCRIPTION
│   ├── NAMESPACE
│   └── README.md
│
├── osm/                       # OSM download tools; .pbf files ignored
│
├── scripts/                   # Main routing workflow scripts
│   ├── 01_sample_2023.R
│   ├── 02_route_2023_directional.R
│   ├── 03_string_2023_directional.R
│   ├── 04_aggr_2023_directional.R
│   ├── 05_traffic_segments_2023_directional.R
│   └── 06_city_traffic_maps_2023.R
│
├── visualizations/            # Generated maps and outputs
│
├── LICENSE.md
├── Projects.Rproj
└── README.md
```

---

# Data Sources

Large raw data files and generated artifacts are not committed to this GitHub
repository. Download the source datasets into `data/`, run `make -C osm` to
obtain the OpenStreetMap PBF file, and regenerate outputs with the scripts.

## 1. Journey-to-Work Census Data

2023 Census commuting flows between Statistical Area 2 regions.

Source:

```text
https://datafinder.stats.govt.nz/table/121988-2023-census-main-means-of-travel-to-work-by-statistical-area-2/
```

Used for:

- commuter OD flows
- commuter counts
- directional commute analysis

---

## 2. Meshblock Electoral-Population Data

Source:

```text
https://datafinder.stats.govt.nz/layer/121975-2023-census-electoral-population-at-meshblock-level-2025-meshblock/
```

Used for:

- meshblock geometry polygons
- meshblock electoral-population weights
- population-weighted point sampling
- realistic commuter origin/destination generation

---

## 3. OpenStreetMap Road Network

Road-network routing data downloaded from OpenStreetMap / Geofabrik.

Used through:

- GraphHopper
- ghroute R package

---


# Technologies Used

- R
- sf
- dplyr
- leaflet
- htmlwidgets
- scales
- GraphHopper
- ghroute
- OpenStreetMap


---

# Author

Yuxin Zhang  
University of Auckland  
Data Science 
