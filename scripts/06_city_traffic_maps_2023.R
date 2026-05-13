library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(scales)

year <- 2023
city_name <- "Auckland"

out_dir <- "visualizations"
dir.create(out_dir, showWarnings = FALSE)

city_boxes <- list(
  Auckland = c(xmin = 174.55, xmax = 175.05, ymin = -37.15, ymax = -36.65),
  Wellington = c(xmin = 174.55, xmax = 175.05, ymin = -41.45, ymax = -41.05),
  Christchurch = c(xmin = 172.35, xmax = 172.85, ymin = -43.75, ymax = -43.35)
)

bbox <- city_boxes[[city_name]]

make_bbox_poly <- function(bbox) {
  st_as_sfc(
    st_bbox(
      c(
        xmin = bbox["xmin"],
        ymin = bbox["ymin"],
        xmax = bbox["xmax"],
        ymax = bbox["ymax"]
      ),
      crs = st_crs(4326)
    )
  )
}

read_city_traffic <- function(direction, bbox, top_n = 2500) {
  
  f <- paste0(
    "artifacts/commuter-traffic-segments-",
    year,
    "-pop-weighted-points-",
    direction,
    ".rds"
  )
  
  if (!file.exists(f)) {
    stop("Cannot find file: ", f)
  }
  
  city_poly <- make_bbox_poly(bbox)
  
  x <- readRDS(f) |>
    st_transform(4326) |>
    filter(
      !is.na(commuter_traffic),
      commuter_traffic > 0
    )
  
  x <- suppressWarnings(
    st_filter(x, city_poly, .predicate = st_intersects)
  )
  
  if (nrow(x) == 0) {
    warning("No segments found for ", direction)
    return(x)
  }
  
  x <- x |>
    arrange(desc(commuter_traffic)) |>
    slice_head(n = min(top_n, nrow(x))) |>
    mutate(
      direction = direction,
      popup_text = paste0(
        "<b>Direction:</b> ", direction, "<br>",
        "<b>Estimated commuters:</b> ", comma(round(commuter_traffic, 1)), "<br>",
        "<b>Route count:</b> ", comma(route_count), "<br>",
        "<b>Rank:</b> ", rank
      ),
      label_text = paste0(direction, ": ", comma(round(commuter_traffic, 1))),
      line_width = rescale(log1p(commuter_traffic), to = c(0.4, 3.0))
    )
  
  st_simplify(x, dTolerance = 0.00002, preserveTopology = TRUE)
}

traffic_h2w <- read_city_traffic("H2W", bbox, top_n = 2500)
traffic_w2h <- read_city_traffic("W2H", bbox, top_n = 2500)

cat("H2W segments:", nrow(traffic_h2w), "\n")
cat("W2H segments:", nrow(traffic_w2h), "\n")

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  fitBounds(
    lng1 = bbox["xmin"],
    lat1 = bbox["ymin"],
    lng2 = bbox["xmax"],
    lat2 = bbox["ymax"]
  )

if (nrow(traffic_h2w) > 0) {
  m <- m |>
    addPolylines(
      data = traffic_h2w,
      group = "H2W: Home to Work",
      color = "#4B00FF",
      weight = ~line_width,
      opacity = 0.75,
      popup = ~popup_text,
      label = ~label_text
    )
}

if (nrow(traffic_w2h) > 0) {
  m <- m |>
    addPolylines(
      data = traffic_w2h,
      group = "W2H: Work to Home",
      color = "#E74C3C",
      weight = ~line_width,
      opacity = 0.65,
      popup = ~popup_text,
      label = ~label_text
    )
}

m <- m |>
  addLayersControl(
    overlayGroups = c(
      "H2W: Home to Work",
      "W2H: Work to Home"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

out_file <- file.path(
  out_dir,
  paste0(
    "interactive-traffic-map-",
    tolower(city_name),
    "-",
    year,
    "-H2W-W2H.html"
  )
)

saveWidget(
  m,
  file = out_file,
  selfcontained = FALSE
)

cat("Saved:", out_file, "\n")

m