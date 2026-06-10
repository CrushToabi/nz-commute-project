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
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(scales)

year <- 2023
map_name <- "New Zealand"
top_n_per_direction <- as.integer(Sys.getenv("NZCOMMUTE_MAP_TOP_N", "20000"))
if (is.na(top_n_per_direction) || top_n_per_direction < 1) top_n_per_direction <- 20000

out_dir <- "visualizations"
dir.create(out_dir, showWarnings = FALSE)

map_bbox <- c(
  xmin = 166.0,
  xmax = 179.5,
  ymin = -47.5,
  ymax = -34.0
)

city_boxes <- list(
  NZ = map_bbox,
  AKL = c(xmin = 174.55, xmax = 175.05, ymin = -37.15, ymax = -36.65),
  WLG = c(xmin = 174.55, xmax = 175.05, ymin = -41.45, ymax = -41.05),
  CHC = c(xmin = 172.35, xmax = 172.85, ymin = -43.75, ymax = -43.35)
)

read_national_traffic <- function(direction, top_n = top_n_per_direction) {
  f <- paste0(
    "artifacts/commuter-traffic-segments-",
    year,
    "-pop-weighted-points-",
    direction,
    ".rds"
  )
  
  if (!file.exists(f)) {
    warning("Cannot find file for ", direction, ": ", f)
    return(st_sf(direction = character(), geometry = st_sfc(crs = 4326)))
  }

  x <- readRDS(f) |>
    st_transform(4326) |>
    filter(
      !is.na(commuter_traffic),
      commuter_traffic > 0
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
  
  st_simplify(x, dTolerance = 0.00005, preserveTopology = TRUE)
}

traffic_h2w <- read_national_traffic("H2W")
traffic_w2h <- read_national_traffic("W2H")

cat("H2W segments:", nrow(traffic_h2w), "\n")
cat("W2H segments:", nrow(traffic_w2h), "\n")
cat("Map:", map_name, "\n")
cat("Top segments per direction:", top_n_per_direction, "\n")

traffic_values <- log(c(traffic_h2w$commuter_traffic, traffic_w2h$commuter_traffic))
traffic_palette <- colorNumeric(
  palette = c("#2FBF71", "#F4D35E", "#F39C12", "#E85D3F"),
  domain = traffic_values,
  na.color = "#CCCCCC"
)

city_button_js <- paste0(
  "function(el, x) {",
  "  var map = this;",
  "  var bounds = {",
  "    NZ: [[", city_boxes$NZ["ymin"], ",", city_boxes$NZ["xmin"], "], [", city_boxes$NZ["ymax"], ",", city_boxes$NZ["xmax"], "]],",
  "    AKL: [[", city_boxes$AKL["ymin"], ",", city_boxes$AKL["xmin"], "], [", city_boxes$AKL["ymax"], ",", city_boxes$AKL["xmax"], "]],",
  "    WLG: [[", city_boxes$WLG["ymin"], ",", city_boxes$WLG["xmin"], "], [", city_boxes$WLG["ymax"], ",", city_boxes$WLG["xmax"], "]],",
  "    CHC: [[", city_boxes$CHC["ymin"], ",", city_boxes$CHC["xmin"], "], [", city_boxes$CHC["ymax"], ",", city_boxes$CHC["xmax"], "]]",
  "  };",
  "  var control = L.control({position: 'topleft'});",
  "  control.onAdd = function() {",
  "    var div = L.DomUtil.create('div', 'city-zoom-control leaflet-bar');",
  "    div.innerHTML = '<button type=\"button\" data-city=\"NZ\">NZ</button>' +",
  "      '<button type=\"button\" data-city=\"AKL\">AKL</button>' +",
  "      '<button type=\"button\" data-city=\"WLG\">WLG</button>' +",
  "      '<button type=\"button\" data-city=\"CHC\">CHC</button>';",
  "    L.DomEvent.disableClickPropagation(div);",
  "    return div;",
  "  };",
  "  control.addTo(map);",
  "  var style = document.createElement('style');",
  "  style.textContent = '.city-zoom-control { background: white; margin-top: 8px; }' +",
  "    '.city-zoom-control button { display: block; width: 42px; height: 28px; border: 0; border-bottom: 1px solid #ccc; background: white; font: 700 12px/1.1 system-ui, -apple-system, BlinkMacSystemFont, sans-serif; cursor: pointer; }' +",
  "    '.city-zoom-control button:last-child { border-bottom: 0; }' +",
  "    '.city-zoom-control button:hover { background: #f0f0f0; }';",
  "  document.head.appendChild(style);",
  "  el.querySelectorAll('.city-zoom-control button').forEach(function(button) {",
  "    button.addEventListener('click', function(event) {",
  "      var city = event.currentTarget.getAttribute('data-city');",
  "      map.fitBounds(bounds[city], {padding: [20, 20]});",
  "    });",
  "  });",
  "}"
)

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  fitBounds(
    lng1 = unname(map_bbox["xmin"]),
    lat1 = unname(map_bbox["ymin"]),
    lng2 = unname(map_bbox["xmax"]),
    lat2 = unname(map_bbox["ymax"])
  )

if (nrow(traffic_h2w) > 0) {
  m <- m |>
    addPolylines(
      data = traffic_h2w,
      group = "H2W: Home to Work",
      color = ~traffic_palette(log(commuter_traffic)),
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
      color = ~traffic_palette(log(commuter_traffic)),
      weight = ~line_width,
      opacity = 0.55,
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
  ) |>
  addLegend(
    position = "bottomright",
    pal = traffic_palette,
    values = traffic_values,
    title = "log(Commuter traffic)",
    opacity = 0.85,
    labFormat = labelFormat(digits = 1)
  ) |>
  addScaleBar(position = "bottomleft") |>
  htmlwidgets::onRender(city_button_js)

out_file <- file.path(
  out_dir,
  paste0(
    "interactive-traffic-map-",
    gsub("[^a-z0-9]+", "-", tolower(map_name)),
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
