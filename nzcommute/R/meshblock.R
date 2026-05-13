#' Prepare Meshblock Population Geometry
#'
#' Joins meshblock geometry with population data and creates a cleaned sf object.
#'
#' @param pop_file Path to meshblock population CSV.
#' @param mb_file Path to meshblock geometry CSV containing a WKT column.
#' @param target_crs Target CRS, typically the CRS of the SA2 shapefile.
#' @param meshblock_id_col Meshblock ID column used for joining.
#' @param wkt_col WKT geometry column.
#' @return An sf object with a `total_population` column.
#' @export
prepare_meshblock_population <- function(pop_file,
                                         mb_file,
                                         target_crs,
                                         meshblock_id_col = "MB2025_V1_00",
                                         wkt_col = "WKT") {
  pop <- readr::read_csv(pop_file, show_col_types = FALSE)
  mb <- readr::read_csv(mb_file, show_col_types = FALSE)

  if (!meshblock_id_col %in% names(pop)) {
    stop("Population file does not contain ", meshblock_id_col, call. = FALSE)
  }
  if (!meshblock_id_col %in% names(mb)) {
    stop("Meshblock geometry file does not contain ", meshblock_id_col, call. = FALSE)
  }
  if (!wkt_col %in% names(mb)) {
    stop("Meshblock geometry file does not contain ", wkt_col, call. = FALSE)
  }

  pop_clean <- pop |>
    dplyr::mutate(
      General_Electoral_Population = ifelse(
        is.na(.data$General_Electoral_Population) | .data$General_Electoral_Population < 0,
        0,
        .data$General_Electoral_Population
      ),
      Māori_Electoral_Population = ifelse(
        is.na(.data$Māori_Electoral_Population) | .data$Māori_Electoral_Population < 0,
        0,
        .data$Māori_Electoral_Population
      ),
      total_population = .data$General_Electoral_Population + .data$Māori_Electoral_Population
    )

  mb |>
    dplyr::filter(!is.na(.data[[wkt_col]]), .data[[wkt_col]] != "") |>
    sf::st_as_sf(wkt = wkt_col, crs = 2193) |>
    sf::st_make_valid() |>
    dplyr::left_join(pop_clean, by = meshblock_id_col) |>
    dplyr::mutate(
      total_population = ifelse(
        is.na(.data$total_population) | .data$total_population < 0,
        0,
        .data$total_population
      )
    ) |>
    sf::st_transform(target_crs)
}

#' Assign Meshblocks to SA2 Areas
#'
#' Assigns each meshblock to an SA2 using the centroid of the meshblock geometry.
#'
#' @param meshblock_sf Meshblock sf object.
#' @param sa2_sf SA2 sf object.
#' @param sa2_col Name of SA2 code column in `sa2_sf`.
#' @param output_col Name of the new meshblock SA2 column.
#' @return Meshblock sf object with SA2 code attached.
#' @export
assign_meshblocks_to_sa2 <- function(meshblock_sf,
                                     sa2_sf,
                                     sa2_col = "SA22023_V1",
                                     output_col = "SA2_CODE") {
  if (!sa2_col %in% names(sa2_sf)) {
    stop("SA2 shapefile does not contain ", sa2_col, call. = FALSE)
  }

  mb_cent <- sf::st_centroid(sf::st_geometry(meshblock_sf))
  idx <- sf::st_within(mb_cent, sa2_sf)

  meshblock_sf[[output_col]] <- NA_character_
  sa2_codes <- as.character(sa2_sf[[sa2_col]])

  for (i in seq_along(idx)) {
    if (length(idx[[i]]) > 0) {
      meshblock_sf[[output_col]][i] <- sa2_codes[idx[[i]][1]]
    }
  }

  meshblock_sf |>
    dplyr::filter(!is.na(.data[[output_col]]))
}

#' Create Population-Weighted Point Sampler
#'
#' Creates a closure that samples random points inside meshblocks within an SA2,
#' using meshblock population as sampling probability.
#'
#' @param meshblock_sf Meshblock sf object with SA2 and population columns.
#' @param sa2_sf SA2 sf object used as fallback geometry.
#' @param sa2_col SA2 code column in `sa2_sf`.
#' @param meshblock_sa2_col SA2 code column in `meshblock_sf`.
#' @param meshblock_id_col Meshblock ID column.
#' @param population_col Population weight column.
#' @return A function that takes one SA2 code and returns sampled point metadata.
#' @export
create_population_weighted_sampler <- function(meshblock_sf,
                                               sa2_sf,
                                               sa2_col = "SA22023_V1",
                                               meshblock_sa2_col = "SA2_CODE",
                                               meshblock_id_col = "MB2025_V1_00",
                                               population_col = "total_population") {
  if (!meshblock_sa2_col %in% names(meshblock_sf)) {
    stop("meshblock_sf does not contain ", meshblock_sa2_col, call. = FALSE)
  }
  if (!population_col %in% names(meshblock_sf)) {
    stop("meshblock_sf does not contain ", population_col, call. = FALSE)
  }
  if (!sa2_col %in% names(sa2_sf)) {
    stop("sa2_sf does not contain ", sa2_col, call. = FALSE)
  }

  meshblock_by_sa2 <- split(meshblock_sf, meshblock_sf[[meshblock_sa2_col]])
  sa2_lookup <- sa2_sf
  rownames(sa2_lookup) <- as.character(sa2_lookup[[sa2_col]])

  function(sa2_code) {
    sub <- meshblock_by_sa2[[as.character(sa2_code)]]

    if (is.null(sub) || nrow(sub) == 0) {
      poly <- sa2_lookup[as.character(sa2_code), ]
      pt <- tryCatch(sf::st_sample(poly, size = 1, type = "random"), error = function(e) NULL)
      if (is.null(pt) || length(pt) == 0) pt <- sf::st_point_on_surface(poly)

      pt_sf <- sf::st_sf(geometry = sf::st_sfc(pt, crs = sf::st_crs(sa2_sf)))
      xy <- sf::st_coordinates(sf::st_transform(pt_sf, 4326))

      return(data.frame(
        lon = as.numeric(xy[1, 1]),
        lat = as.numeric(xy[1, 2]),
        mb_code = NA_character_,
        mb_population = NA_real_,
        sa2_population = NA_real_,
        mb_prob = 1
      ))
    }

    w <- sub[[population_col]]
    w[is.na(w) | w < 0] <- 0
    if (sum(w) <= 0) w <- rep(1, nrow(sub))

    selected <- sample(seq_len(nrow(sub)), size = 1, prob = w)
    poly <- sub[selected, ]

    pt <- tryCatch(sf::st_sample(poly, size = 1, type = "random"), error = function(e) NULL)
    if (is.null(pt) || length(pt) == 0) pt <- sf::st_point_on_surface(poly)

    pt_sf <- sf::st_sf(geometry = sf::st_sfc(pt, crs = sf::st_crs(sa2_sf)))
    xy <- sf::st_coordinates(sf::st_transform(pt_sf, 4326))

    sa2_pop <- sum(w, na.rm = TRUE)
    mb_pop <- poly[[population_col]][1]
    if (is.na(mb_pop) || mb_pop < 0) mb_pop <- 0

    data.frame(
      lon = as.numeric(xy[1, 1]),
      lat = as.numeric(xy[1, 2]),
      mb_code = if (meshblock_id_col %in% names(poly)) as.character(poly[[meshblock_id_col]][1]) else NA_character_,
      mb_population = as.numeric(mb_pop),
      sa2_population = as.numeric(sa2_pop),
      mb_prob = as.numeric(ifelse(sa2_pop > 0, mb_pop / sa2_pop, 1 / nrow(sub)))
    )
  }
}
