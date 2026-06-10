#' Read Journey-to-Work File
#'
#' Reads a Census Journey-to-Work CSV file using UTF-8 BOM handling.
#'
#' @param jtw_file Path to the Journey-to-Work CSV file.
#' @return A data frame.
#' @export
read_jtw_file <- function(jtw_file) {
  if (!file.exists(jtw_file)) {
    stop("Cannot find Journey-to-Work file: ", jtw_file, call. = FALSE)
  }
  utils::read.csv(jtw_file, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
}

#' Prepare Generic OD Flow Data
#'
#' Cleans an origin-destination table, combines one or more count columns, and
#' returns a standard OD flow table.
#'
#' @param data Input OD data frame.
#' @param origin_col Origin ID column.
#' @param dest_col Destination ID column.
#' @param count_cols One or more numeric flow-count columns.
#' @param valid_origin_ids Optional vector of valid origin IDs.
#' @param valid_dest_ids Optional vector of valid destination IDs.
#' @param reverse If `TRUE`, swaps origin and destination in the output.
#' @param direction Optional direction label to attach to the output.
#' @param count_col Output flow-count column name.
#' @return A standard OD data frame with `origin_id`, `dest_id`, and `count_col`.
#' @export
prepare_od_data <- function(data,
                            origin_col,
                            dest_col,
                            count_cols,
                            valid_origin_ids = NULL,
                            valid_dest_ids = NULL,
                            reverse = FALSE,
                            direction = NULL,
                            count_col = "flow_count") {
  required <- c(origin_col, dest_col, count_cols)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!isTRUE(length(count_cols) > 0)) {
    stop("count_cols must contain at least one column", call. = FALSE)
  }

  out <- data |>
    dplyr::mutate(
      origin_raw = as.character(.data[[origin_col]]),
      dest_raw = as.character(.data[[dest_col]])
    )

  count_values <- lapply(count_cols, function(col) {
    x <- out[[col]]
    x[is.na(x) | x < 0] <- 0
    x
  })
  out[[count_col]] <- Reduce(`+`, count_values)

  if (!is.null(valid_origin_ids)) {
    out <- out |>
      dplyr::filter(.data$origin_raw %in% valid_origin_ids)
  }
  if (!is.null(valid_dest_ids)) {
    out <- out |>
      dplyr::filter(.data$dest_raw %in% valid_dest_ids)
  }

  out <- out |>
    dplyr::filter(
      .data$origin_raw != .data$dest_raw,
      .data[[count_col]] > 0
    ) |>
    dplyr::mutate(original_od_id = dplyr::row_number())

  if (isTRUE(reverse)) {
    out$origin_id <- out$dest_raw
    out$dest_id <- out$origin_raw
  } else {
    out$origin_id <- out$origin_raw
    out$dest_id <- out$dest_raw
  }

  if (!is.null(direction)) {
    out$direction <- check_direction(direction)
  }

  out
}

#' Prepare Journey-to-Work OD Data
#'
#' Convenience wrapper for the NZ 2023 Journey-to-Work use case. It computes car
#' commuter counts and returns the legacy column names used by the scripts.
#'
#' @param jtw Journey-to-Work data frame.
#' @param valid_sa2 Character vector of valid SA2 codes.
#' @param direction Direction label, usually `"H2W"` or `"W2H"`.
#' @param home_col Column name for usual residence SA2.
#' @param work_col Column name for workplace SA2.
#' @param private_car_col Column name for private car counts.
#' @param company_car_col Column name for company car counts.
#' @return A directional OD data frame.
#' @export
prepare_jtw_od_data <- function(jtw,
                                valid_sa2,
                                direction = "H2W",
                                home_col = "SA22023_V1_00_usual_residence_address",
                                work_col = "SA22023_V1_00_workplace_address",
                                private_car_col = "X2023_Drive_a_private_car_truck_or_van",
                                company_car_col = "X2023_Drive_a_company_car_truck_or_van") {
  direction <- check_direction(direction, allowed = c("H2W", "W2H"))

  prepare_od_data(
    data = jtw,
    origin_col = home_col,
    dest_col = work_col,
    count_cols = c(private_car_col, company_car_col),
    valid_origin_ids = valid_sa2,
    valid_dest_ids = valid_sa2,
    reverse = direction == "W2H",
    direction = direction,
    count_col = "commuter_count"
  ) |>
    dplyr::mutate(
      home_sa2 = as.character(.data[[home_col]]),
      work_sa2 = as.character(.data[[work_col]]),
      origin_sa2 = .data$origin_id,
      dest_sa2 = .data$dest_id
    ) |>
    dplyr::filter(
      .data$home_sa2 != .data$work_sa2
    ) |>
    dplyr::mutate(
      private_car = ifelse(is.na(.data[[private_car_col]]) | .data[[private_car_col]] < 0, 0, .data[[private_car_col]]),
      company_car = ifelse(is.na(.data[[company_car_col]]) | .data[[company_car_col]] < 0, 0, .data[[company_car_col]])
    )
}

#' Choose Adaptive Number of Route Samples
#'
#' Computes the number of representative routes to generate for one OD pair.
#'
#' @param commuter_count Number of commuters for one OD pair.
#' @param min_samples Minimum number of sampled routes.
#' @param max_samples Maximum number of sampled routes.
#' @param commuters_per_sample Number of commuters represented by one sampled route.
#' @return Integer number of sampled routes.
#' @export
choose_n_samples <- function(commuter_count,
                             min_samples = 1,
                             max_samples = 15,
                             commuters_per_sample = 50) {
  n <- ceiling(commuter_count / commuters_per_sample)
  n <- max(min_samples, n)
  n <- min(max_samples, n)
  as.integer(n)
}

#' Expand OD Pairs into Sampled Route Rows
#'
#' Applies adaptive sampling to each OD pair and expands each row into one or
#' more representative route rows.
#'
#' @param od_data OD data frame containing `commuter_count`.
#' @param min_samples Minimum number of sampled routes.
#' @param max_samples Maximum number of sampled routes.
#' @param commuters_per_sample Number of commuters represented by one sampled route.
#' @return Expanded data frame with `n_samples` and `sample_id`.
#' @export
expand_od_samples <- function(od_data,
                              min_samples = 1,
                              max_samples = 15,
                              commuters_per_sample = 50) {
  if (!"commuter_count" %in% names(od_data)) {
    stop("od_data must contain commuter_count", call. = FALSE)
  }

  od_data$n_samples <- vapply(
    od_data$commuter_count,
    choose_n_samples,
    integer(1),
    min_samples = min_samples,
    max_samples = max_samples,
    commuters_per_sample = commuters_per_sample
  )

  expanded_index <- rep(seq_len(nrow(od_data)), times = od_data$n_samples)
  out <- od_data[expanded_index, ]
  out$sample_id <- unlist(lapply(od_data$n_samples, seq_len), use.names = FALSE)
  rownames(out) <- NULL
  out
}

#' Analyse Candidate Sample Rules
#'
#' Summarises the commuter-count distribution and compares several adaptive
#' route-sampling rules.
#'
#' @param od_data OD data frame containing `commuter_count`.
#' @param commuters_per_sample_values Candidate commuters-per-sample values.
#' @param max_samples_values Candidate maximum sample caps.
#' @param min_samples Minimum number of samples per OD pair.
#' @param target_routes Preferred total route-count interval.
#' @return A list with `summary`, `rules`, `recommended`, and `od_sample_size`.
#' @export
analyse_commuter_sample_rules <- function(od_data,
                                          commuters_per_sample_values = c(50, 75, 100, 150, 200),
                                          max_samples_values = c(5, 10, 15, 20),
                                          min_samples = 1,
                                          target_routes = c(40000, 80000)) {
  if (!"commuter_count" %in% names(od_data)) {
    stop("od_data must contain commuter_count", call. = FALSE)
  }

  commuter_summary <- od_data |>
    dplyr::summarise(
      n_od = dplyr::n(),
      total_commuters = sum(.data$commuter_count, na.rm = TRUE),
      min = min(.data$commuter_count, na.rm = TRUE),
      q10 = stats::quantile(.data$commuter_count, 0.10, na.rm = TRUE),
      q25 = stats::quantile(.data$commuter_count, 0.25, na.rm = TRUE),
      median = stats::median(.data$commuter_count, na.rm = TRUE),
      mean = mean(.data$commuter_count, na.rm = TRUE),
      q75 = stats::quantile(.data$commuter_count, 0.75, na.rm = TRUE),
      q90 = stats::quantile(.data$commuter_count, 0.90, na.rm = TRUE),
      q95 = stats::quantile(.data$commuter_count, 0.95, na.rm = TRUE),
      q99 = stats::quantile(.data$commuter_count, 0.99, na.rm = TRUE),
      max = max(.data$commuter_count, na.rm = TRUE)
    )

  candidate_rules <- expand.grid(
    commuters_per_sample = commuters_per_sample_values,
    max_samples = max_samples_values
  )

  rule_results <- lapply(seq_len(nrow(candidate_rules)), function(i) {
    cps <- candidate_rules$commuters_per_sample[i]
    max_s <- candidate_rules$max_samples[i]
    n_samples <- vapply(
      od_data$commuter_count,
      choose_n_samples,
      integer(1),
      min_samples = min_samples,
      max_samples = max_s,
      commuters_per_sample = cps
    )
    data.frame(
      commuters_per_sample = cps,
      min_samples = min_samples,
      max_samples = max_s,
      total_routes = sum(n_samples),
      mean_samples = mean(n_samples),
      median_samples = stats::median(n_samples),
      max_actual_samples = max(n_samples)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$total_routes)

  mid_target <- mean(target_routes)
  recommended <- rule_results |>
    dplyr::filter(.data$total_routes >= target_routes[1], .data$total_routes <= target_routes[2]) |>
    dplyr::arrange(abs(.data$total_routes - mid_target)) |>
    dplyr::slice(1)

  if (nrow(recommended) == 0) {
    recommended <- rule_results |>
      dplyr::arrange(abs(.data$total_routes - mid_target)) |>
      dplyr::slice(1)
  }

  best_cps <- recommended$commuters_per_sample[1]
  best_max <- recommended$max_samples[1]
  od_sample_size <- od_data |>
    dplyr::mutate(
      n_samples = vapply(
        .data$commuter_count,
        choose_n_samples,
        integer(1),
        min_samples = min_samples,
        max_samples = best_max,
        commuters_per_sample = best_cps
      )
    )

  list(
    summary = commuter_summary,
    rules = rule_results,
    recommended = recommended,
    od_sample_size = od_sample_size
  )
}

#' Save Sample Rule Analysis
#'
#' Saves outputs from `analyse_commuter_sample_rules()` to standard artifact paths.
#'
#' @param analysis Result from `analyse_commuter_sample_rules()`.
#' @param paths Named artifact paths from `build_artifact_paths()`.
#' @return Invisibly returns `analysis`.
#' @export
save_sample_rule <- function(analysis, paths) {
  utils::write.csv(analysis$summary, paths$commuter_summary_csv, row.names = FALSE)
  utils::write.csv(analysis$rules, paths$sample_rule_comparison_csv, row.names = FALSE)
  utils::write.csv(analysis$recommended, paths$sample_rule_csv, row.names = FALSE)
  utils::write.csv(analysis$od_sample_size, paths$od_sample_size_csv, row.names = FALSE)
  saveRDS(analysis$recommended, paths$sample_rule_rds)
  invisible(analysis)
}
