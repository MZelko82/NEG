# Center-based coordinate shift correction functions

#' Detect center zone points and calculate shift correction
#' @param data Dataframe containing X, Y coordinates and InCentre indicator
#' @param center_zone_radius Numeric radius around center to consider for initial filtering
#' @param eps DBSCAN clustering parameter for neighborhood size
#' @param min_pts Minimum points required to form a cluster
#' @return List containing shift values and diagnostic information
detect_center_shift <- function(data, center_zone_radius = 5, eps = 2, min_pts = 5) {
  require(dbscan)
  
  # Filter points in center zone using InCentre indicator
  center_points <- data %>%
    filter(InCentre == 1) %>%
    select(Time, X, Y)
  
  n_points <- nrow(center_points)
  
  # Handle cases with very few points differently
  if(n_points == 0) {
    warning("No center zone points found")
    return(list(
      x_shift = 0,
      y_shift = 0,
      success = FALSE,
      reason = "no_points",
      n_points = 0,
      method = "none",
      cluster_points = NULL,
      centroid = NULL,
      low_confidence = TRUE
    ))
  } else if(n_points < min_pts) {
    # For very few points, use simple averaging instead of clustering
    warning(sprintf("Only %d center zone points found, using simple averaging", n_points))
    
    cluster_points <- center_points %>%
      mutate(cluster = 1)
    
    centroid <- cluster_points %>%
      summarise(
        x_center = mean(X),
        y_center = mean(Y)
      )
    
    return(list(
      x_shift = -centroid$x_center,
      y_shift = -centroid$y_center,
      success = TRUE,
      n_points = n_points,
      cluster_points = cluster_points,
      centroid = centroid,
      method = "average",
      low_confidence = TRUE
    ))
  }
  
  # For cases with enough points, proceed with DBSCAN clustering
  clusters <- dbscan::dbscan(
    center_points %>% select(X, Y),
    eps = eps,
    minPts = min_pts
  )
  
  # If no clusters found, fall back to simple averaging
  if(max(clusters$cluster) == 0) {
    warning("No significant clusters found, falling back to simple averaging")
    
    cluster_points <- center_points %>%
      mutate(cluster = 1)
    
    centroid <- cluster_points %>%
      summarise(
        x_center = mean(X),
        y_center = mean(Y)
      )
    
    return(list(
      x_shift = -centroid$x_center,
      y_shift = -centroid$y_center,
      success = TRUE,
      n_points = n_points,
      cluster_points = cluster_points,
      centroid = centroid,
      method = "average",
      low_confidence = TRUE
    ))
  }
  
  # Find the largest cluster
  main_cluster <- which.max(table(clusters$cluster))
  cluster_points <- center_points %>%
    mutate(cluster = clusters$cluster) %>%
    filter(cluster == main_cluster)
  
  # Calculate centroid of main cluster
  centroid <- cluster_points %>%
    summarise(
      x_center = mean(X),
      y_center = mean(Y)
    )
  
  # Calculate required shift to move centroid to (0,0)
  x_shift <- -centroid$x_center
  y_shift <- -centroid$y_center
  
  return(list(
    x_shift = x_shift,
    y_shift = y_shift,
    success = TRUE,
    n_points = nrow(cluster_points),
    cluster_points = cluster_points,
    centroid = centroid,
    method = "cluster",
    low_confidence = FALSE
  ))
}

#' Apply shift correction to coordinates
apply_center_shift <- function(data, shift_values) {
  if(!shift_values$success) {
    warning("Using uncorrected coordinates due to unsuccessful shift detection")
    return(data %>% 
           mutate(
             x_m = X,
             y_m = Y
           ))
  }
  
  if(shift_values$low_confidence) {
    warning(sprintf("Low confidence correction using %s method with %d points", 
                   shift_values$method, shift_values$n_points))
  }
  
  data %>%
    mutate(
      x_m = X + shift_values$x_shift,
      y_m = Y + shift_values$y_shift
    )
}

#' Read and clean EPM trial data
read_and_clean_data <- function(file_path, head_skip, data_skip, meta_range, ...) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Check file extension
  if (!grepl("\\.xlsx$", file_path)) {
    stop("File must be an Excel (.xlsx) file")
  }
  
  cat("\nProcessing file:", basename(file_path), "\n")


  # Read metadata
  metadata <- tryCatch({
    cat("Reading metadata...\n")
    meta_values <- readxl::read_xlsx(file_path, range = meta_range, col_names = FALSE)
    parse_meta_range <- function(range_str) {
      match <- regexec("^([A-Za-z]+)(\\d+):([A-Za-z]+)(\\d+)$", range_str)
      parts <- regmatches(range_str, match)[[1]]
      if (length(parts) == 0) {
        stop("meta_range must be in Excel A1:B2 format")
      }
      list(
        start_col = toupper(parts[2]),
        start_row = as.integer(parts[3]),
        end_col = toupper(parts[4]),
        end_row = as.integer(parts[5])
      )
    }
    
    col_to_num <- function(col) {
      letters <- strsplit(col, "")[[1]]
      num <- 0
      for (ch in letters) {
        num <- num * 26 + match(ch, LETTERS)
      }
      num
    }
    
    num_to_col <- function(num) {
      if (num <= 0) stop("Column index must be positive")
      letters <- c()
      while (num > 0) {
        rem <- (num - 1) %% 26
        letters <- c(LETTERS[rem + 1], letters)
        num <- (num - 1) %/% 26
      }
      paste(letters, collapse = "")
    }
    
    parsed <- parse_meta_range(meta_range)
    if (parsed$start_col != parsed$end_col) {
      stop("meta_range must be a single column (e.g., B32:B35)")
    }
    
    left_col_num <- col_to_num(parsed$start_col) - 1
    if (left_col_num <= 0) {
      stop("Metadata range must not start in the first column to read left-adjacent labels")
    }
    
    left_col <- num_to_col(left_col_num)
    meta_left_range <- paste0(left_col, parsed$start_row, ":", left_col, parsed$end_row)
    meta_labels <- readxl::read_xlsx(file_path, range = meta_left_range, col_names = FALSE)
    
    if (!all(dim(meta_values) == dim(meta_labels))) {
      stop("Metadata values and left-adjacent labels ranges are mismatched")
    }
    
    meta_df <- tibble(
      field = as.character(unlist(meta_labels, use.names = FALSE)),
      value = as.character(unlist(meta_values, use.names = FALSE))
    ) %>%
      mutate(field = trimws(field)) %>%
      filter(!is.na(field), field != "")
    
    if (nrow(meta_df) == 0) {
      stop("No metadata labels found in left-adjacent cells")
    }
    if (anyDuplicated(meta_df$field)) {
      stop("Duplicate metadata labels found in left-adjacent cells")
    }
    
    meta_wide <- meta_df %>%
      pivot_wider(names_from = field, values_from = value)
    
    normalize_meta_name <- function(x) {
      x <- trimws(x)
      x <- gsub("[^A-Za-z0-9]+", "_", x)
      x <- gsub("^_+|_+$", "", x)
      x <- gsub("_+", "_", x)
      tolower(x)
    }
    
    meta_wide %>%
      rename_with(normalize_meta_name)
    
  }, error = function(e) {
    cat("ERROR reading metadata:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(metadata)) {
    stop("Failed to read metadata")
  }
  

  # Read time series data
  data <- tryCatch({
    cat("Reading time series data...\n")
    header_candidates <- unique(c(head_skip, head_skip - 1, head_skip + 1, head_skip - 2, head_skip + 2))
    header_candidates <- header_candidates[header_candidates >= 0]
    data_offset <- data_skip - head_skip
    header_skip <- head_skip
    data_start_skip <- data_skip
    headers <- NULL
    
    for (candidate in header_candidates) {
      candidate_headers <- readxl::read_xlsx(
        file_path,
        skip = candidate,
        n_max = 1,
        .name_repair = "minimal"
      )
      candidate_names <- names(candidate_headers)
      if (all(c("Trial time", "X center", "Y center") %in% candidate_names)) {
        headers <- candidate_headers
        header_skip <- candidate
        data_start_skip <- candidate + data_offset
        break
      }
    }
    
    if (is.null(headers)) {
      headers <- readxl::read_xlsx(file_path, skip = head_skip, n_max = 1, .name_repair = "minimal")
    }
    
    data <- readxl::read_xlsx(file_path, 
                             skip = data_start_skip,
                             col_names = names(headers),
                             na = c("", "-", "NA"),
                             .name_repair = "minimal")
    
    # Normalize column names early to avoid case mismatches
    data <- data %>%
      rename_with(~tolower(.x))
    
    # Clean the data
    data <- data %>%
      mutate(across(c(`trial time`, `x center`, `y center`), 
                   ~as.numeric(ifelse(. == "-", NA, .))))
    
    # Check for required columns
    actual_cols <- names(data)
    missing_cols <- c()
    
    if(!"trial time" %in% actual_cols) missing_cols <- c(missing_cols, "Trial time")
    if(!"x center" %in% actual_cols) missing_cols <- c(missing_cols, "X center")
    if(!"y center" %in% actual_cols) missing_cols <- c(missing_cols, "Y center")
    
    open_arm_cols <- c("In zone(open_arm_1 / center-point)", "In zone(open_arm_2 / center-point)")
    closed_arm_cols <- c("In zone(closed_arm_1 / center-point)", "In zone(closed_arm_2 / center-point)")
    
    open_arm_cols_norm <- tolower(open_arm_cols)
    closed_arm_cols_norm <- tolower(closed_arm_cols)
    
    has_open_arms <- all(open_arm_cols_norm %in% actual_cols)
    has_closed_arms <- all(closed_arm_cols_norm %in% actual_cols)
    
    if(!has_open_arms) missing_cols <- c(missing_cols, "open arm zones")
    if(!has_closed_arms) missing_cols <- c(missing_cols, "closed arm zones")
    
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    if (nrow(data) == 0) {
      stop("No time series data found in file")
    }
    
    # Convert zone columns to numeric
    data <- data %>%
      mutate(across(starts_with("in zone"), ~as.numeric(ifelse(. == "-", 0, .))))
    
    # Create InOpen and InClosed columns
    data <- data %>%
      mutate(
        InOpen = case_when(
          `in zone(open_arm_1 / center-point)` == 1 | `in zone(open_arm_2 / center-point)` == 1 ~ 1,
          TRUE ~ 0
        ),
        InClosed = case_when(
          `in zone(closed_arm_1 / center-point)` == 1 | `in zone(closed_arm_2 / center-point)` == 1 ~ 1,
          TRUE ~ 0
        ),
        InCentre = case_when(
          `in zone(centre_zone / center-point)` == 1 ~ 1,
          TRUE ~ 0
        )
      )
    
    # Create clean data frame
    data <- tibble(
      Time = as.numeric(data$`trial time`),
      X = as.numeric(data$`x center`),
      Y = as.numeric(data$`y center`),
      InOpen = data$InOpen,
      InClosed = data$InClosed,
      InCentre = data$InCentre
    ) %>%
      filter(!is.na(X), !is.na(Y))
    
    # Apply center-based shift correction
    tryCatch({
      shift_values <- detect_center_shift(data)
      
      if (!shift_values$success) {
        data <- data %>%
          mutate(x_m = X, y_m = Y)
      } else {
        data <- apply_center_shift(data, shift_values)
      }
      
      # Ensure proper column structure
      data <- data %>%
        select(Time, X, Y, InOpen, InClosed, InCentre, x_m, y_m)
      
    }, error = function(e) {
      cat("\nERROR in shift correction:", conditionMessage(e), "\n")
      data <- data %>%
        mutate(x_m = X, y_m = Y) %>%
        select(Time, X, Y, InOpen, InClosed, InCentre, x_m, y_m)
      return(data)
    })
    
    # Calculate distance and velocity
    data <- data %>%
      mutate(
        distance = sqrt((x_m - lag(x_m))^2 + (y_m - lag(y_m))^2),
        velocity = distance / (Time - lag(Time))
      )
    
    data
    
  }, error = function(e) {
    cat("ERROR processing time series data:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(data)) {
    stop("Failed to process time series data")
  }
  
  # Create nested structure
  result <- tryCatch({
    data <- as_tibble(data)
    
    result <- metadata %>%
      mutate(
        filename = basename(file_path),
        data = list(data)
      )
    
    result
    
  }, error = function(e) {
    # Fallback nesting attempt
    result <- tibble(
      filename = basename(file_path),
      RatID = metadata$RatID,
      Diet = metadata$Diet,
      Strain = metadata$Strain,
      Treatment = metadata$Treatment,
      data = list(data)
    )
    result
  })
  
  return(result)
}

#' Create complete arm grids
create_complete_arm_grids <- function(data) {
  # Filter data for each area
  open_arms_data <- data %>% 
    filter(InOpen == 1, InCentre == 0) %>%
    filter(!is.na(x_m) & !is.na(y_m))
  
  closed_arms_data <- data %>% 
    filter(InClosed == 1, InCentre == 0) %>%
    filter(!is.na(x_m) & !is.na(y_m))
  
  # Get center zone coordinates
  center_data <- data %>%
    filter(InCentre == 1) %>%
    filter(!is.na(x_m) & !is.na(y_m))
  
  # Find the boundaries of the center zone
  center_x_min <- min(center_data$x_m, na.rm = TRUE)
  center_x_max <- max(center_data$x_m, na.rm = TRUE)
  center_y_min <- min(center_data$y_m, na.rm = TRUE)
  center_y_max <- max(center_data$y_m, na.rm = TRUE)
  
  # For open arms (along y-axis)
  open_positive_depths <- seq(from = center_y_max, 
                            to = max(open_arms_data$y_m, na.rm = TRUE), 
                            by = 2)
  open_negative_depths <- seq(from = center_y_min, 
                            to = min(open_arms_data$y_m, na.rm = TRUE), 
                            by = -2)
  
  # Create bands for open arms
  open_bands <- bind_rows(
    tibble(
      depth_start = head(open_positive_depths, -1),
      depth_end = tail(open_positive_depths, -1),
      x_min = min(open_arms_data$x_m, na.rm = TRUE),
      x_max = max(open_arms_data$x_m, na.rm = TRUE)
    ),
    tibble(
      depth_start = head(open_negative_depths, -1),
      depth_end = tail(open_negative_depths, -1),
      x_min = min(open_arms_data$x_m, na.rm = TRUE),
      x_max = max(open_arms_data$x_m, na.rm = TRUE)
    )
  ) %>%
    mutate(
      depth_low = pmin(depth_start, depth_end),
      depth_high = pmax(depth_start, depth_end),
      depth_start = depth_low,
      depth_end = depth_high,
      band_id = row_number(),
      grid_type = "open"
    ) %>%
    select(-depth_low, -depth_high)
  
  # For closed arms (along x-axis)
  closed_positive_depths <- seq(from = center_x_max, 
                              to = max(closed_arms_data$x_m, na.rm = TRUE), 
                              by = 2)
  closed_negative_depths <- seq(from = center_x_min, 
                              to = min(closed_arms_data$x_m, na.rm = TRUE), 
                              by = -2)
  
  # Create bands for closed arms
  closed_bands <- bind_rows(
    tibble(
      depth_start = head(closed_positive_depths, -1),
      depth_end = tail(closed_positive_depths, -1),
      y_min = min(closed_arms_data$y_m, na.rm = TRUE),
      y_max = max(closed_arms_data$y_m, na.rm = TRUE)
    ),
    tibble(
      depth_start = head(closed_negative_depths, -1),
      depth_end = tail(closed_negative_depths, -1),
      y_min = min(closed_arms_data$y_m, na.rm = TRUE),
      y_max = max(closed_arms_data$y_m, na.rm = TRUE)
    )
  ) %>%
    mutate(
      depth_low = pmin(depth_start, depth_end),
      depth_high = pmax(depth_start, depth_end),
      depth_start = depth_low,
      depth_end = depth_high,
      band_id = row_number(),
      grid_type = "closed"
    ) %>%
    select(-depth_low, -depth_high)
  
  # Combine all bands
  total_bands <- bind_rows(open_bands, closed_bands)
  
  # Create total grid type
  total_grid <- bind_rows(
    open_bands %>% select(-grid_type),
    closed_bands %>% select(-grid_type)
  ) %>%
  mutate(grid_type = "total")
  
  # Create nested structure
  grid_data <- bind_rows(total_bands, total_grid) %>%
  group_by(grid_type) %>%
  nest()
  
  return(grid_data)
}

#' Calculate first visits and cumulative percentages
calculate_grid_exploration <- function(trial_data, grid_data) {
  # Extract grid coordinates for each type
  grid_types <- grid_data %>%
    unnest(data) %>%
    split(.$grid_type)
  
  # Function to process each grid type
  process_grid_type <- function(trial_df, grid_type_df) {
    if (nrow(trial_df) == 0 || nrow(grid_type_df) == 0) {
      return(NULL)
    }
    
    total_bands <- nrow(grid_type_df)
    
    if (total_bands == 0) {
      return(NULL)
    }
    
    # Create a vector to track visited bands
    visited <- rep(FALSE, nrow(grid_type_df))
    names(visited) <- grid_type_df$band_id
    
    # Process each timepoint
    result <- trial_df %>%
      mutate(
        is_new_visit = map_lgl(seq_len(n()), function(i) {
          x <- x_m[i]
          y <- y_m[i]
          
          # Check which band contains this point
          if (grid_type_df$grid_type[1] == "open") {
            band_idx <- which(
              y >= grid_type_df$depth_start & 
              y <= grid_type_df$depth_end &
              x >= grid_type_df$x_min &
              x <= grid_type_df$x_max
            )
          } else if (grid_type_df$grid_type[1] == "closed") {
            band_idx <- which(
              x >= grid_type_df$depth_start & 
              x <= grid_type_df$depth_end &
              y >= grid_type_df$y_min &
              y <= grid_type_df$y_max
            )
          } else {
            # For total, check both conditions
            band_idx <- which(
              (y >= grid_type_df$depth_start & 
               y <= grid_type_df$depth_end &
               x >= grid_type_df$x_min &
               x <= grid_type_df$x_max) |
              (x >= grid_type_df$depth_start & 
               x <= grid_type_df$depth_end &
               y >= grid_type_df$y_min &
               y <= grid_type_df$y_max)
            )
          }
          
          # If point is in a band and band hasn't been visited
          if (length(band_idx) > 0 && !visited[band_idx]) {
            visited[band_idx] <<- TRUE
            return(TRUE)
          }
          return(FALSE)
        }),
        cumulative_visits = cumsum(is_new_visit),
        exploration_percentage = (cumulative_visits / total_bands) * 100
      )
    
    return(result)
  }
  
  # Process each grid type and combine results
  results <- map(grid_types, function(grid_type_df) {
    result <- process_grid_type(trial_data, grid_type_df)
    if (is.null(result)) {
      return(trial_data %>% 
             mutate(
               is_new_visit = FALSE,
               cumulative_visits = 0,
               exploration_percentage = 0
             ))
    }
    return(result)
  }) %>%
    bind_rows(.id = "grid_type")
  
  return(results)
}

#' Process multiple files with combined grid approach
#' @param data_path Path to directory containing data files
#' @param file_pattern Regex pattern to match files
#' @param max_files Optional limit on number of files to process
#' @return Combined exploration data with consistent grid across all trials
process_multiple_files <- function(data_path, file_pattern = "^NEG_Test.*\\.xlsx$", max_files = NULL, head_skip, data_skip, meta_range) {
  # List all trial files matching the pattern
  trial_files <- list.files(data_path, 
                           pattern = file_pattern,
                           full.names = TRUE)
  
  cat("Found", length(trial_files), "files matching pattern:", file_pattern, "\n")
  
  # Optionally limit number of files for testing
  if (!is.null(max_files)) {
    trial_files <- trial_files[1:min(length(trial_files), max_files)]
    cat("Processing", length(trial_files), "files (limited by max_files)\n")
  }
  
  cat("\n=== PHASE 1: Loading and preprocessing all files ===\n")
  
  # First pass: Load all data to create combined grid
  all_trial_data <- list()
  all_unnested_data <- list()
  
  for (i in seq_along(trial_files)) {
    f <- trial_files[i]
    cat("Loading file", i, "of", length(trial_files), ":", basename(f), "\n")
    
    tryCatch({
      # Read and preprocess single file
      trial_data <- read_and_clean_data(
        f,
        head_skip = head_skip,
        data_skip = data_skip,
        meta_range = meta_range
      )
      
      # Store the nested data
      all_trial_data[[i]] <- trial_data
      
      # Unnest data for grid calculation
      trial_unnested <- trial_data %>%
        unnest(data) %>%
        mutate(
          trial_index = i,
          filename = basename(f)
        )
      
      all_unnested_data[[i]] <- trial_unnested
      
    }, error = function(e) {
      cat("ERROR loading file", basename(f), ":", e$message, "\n")
      all_trial_data[[i]] <<- NULL
      all_unnested_data[[i]] <<- NULL
    })
  }
  
  # Remove NULL entries (failed files)
  all_trial_data <- all_trial_data[!sapply(all_trial_data, is.null)]
  all_unnested_data <- all_unnested_data[!sapply(all_unnested_data, is.null)]
  
  if (length(all_unnested_data) == 0) {
    stop("No files were successfully loaded")
  }
  
  cat("\n=== PHASE 2: Creating combined grid from all recentered data ===\n")
  
  # Combine all unnested data
  combined_data <- bind_rows(all_unnested_data)
  
  cat("Combined data from", length(all_unnested_data), "trials\n")
  cat("Total data points:", nrow(combined_data), "\n")
  cat("Coordinate ranges - X:", round(min(combined_data$x_m, na.rm = TRUE), 2), "to", 
      round(max(combined_data$x_m, na.rm = TRUE), 2), "\n")
  cat("Coordinate ranges - Y:", round(min(combined_data$y_m, na.rm = TRUE), 2), "to", 
      round(max(combined_data$y_m, na.rm = TRUE), 2), "\n")
  
  # Create master grid from combined data
  master_grid <- create_complete_arm_grids(combined_data)
  
  # Print grid information
  cat("\nMaster grid created:\n")
  grid_summary <- master_grid %>%
    unnest(data) %>%
    group_by(grid_type) %>%
    summarise(
      n_bands = n(),
      .groups = 'drop'
    )
  
  for (i in 1:nrow(grid_summary)) {
    cat("-", grid_summary$grid_type[i], "arms:", grid_summary$n_bands[i], "bands\n")
  }
  
  cat("\n=== PHASE 3: Calculating exploration using master grid ===\n")
  
  # Second pass: Calculate exploration using master grid
  all_exploration_results <- list()
  
  for (i in seq_along(all_trial_data)) {
    if (is.null(all_trial_data[[i]])) next
    
    trial_data <- all_trial_data[[i]]
    get_meta <- function(df, name) {
      if (name %in% names(df)) {
        return(df[[name]])
      }
      NA_character_
    }
    find_id_column <- function(df) {
      if ("rat_id" %in% names(df)) {
        return("rat_id")
      }
      id_cols <- grep("id", names(df), ignore.case = TRUE, value = TRUE)
      if (length(id_cols) > 0) {
        return(id_cols[[1]])
      }
      NULL
    }
    cat("Calculating exploration for trial", i, ":", get_meta(trial_data, "rat_id"), "\n")
    
    tryCatch({
      # Extract metadata
      metadata <- trial_data %>%
        select(-data)
      
      id_col <- find_id_column(metadata)
      if (is.null(id_col)) {
        metadata$id <- paste0("trial_", i)
        cat("No ID column found in metadata for trial", i, "- assigned unique id:", metadata$id, "\n")
        id_col <- "id"
      }
      
      # Unnest data for this trial
      trial_unnested <- trial_data %>%
        unnest(data) %>%
        mutate(x_mr = round(x_m), y_mr = round(y_m))
      
      # Calculate exploration using MASTER GRID
      exploration_data <- calculate_grid_exploration(trial_unnested, master_grid)
      
      # Add metadata to results
      trial_id <- if (all(c("rat_id", "diet", "strain") %in% names(metadata))) {
        paste(metadata$rat_id, metadata$diet, metadata$strain, sep = "_")
      } else if (!is.null(id_col)) {
        as.character(metadata[[id_col]])
      } else if (ncol(metadata) > 0) {
        as.character(metadata[[1]])
      } else {
        as.character(trial_data$filename)
      }
      
      exploration_data <- exploration_data %>%
        mutate(
          trial_id = trial_id,
          filename = trial_data$filename
        )
      missing_meta <- setdiff(names(metadata), names(exploration_data))
      if (length(missing_meta) > 0) {
        exploration_data <- bind_cols(exploration_data, metadata[missing_meta])
      }
      
      all_exploration_results[[i]] <- exploration_data
      
      # Clean up
      rm(trial_unnested, exploration_data)
      gc()
      
    }, error = function(e) {
      cat("ERROR calculating exploration for trial", i, ":", e$message, "\n")
    })
  }
  
  cat("\n=== PHASE 4: Combining results ===\n")
  
  # Remove NULL entries and combine results
  all_exploration_results <- all_exploration_results[!sapply(all_exploration_results, is.null)]
  
  if (length(all_exploration_results) == 0) {
    stop("No exploration data was successfully calculated")
  }
  
  # Combine all results
  all_results <- bind_rows(all_exploration_results)
  
  cat("Successfully processed", length(all_exploration_results), "trials\n")
  cat("Final dataset contains", nrow(all_results), "rows\n")
  
  # Add master grid as attribute for reference
  attr(all_results, "master_grid") <- master_grid
  attr(all_results, "grid_summary") <- grid_summary
  
  return(all_results)
}

#' Extract master grid from process_multiple_files results
#' @param exploration_results Results from process_multiple_files
#' @return The master grid used for all trials
get_master_grid <- function(exploration_results) {
  master_grid <- attr(exploration_results, "master_grid")
  if (is.null(master_grid)) {
    stop("No master grid found. Was this data processed with the updated process_multiple_files function?")
  }
  return(master_grid)
}

#' Get grid summary from process_multiple_files results
#' @param exploration_results Results from process_multiple_files
#' @return Summary of grid structure
get_grid_summary <- function(exploration_results) {
  grid_summary <- attr(exploration_results, "grid_summary")
  if (is.null(grid_summary)) {
    stop("No grid summary found. Was this data processed with the updated process_multiple_files function?")
  }
  return(grid_summary)
}

#' Visualize the master grid used across all trials
#' @param exploration_results Results from process_multiple_files
#' @param sample_data Optional: sample data to overlay on grid
#' @return ggplot object showing the master grid
plot_master_grid <- function(exploration_results, sample_data = NULL) {
  require(ggplot2)
  
  master_grid <- get_master_grid(exploration_results)
  
  # Extract grid data
  open_bands <- master_grid %>% filter(grid_type == "open") %>% unnest(data)
  closed_bands <- master_grid %>% filter(grid_type == "closed") %>% unnest(data)
  
  p <- ggplot() +
    # Open arms (vertical bands)
    geom_rect(data = open_bands, 
              aes(xmin = x_min, xmax = x_max,
                  ymin = depth_start, ymax = depth_end),
              fill = "lightblue", color = "darkblue", alpha = 0.4) +
    # Closed arms (horizontal bands)
    geom_rect(data = closed_bands, 
              aes(xmin = depth_start, xmax = depth_end,
                  ymin = y_min, ymax = y_max),
              fill = "lightcoral", color = "darkred", alpha = 0.4) +
    coord_equal() +
    theme_minimal() +
    labs(title = "Master Grid Used Across All Trials",
         subtitle = paste("Open arms:", nrow(open_bands), "bands | Closed arms:", nrow(closed_bands), "bands"),
         x = "X Position (cm)",
         y = "Y Position (cm)")
  
  # Optionally overlay sample data
  if (!is.null(sample_data)) {
    p <- p + 
      geom_point(data = sample_data, 
                 aes(x = x_m, y = y_m, color = factor(InOpen)), 
                 alpha = 0.5, size = 0.3) +
      scale_color_manual(values = c("red", "blue"), 
                        labels = c("Closed Arm", "Open Arm"),
                        name = "Location")
  }
  
  return(p)
}
