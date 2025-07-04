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
read_and_clean_data <- function(file_path, ...) {
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
    metadata <- readxl::read_xlsx(file_path, range = "B32:B35", col_names = FALSE)
    if (nrow(metadata) != 4) {
      stop("Metadata rows 32-35 are incomplete or missing")
    }
    
    metadata <- metadata %>%
      rename(value = 1) %>%
      mutate(field = c("RatID", "Diet", "Strain", "Treatment")) %>%
      pivot_wider(names_from = field, values_from = value)
    
    metadata
    
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
    headers <- readxl::read_xlsx(file_path, skip = 36, n_max = 1)
    
    data <- readxl::read_xlsx(file_path, 
                             skip = 38,
                             col_names = names(headers),
                             na = c("", "-", "NA"))
    
    # Clean the data
    data <- data %>%
      mutate(across(c(`Trial time`, `X center`, `Y center`), 
                   ~as.numeric(ifelse(. == "-", NA, .))))
    
    # Check for required columns
    actual_cols <- names(data)
    missing_cols <- c()
    
    if(!"Trial time" %in% actual_cols) missing_cols <- c(missing_cols, "Trial time")
    if(!"X center" %in% actual_cols) missing_cols <- c(missing_cols, "X center")
    if(!"Y center" %in% actual_cols) missing_cols <- c(missing_cols, "Y center")
    
    open_arm_cols <- c("In zone(open_arm_1 / center-point)", "In zone(open_arm_2 / center-point)")
    closed_arm_cols <- c("In zone(closed_arm_1 / center-point)", "In zone(closed_arm_2 / center-point)")
    
    has_open_arms <- all(open_arm_cols %in% actual_cols)
    has_closed_arms <- all(closed_arm_cols %in% actual_cols)
    
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
      mutate(across(starts_with("In zone"), ~as.numeric(ifelse(. == "-", 0, .))))
    
    # Create InOpen and InClosed columns
    data <- data %>%
      mutate(
        InOpen = case_when(
          `In zone(open_arm_1 / center-point)` == 1 | `In zone(open_arm_2 / center-point)` == 1 ~ 1,
          TRUE ~ 0
        ),
        InClosed = case_when(
          `In zone(closed_arm_1 / center-point)` == 1 | `In zone(closed_arm_2 / center-point)` == 1 ~ 1,
          TRUE ~ 0
        ),
        InCentre = case_when(
          `In zone(centre_zone / center-point)` == 1 ~ 1,
          TRUE ~ 0
        )
      )
    
    # Create clean data frame
    data <- tibble(
      Time = as.numeric(data$`Trial time`),
      X = as.numeric(data$`X center`),
      Y = as.numeric(data$`Y center`),
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
      band_id = row_number(),
      grid_type = "open"
    )
  
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
      band_id = row_number(),
      grid_type = "closed"
    )
  
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
    
    total_bands <- nrow(grid_type_df) / 2
    
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
process_multiple_files <- function(data_path, file_pattern = "^NEG_Test.*\\.xlsx$", max_files = NULL) {
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
      trial_data <- read_and_clean_data(f)
      
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
    cat("Calculating exploration for trial", i, ":", trial_data$RatID, "\n")
    
    tryCatch({
      # Extract metadata
      metadata <- trial_data %>%
        select(RatID, Diet, Strain, Treatment)
      
      # Unnest data for this trial
      trial_unnested <- trial_data %>%
        unnest(data) %>%
        mutate(x_mr = round(x_m), y_mr = round(y_m))
      
      # Calculate exploration using MASTER GRID
      exploration_data <- calculate_grid_exploration(trial_unnested, master_grid)
      
      # Add metadata to results
      trial_id <- paste(metadata$RatID, metadata$Diet, metadata$Strain, sep="_")
      
      exploration_data <- exploration_data %>%
        mutate(
          RatID = metadata$RatID,
          Diet = metadata$Diet,
          Strain = metadata$Strain,
          Treatment = metadata$Treatment,
          trial_id = trial_id,
          filename = trial_data$filename
        )
      
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