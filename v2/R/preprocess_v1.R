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
    select(Time, X, Y)  # Explicit column selection in test order
  
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
    
    # Create a proper cluster_points structure
    cluster_points <- center_points %>%
      mutate(cluster = 1)  # All points in one cluster
    
    # Calculate centroid
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
    
    # Create a proper cluster_points structure
    cluster_points <- center_points %>%
      mutate(cluster = 1)  # All points in one cluster
    
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
#' @param data Dataframe containing coordinates to correct
#' @param shift_values List containing x_shift and y_shift values
#' @return Dataframe with corrected coordinates
apply_center_shift <- function(data, shift_values) {
  if(!shift_values$success) {
    warning("Using uncorrected coordinates due to unsuccessful shift detection")
    return(data %>% 
           mutate(
             x_m = X,
             y_m = Y
           ))
  }
  
  # Add warning for low confidence corrections
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

#' Plot diagnostic visualization of shift correction
#' @param data Original data
#' @param shift_values Shift correction results
#' @return ggplot object showing original and corrected points
plot_shift_correction <- function(data, shift_values) {
  require(ggplot2)
  
  if(!shift_values$success) {
    return(ggplot() + 
      annotate("text", x = 0, y = 0, 
               label = paste("Shift correction failed:", shift_values$reason)) +
      theme_minimal())
  }
  
  # Apply correction to get corrected coordinates
  corrected_data <- apply_center_shift(data, shift_values)
  
  # Base plot with original and corrected points
  p <- ggplot() +
    # Original points
    geom_point(data = data, 
               aes(x = X, y = Y), 
               alpha = 0.3, color = "gray") +
    # Corrected points
    geom_point(data = corrected_data,
               aes(x = x_m, y = y_m),
               alpha = 0.3, color = "blue")
  
  # Add cluster points if available
  if (!is.null(shift_values$cluster_points) && nrow(shift_values$cluster_points) > 0) {
    p <- p + 
      geom_point(data = shift_values$cluster_points,
                 aes(x = X, y = Y),
                 color = "red", size = 2)
  }
  
  # Add centroid if available
  if (!is.null(shift_values$centroid)) {
    p <- p + 
      geom_point(data = shift_values$centroid,
                 aes(x = x_center, y = y_center),
                 color = "green", size = 4, shape = "x")
    
    # Add shift arrow
    p <- p +
      geom_segment(data = shift_values$centroid,
                  aes(x = x_center, y = y_center,
                      xend = 0, yend = 0),
                  arrow = arrow(length = unit(0.5, "cm")),
                  color = "red")
  }
  
  # Add origin point
  p <- p + geom_point(aes(x = 0, y = 0), color = "black", size = 2)
  
  # Add title and theme
  confidence_note <- if(shift_values$low_confidence) " [Low Confidence]" else ""
  p <- p +
    coord_fixed() +
    theme_minimal() +
    labs(title = "Shift Correction Visualization",
         subtitle = sprintf("Method: %s (%d points)%s\nShift: (%.2f, %.2f)", 
                          shift_values$method,
                          shift_values$n_points,
                          confidence_note,
                          shift_values$x_shift, 
                          shift_values$y_shift))
  
  return(p)
}

#' Plot comparison of raw and corrected trajectories
#' @param data Original data
#' @param shift_values Shift correction results
#' @param subject_info List containing RatID, Diet, and Treatment
#' @return ggplot object showing raw and corrected trajectories side by side
plot_trajectory_comparison <- function(data, shift_values, subject_info) {
  require(ggplot2)
  require(patchwork)
  
  # Create title with subject info
  title <- sprintf("Rat: %s\nDiet: %s, Treatment: %s",
                  subject_info$RatID,
                  subject_info$Diet,
                  subject_info$Treatment)
  
  # Plot raw trajectory
  p1 <- ggplot(data, aes(x = X, y = Y)) +
    geom_path(alpha = 0.5, color = "gray50") +
    geom_point(aes(color = Time), size = 0.5) +
    scale_color_viridis_c(name = "Time (s)") +
    coord_fixed() +
    theme_minimal() +
    labs(title = "Raw Trajectory")
  
  if(shift_values$success) {
    # Apply correction and plot corrected trajectory
    corrected_data <- apply_center_shift(data, shift_values)
    
    subtitle <- sprintf("Shift: (%.2f, %.2f)\n%s method (%d points)%s",
                       shift_values$x_shift,
                       shift_values$y_shift,
                       shift_values$method,
                       shift_values$n_points,
                       if(shift_values$low_confidence) " [Low Confidence]" else "")
    
    p2 <- ggplot(corrected_data, aes(x = x_m, y = y_m)) +
      geom_path(alpha = 0.5, color = "gray50") +
      geom_point(aes(color = Time), size = 0.5)
    
    # Add center points if available
    if (!is.null(shift_values$cluster_points) && nrow(shift_values$cluster_points) > 0) {
      # Create corrected cluster points
      corrected_cluster_points <- shift_values$cluster_points %>%
        mutate(
          X_corrected = X + shift_values$x_shift,
          Y_corrected = Y + shift_values$y_shift
        )
      
      p2 <- p2 + 
        geom_point(data = corrected_cluster_points,
                  aes(x = X_corrected, y = Y_corrected),
                  color = "red", size = 1)
    }
    
    # Add origin point
    p2 <- p2 +
      geom_point(x = 0, y = 0,
                color = "green", size = 3, shape = "x") +
      scale_color_viridis_c(name = "Time (s)") +
      coord_fixed() +
      theme_minimal() +
      labs(title = "Corrected Trajectory",
           subtitle = subtitle)
  } else {
    # If correction failed, show message
    p2 <- ggplot() +
      annotate("text", x = 0, y = 0,
               label = sprintf("Correction failed:\n%s", shift_values$reason)) +
      theme_minimal() +
      labs(title = "Correction Failed")
  }
  
  # Combine plots
  p1 / p2 +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
}

#' Process and plot trajectories for multiple trials
#' @param trials_data List or dataframe containing multiple trials
#' @param save_dir Optional directory to save plots
#' @return List of plots
process_and_plot_trials <- function(trials_data, save_dir = NULL) {
  require(purrr)
  require(ggplot2)
  
  # Create save directory if specified
  if(!is.null(save_dir)) {
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Process each trial
  plots <- trials_data %>%
    group_by(filename, RatID, Diet, Treatment) %>%
    group_map(function(trial_data, group_info) {
      # Extract the actual tracking data
      data <- trial_data$data[[1]]
      
      # Get shift values
      shift_values <- detect_center_shift(data)
      
      # Create subject info list
      subject_info <- list(
        RatID = group_info$RatID,
        Diet = group_info$Diet,
        Treatment = group_info$Treatment
      )
      
      # Create plot
      p <- plot_trajectory_comparison(data, shift_values, subject_info)
      
      # Save plot if directory specified
      if(!is.null(save_dir)) {
        filename <- file.path(save_dir,
                            paste0("trajectory_", 
                                  gsub("[^[:alnum:]]", "_", group_info$filename),
                                  ".png"))
        ggsave(filename, p, width = 12, height = 6, dpi = 300)
      }
      
      return(p)
    })
  
  return(plots)
}

#' Read and clean EPM trial data
#' @param file_path Path to the Excel file containing trial data
#' @return A nested tibble with the following structure:
#'   - filename: Name of the source file
#'   - RatID: Identifier for the rat
#'   - Diet: Diet condition
#'   - Strain: Rat strain
#'   - Treatment: Treatment condition
#'   - data: Nested tibble containing time series data with columns:
#'     * Time: Trial time
#'     * X, Y: Centered coordinates
#'     * InOpen: Binary indicator for open arm position
#'     * InClosed: Binary indicator for closed arm position
#'     * distance: Distance traveled between points
#'     * velocity: Instantaneous velocity
read_and_clean_data <- function(file_path, ...) {  # Add ... to capture additional arguments
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Check file extension
  if (!grepl("\\.xlsx$", file_path)) {
    stop("File must be an Excel (.xlsx) file")
  }
  
  cat("\nProcessing file:", basename(file_path), "\n")
  
  # Safely read metadata
  metadata <- tryCatch({
    # Read specifically column B from rows 32-35
    cat("Reading metadata...\n")
    metadata <- readxl::read_xlsx(file_path, range = "B32:B35", col_names = FALSE)
    if (nrow(metadata) != 4) {
      stop("Metadata rows 32-35 are incomplete or missing")
    }
    
    # Debug output for metadata
    cat("Raw metadata values:\n")
    print(metadata)
    
    # Process the metadata
    metadata <- metadata %>%
      rename(value = 1) %>%  # Rename the column
      mutate(field = c("RatID", "Diet", "Strain", "Treatment")) %>%
      pivot_wider(names_from = field, values_from = value)
    
    cat("Processed metadata:\n")
    print(metadata)
    metadata
    
  }, error = function(e) {
    cat("ERROR reading metadata:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(metadata)) {
    stop("Failed to read metadata")
  }
  
  # Required column names for time series data
  required_cols <- c(
    "Trial time",
    "X center",
    "Y center",
    "In zone(centre_zone / center-point)",
    "In zone(open_arm_1 / center-point)",
    "In zone(open_arm_2 / center-point)",
    "In zone(closed_arm_1 / center-point)",
    "In zone(closed_arm_2 / center-point)"
  )
  
  # Safely read time series data
  data <- tryCatch({
    cat("Reading time series data...\n")
    # First read the headers to check column names
    headers <- readxl::read_xlsx(file_path, skip = 36, n_max = 1)
    cat("Available columns:\n")
    print(names(headers))
    
    # Read data using the headers we found
    cat("Reading full data...\n")
    data <- readxl::read_xlsx(file_path, 
                             skip = 38,
                             col_names = names(headers),
                             na = c("", "-", "NA"))  # Treat "-" as NA
    
    cat("Rows read:", nrow(data), "\n")
    cat("Initial columns:", paste(names(data), collapse = ", "), "\n")
    
    # Clean the data - replace any non-numeric values with NA and convert to numeric
    data <- data %>%
      mutate(across(c(`Trial time`, `X center`, `Y center`), 
                   ~as.numeric(ifelse(. == "-", NA, .))))
    
    # Check for required columns with more flexible matching
    actual_cols <- names(data)
    missing_cols <- c()
    
    # Check for basic columns with exact matches
    if(!"Trial time" %in% actual_cols) missing_cols <- c(missing_cols, "Trial time")
    if(!"X center" %in% actual_cols) missing_cols <- c(missing_cols, "X center")
    if(!"Y center" %in% actual_cols) missing_cols <- c(missing_cols, "Y center")
    
    # Check for zone columns with exact matches
    open_arm_cols <- c("In zone(open_arm_1 / center-point)", "In zone(open_arm_2 / center-point)")
    closed_arm_cols <- c("In zone(closed_arm_1 / center-point)", "In zone(closed_arm_2 / center-point)")
    
    has_open_arms <- all(open_arm_cols %in% actual_cols)
    has_closed_arms <- all(closed_arm_cols %in% actual_cols)
    
    if(!has_open_arms) missing_cols <- c(missing_cols, "open arm zones")
    if(!has_closed_arms) missing_cols <- c(missing_cols, "closed arm zones")
    
    if (length(missing_cols) > 0) {
      cat("\nActual columns found:\n")
      print(actual_cols)
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    # Check for empty dataframe
    if (nrow(data) == 0) {
      stop("No time series data found in file")
    }
    
    cat("Processing zone data...\n")
    # Convert zone columns to numeric (they might be character due to "-" values)
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
    
    cat("Processing coordinates...\n")
    # Create a single data frame with all necessary columns
    data <- tibble(
      Time = as.numeric(data$`Trial time`),
      X = as.numeric(data$`X center`),
      Y = as.numeric(data$`Y center`),
      InOpen = data$InOpen,
      InClosed = data$InClosed,
      InCentre = data$InCentre
    ) %>%
      # Remove rows where X or Y is NA
      dplyr::filter(!is.na(X), !is.na(Y))
    
    cat("Rows after cleaning:", nrow(data), "\n")
    
    # Check for numeric coordinates
    if (!is.numeric(data$X) || !is.numeric(data$Y)) {
      stop("X and Y coordinates must be numeric")
    }
    
    # Check for valid time values
    if (!is.numeric(data$Time) || any(data$Time < 0, na.rm = TRUE)) {
      stop("Time values must be numeric and non-negative")
    }
    
    # Debug output before shift correction
    cat("\nPreparing for shift correction:\n")
    cat("Number of center zone points:", sum(data$InCentre == 1), "\n")
    cat("Coordinate ranges before correction:\n")
    print(summary(data[c("X", "Y")]))
    
    # Apply center-based shift correction with error handling
    tryCatch({
      # Check if required functions exist
      if (!exists("detect_center_shift") || !exists("apply_center_shift")) {
        stop("Required functions not loaded properly")
      }
      
      # Get number of center points before correction
      n_center_points <- sum(data$InCentre == 1)
      cat("Number of center points found:", n_center_points, "\n")
      
      shift_values <- detect_center_shift(data)
      
      cat("Shift detection results:\n")
      cat("Success:", shift_values$success, "\n")
      if (!shift_values$success) {
        cat("Failure reason:", shift_values$reason, "\n")
        cat("Number of points found:", shift_values$n_points, "\n")
        # Even if shift detection fails, we should still proceed with original coordinates
        data <- data %>%
          mutate(
            x_m = X,
            y_m = Y
          )
      } else {
        cat("X shift:", shift_values$x_shift, "\n")
        cat("Y shift:", shift_values$y_shift, "\n")
        cat("Number of points:", shift_values$n_points, "\n")
        cat("Method:", shift_values$method, "\n")
        if(shift_values$low_confidence) {
          cat("Warning: Low confidence correction\n")
        }
        
        # Apply the correction and update coordinates
        cat("\nApplying coordinate correction...\n")
        data <- apply_center_shift(data, shift_values)
      }
      
      # Verify the corrected coordinates exist
      if (!all(c("x_m", "y_m") %in% names(data))) {
        warning("Coordinate correction incomplete: adding uncorrected coordinates")
        data <- data %>%
          mutate(
            x_m = X,
            y_m = Y
          )
      }
      
      # Ensure all necessary columns are present in the correct order
      data <- data %>%
        select(Time, X, Y, InOpen, InClosed, InCentre, x_m, y_m)
      
      cat("\nFinal data structure:\n")
      print(str(data))
      
    }, error = function(e) {
      cat("\nERROR in shift correction:", conditionMessage(e), "\n")
      # On error, use original coordinates and ensure proper column structure
      data <- data %>%
        mutate(
          x_m = X,
          y_m = Y
        ) %>%
        select(Time, X, Y, InOpen, InClosed, InCentre, x_m, y_m)
      cat("Using original coordinates due to error\n")
      return(data)  # Ensure data is returned even on error
    })
    
    # Verify coordinates are properly named before calculating distance
    if (!all(c("x_m", "y_m") %in% names(data))) {
      stop("Coordinate columns not properly renamed")
    }
    
    # Debug output after coordinate processing
    cat("\nCoordinate ranges after processing:\n")
    print(summary(data[c("x_m", "y_m")]))
    
    # Calculate distance and velocity using the coordinates
    cat("\nCalculating distance and velocity...\n")
    data <- data %>%
      mutate(
        distance = sqrt((x_m - lag(x_m))^2 + (y_m - lag(y_m))^2),
        velocity = distance / (Time - lag(Time))
      )
    
    cat("Final data processing complete\n")
    cat("Final data structure:\n")
    print(str(data))
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
    cat("Creating final nested structure...\n")
    # Debug output
    cat("Data object before nesting:\n")
    print(str(data))
    cat("Metadata object before nesting:\n")
    print(str(metadata))
    
    # Ensure data is a tibble
    data <- as_tibble(data)
    
    # Create nested structure with explicit error checking
    result <- metadata %>%
      mutate(
        filename = basename(file_path),
        data = list(data)  # Explicitly list the data
      )
    
    cat("Final result structure:\n")
    print(str(result))
    result
    
  }, error = function(e) {
    cat("ERROR creating nested structure:", conditionMessage(e), "\n")
    cat("Attempting fallback nesting...\n")
    
    # Fallback nesting attempt
    tryCatch({
      result <- tibble(
        filename = basename(file_path),
        RatID = metadata$RatID,
        Diet = metadata$Diet,
        Strain = metadata$Strain,
        Treatment = metadata$Treatment,
        data = list(data)
      )
      cat("Fallback nesting successful\n")
      result
    }, error = function(e2) {
      cat("CRITICAL ERROR: Fallback nesting failed:", conditionMessage(e2), "\n")
      return(NULL)
    })
  })
  
  if(is.null(result)) {
    stop("Failed to create nested structure")
  }
  
  # Verify the structure is correct
  expected_cols <- c("filename", "RatID", "Diet", "Strain", "Treatment", "data")
  missing_cols <- setdiff(expected_cols, names(result))
  if(length(missing_cols) > 0) {
    stop("Missing columns in final structure: ", paste(missing_cols, collapse = ", "))
  }
  
  return(result)
}

#' Create complete arm grids
#' @param data Dataframe containing trial data
#' @return Dataframe containing grid data
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
  # Create depth bands every 2cm starting from the edge of center zone
  open_positive_depths <- seq(from = center_y_max, 
                            to = max(open_arms_data$y_m, na.rm = TRUE), 
                            by = 2)
  open_negative_depths <- seq(from = center_y_min, 
                            to = min(open_arms_data$y_m, na.rm = TRUE), 
                            by = -2)
  
  # Create bands for open arms
  open_bands <- bind_rows(
    # Positive y direction bands
    tibble(
      depth_start = head(open_positive_depths, -1),
      depth_end = tail(open_positive_depths, -1),
      x_min = min(open_arms_data$x_m, na.rm = TRUE),
      x_max = max(open_arms_data$x_m, na.rm = TRUE)
    ),
    # Negative y direction bands
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
  # Create depth bands every 2cm starting from the edge of center zone
  closed_positive_depths <- seq(from = center_x_max, 
                              to = max(closed_arms_data$x_m, na.rm = TRUE), 
                              by = 2)
  closed_negative_depths <- seq(from = center_x_min, 
                              to = min(closed_arms_data$x_m, na.rm = TRUE), 
                              by = -2)
  
  # Create bands for closed arms
  closed_bands <- bind_rows(
    # Positive x direction bands
    tibble(
      depth_start = head(closed_positive_depths, -1),
      depth_end = tail(closed_positive_depths, -1),
      y_min = min(closed_arms_data$y_m, na.rm = TRUE),
      y_max = max(closed_arms_data$y_m, na.rm = TRUE)
    ),
    # Negative x direction bands
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
  total_bands <- bind_rows(
    open_bands,
    closed_bands
  )
  
  # Create total grid type (combining open and closed bands)
  total_grid <- bind_rows(
    open_bands %>% select(-grid_type),
    closed_bands %>% select(-grid_type)
  ) %>%
  mutate(grid_type = "total")
  
  # Create nested structure
  grid_data <- bind_rows(
    total_bands,
    total_grid
  ) %>%
  group_by(grid_type) %>%
  nest()
  
  return(grid_data)
}


#' Calculate first visits and cumulative percentages
#' @param trial_data Dataframe containing trial data
#' @param grid_data Dataframe containing grid data
#' @return Dataframe containing first visits and cumulative percentages
calculate_grid_exploration <- function(trial_data, grid_data) {
  # Extract grid coordinates for each type
  grid_types <- grid_data %>%
    unnest(data) %>%
    split(.$grid_type)
  
  # Function to process each grid type
  process_grid_type <- function(trial_df, grid_type_df) {
    # Safety check - ensure we have data
    if (nrow(trial_df) == 0 || nrow(grid_type_df) == 0) {
      return(NULL)
    }
    
    # Get total number of depth bands for percentage calculation
    total_bands <- nrow(grid_type_df) / 2  # Divide by 2 since we have bands in both directions
    
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
            # For open arms, check y-coordinate against depth bands
            band_idx <- which(
              y >= grid_type_df$depth_start & 
              y <= grid_type_df$depth_end &
              x >= grid_type_df$x_min &
              x <= grid_type_df$x_max
            )
          } else if (grid_type_df$grid_type[1] == "closed") {
            # For closed arms, check x-coordinate against depth bands
            band_idx <- which(
              x >= grid_type_df$depth_start & 
              x <= grid_type_df$depth_end &
              y >= grid_type_df$y_min &
              y <= grid_type_df$y_max
            )
          } else {
            # For total, check both open and closed conditions
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
      # Return empty dataframe with correct structure if processing failed
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

# Improved approach to process multiple files
# Function to process multiple files with customizable pattern
process_multiple_files <- function(data_path, file_pattern = "^NEG_Test.*\\.xlsx$", max_files = NULL) {
  # List all trial files matching the pattern
  trial_files <- list.files(data_path, 
                           pattern = file_pattern,  # User-defined pattern
                           full.names = TRUE)
  
  # Display found files (optional)
  cat("Found", length(trial_files), "files matching pattern:", file_pattern, "\n")
  cat("Example files:", paste(basename(head(trial_files, 3)), collapse = ", "), "...\n")
  
  # Optionally limit number of files for testing
  if (!is.null(max_files)) {
    trial_files <- trial_files[1:min(length(trial_files), max_files)]
  }
  
  # Create empty list to store results
  all_exploration_results <- list()
  
  # Process one file at a time
  for (i in seq_along(trial_files)) {
    f <- trial_files[i]
    cat("Processing file", i, "of", length(trial_files), ":", basename(f), "\n")
    
    # Try to process this file, continue to next file if error
    tryCatch({
      # Read and preprocess single file
      trial_data <- read_and_clean_data(f)
      
      # Extract metadata for final results
      metadata <- trial_data %>%
        select(RatID, Diet, Strain, Treatment)
      
      # Unnest data for this file only
      trial_unnested <- trial_data %>%
        unnest(data)
      
      # Free memory
      rm(trial_data)
      gc()
      
      # Create grid for this trial
      trial_grid <- create_complete_arm_grids(trial_unnested)
      
      # Calculate exploration
      trial_unnested <- trial_unnested %>%
        mutate(x_mr = round(x_m), y_mr = round(y_m))
      
      exploration_data <- calculate_grid_exploration(trial_unnested, trial_grid)
      
      # Add to results
      trial_id <- paste(metadata$RatID, metadata$Diet, metadata$Strain, sep="_")
      
      exploration_data <- exploration_data %>%
        mutate(
          RatID = metadata$RatID,
          Diet = metadata$Diet,
          Strain = metadata$Strain,
          Treatment = metadata$Treatment,
          trial_id = trial_id
        )
      
      all_exploration_results[[i]] <- exploration_data
      
      # Clean up large objects to free memory
      rm(trial_unnested, trial_grid, exploration_data)
      gc()
      
    }, error = function(e) {
      cat("ERROR processing file:", e$message, "\n")
    })
  }
  
  # Combine all results at the end
  all_results <- bind_rows(all_exploration_results)
  
  return(all_results)
}
