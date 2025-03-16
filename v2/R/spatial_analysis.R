#' @import dplyr tidyr
NULL

#' Create grid structure for arm exploration analysis
#' @param data Processed EPM data with x_m, y_m coordinates
#' @return Nested tibble with grid structures for open arms, closed arms, and total maze
#' @export
create_complete_arm_grids <- function(data) {
  # Filter data for each area
  open_arms_data <- data %>% 
    filter(Inopen_arms == 1, Incenter == 0) %>%
    filter(!is.na(x_m) & !is.na(y_m))
  
  closed_arms_data <- data %>% 
    filter(Inclosed_arms == 1, Incenter == 0) %>%
    filter(!is.na(x_m) & !is.na(y_m))
  
  # Get center zone boundaries
  center_data <- data %>%
    filter(Incenter == 1) %>%
    filter(!is.na(x_m) & !is.na(y_m))
  
  # Find center zone boundaries
  center_x_min <- min(center_data$x_m)
  center_x_max <- max(center_data$x_m)
  center_y_min <- min(center_data$y_m)
  center_y_max <- max(center_data$y_m)
  
  # Create depth bands for open arms (along y-axis)
  open_positive_depths <- seq(from = center_y_max, 
                            to = max(open_arms_data$y_m), 
                            by = 2)
  open_negative_depths <- seq(from = center_y_min, 
                            to = min(open_arms_data$y_m), 
                            by = -2)
  
  # Create bands for open arms
  open_bands <- bind_rows(
    # Positive y direction
    tibble(
      depth_start = head(open_positive_depths, -1),
      depth_end = tail(open_positive_depths, -1),
      x_min = min(open_arms_data$x_m),
      x_max = max(open_arms_data$x_m)
    ),
    # Negative y direction
    tibble(
      depth_start = head(open_negative_depths, -1),
      depth_end = tail(open_negative_depths, -1),
      x_min = min(open_arms_data$x_m),
      x_max = max(open_arms_data$x_m)
    )
  ) %>%
    mutate(
      band_id = row_number(),
      grid_type = "open"
    )
  
  # Create depth bands for closed arms (along x-axis)
  closed_positive_depths <- seq(from = center_x_max, 
                              to = max(closed_arms_data$x_m), 
                              by = 2)
  closed_negative_depths <- seq(from = center_x_min, 
                              to = min(closed_arms_data$x_m), 
                              by = -2)
  
  # Create bands for closed arms
  closed_bands <- bind_rows(
    # Positive x direction
    tibble(
      depth_start = head(closed_positive_depths, -1),
      depth_end = tail(closed_positive_depths, -1),
      y_min = min(closed_arms_data$y_m),
      y_max = max(closed_arms_data$y_m)
    ),
    # Negative x direction
    tibble(
      depth_start = head(closed_negative_depths, -1),
      depth_end = tail(closed_negative_depths, -1),
      y_min = min(closed_arms_data$y_m),
      y_max = max(closed_arms_data$y_m)
    )
  ) %>%
    mutate(
      band_id = row_number(),
      grid_type = "closed"
    )
  
  # Combine all bands and create total grid
  total_bands <- bind_rows(
    open_bands,
    closed_bands
  )
  
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

#' Calculate grid exploration metrics
#' @param trial_data Single trial data
#' @param grid_data Grid structure from create_complete_arm_grids
#' @param time_window Time window for exploration calculation in seconds
#' @return Data frame with exploration metrics
#' @export
calculate_grid_exploration <- function(trial_data, 
                                     grid_data, 
                                     time_window = NULL) {
  # Initialize tracking variables
  visited_squares <- integer(0)
  cumulative_visits <- numeric(0)
  total_squares <- nrow(grid_data)
  
  # Process each timepoint
  exploration_data <- trial_data %>%
    arrange(Time) %>%
    group_by(Time) %>%
    mutate(
      # Find current grid square
      current_square = find_grid_square(x_m, y_m, grid_data),
      
      # Update visited squares
      new_visit = !current_square %in% visited_squares,
      
      # Calculate metrics
      cumulative_squares = if_else(new_visit, 
                                 length(unique(c(visited_squares, current_square))), 
                                 length(unique(visited_squares))),
      
      exploration_percentage = (cumulative_squares / total_squares) * 100
    ) %>%
    ungroup()
  
  # If time window specified, calculate windowed metrics
  if (!is.null(time_window)) {
    exploration_data <- exploration_data %>%
      mutate(
        time_window = ceiling(Time / time_window) * time_window,
        window_start = time_window - time_window,
        window_end = time_window
      )
  }
  
  return(exploration_data)
}

#' Find which grid square contains a point
#' @param x X coordinate
#' @param y Y coordinate
#' @param grid_data Grid structure
#' @return Grid square ID containing the point
#' @noRd
find_grid_square <- function(x, y, grid_data) {
  # Find nearest grid point
  distances <- sqrt((grid_data$depth - x)^2 + (grid_data$width - y)^2)
  return(which.min(distances))
}

#' Transform coordinates based on arm type and position
#' @param data Trial data
#' @param arm_type Type of arm ("open" or "closed")
#' @param arm_number Arm number (1 or 2)
#' @return Data frame with transformed coordinates
#' @noRd
transform_coordinates <- function(data, arm_type, arm_number) {
  # Get arm-specific zone column
  zone_col <- paste0("In", arm_type, "_arms")
  
  # Transform coordinates based on arm position
  transformed_data <- data %>%
    filter(!!sym(zone_col) == 1) %>%
    mutate(
      # Transform coordinates based on arm position
      # This is a placeholder - actual transformation would depend on maze geometry
      transformed_x = x_m,
      transformed_y = y_m
    )
  
  return(transformed_data)
} 