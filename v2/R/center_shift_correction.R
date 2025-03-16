#' Detect center shift from zone data
#' @param data Data frame containing zone and coordinate information
#' @param center_zone_radius Numeric radius around center to consider for initial filtering
#' @param eps DBSCAN clustering parameter for neighborhood size
#' @param min_pts Minimum points required to form a cluster
#' @return List containing shift values and detection metadata
#' @noRd
detect_center_shift <- function(data, center_zone_radius = 5, eps = 2, min_pts = 5) {
  require(dbscan)
  
  # Debug output
  cat("Checking center points...\n")
  cat("Number of points with Incenter == 1:", sum(data$Incenter == 1, na.rm = TRUE), "\n")
  
  # Filter points in center zone using InCentre indicator
  center_points <- data %>%
    filter(Incenter == 1) %>%
    select(Time, X, Y)
  
  n_points <- nrow(center_points)
  cat("Number of center points found:", n_points, "\n")
  
  # Handle cases with very few points
  if(n_points == 0) {
    return(list(
      success = FALSE,
      reason = "No center zone points found",
      n_points = 0,
      x_shift = 0,
      y_shift = 0,
      method = "none",
      low_confidence = TRUE
    ))
  } else if(n_points < min_pts) {
    # For very few points, use simple averaging
    x_shift <- mean(center_points$X, na.rm = TRUE)
    y_shift <- mean(center_points$Y, na.rm = TRUE)
    
    cat("Using average method. Shifts:", x_shift, y_shift, "\n")
    
    return(list(
      success = TRUE,
      x_shift = -x_shift,  # Note: negating the shift
      y_shift = -y_shift,  # Note: negating the shift
      n_points = n_points,
      method = "average",
      low_confidence = TRUE
    ))
  }
  
  # For cases with enough points, use DBSCAN clustering
  clusters <- dbscan::dbscan(
    center_points %>% select(X, Y),
    eps = eps,
    minPts = min_pts
  )
  
  # If no clusters found, fall back to simple averaging
  if(max(clusters$cluster) == 0) {
    x_shift <- mean(center_points$X, na.rm = TRUE)
    y_shift <- mean(center_points$Y, na.rm = TRUE)
    
    cat("No clusters found. Using average method. Shifts:", x_shift, y_shift, "\n")
    
    return(list(
      success = TRUE,
      x_shift = -x_shift,  # Note: negating the shift
      y_shift = -y_shift,  # Note: negating the shift
      n_points = n_points,
      method = "average",
      low_confidence = TRUE
    ))
  }
  
  # Find the largest cluster
  main_cluster <- which.max(table(clusters$cluster))
  cluster_points <- center_points %>%
    filter(clusters$cluster == main_cluster)
  
  # Calculate centroid of main cluster
  x_shift <- mean(cluster_points$X, na.rm = TRUE)
  y_shift <- mean(cluster_points$Y, na.rm = TRUE)
  
  cat("Using cluster method. Shifts:", x_shift, y_shift, "\n")
  
  return(list(
    success = TRUE,
    x_shift = -x_shift,  # Note: negating the shift
    y_shift = -y_shift,  # Note: negating the shift
    n_points = nrow(cluster_points),
    method = "cluster",
    low_confidence = FALSE
  ))
}

#' Apply center shift correction to coordinates
#' @param data Data frame containing coordinates
#' @param shift_values List containing shift information
#' @return Data frame with corrected coordinates
#' @noRd
apply_center_shift <- function(data, shift_values) {
  if (!shift_values$success) {
    # If shift detection failed, use original coordinates
    data <- data %>%
      mutate(
        x_m = X,
        y_m = Y
      )
  } else {
    # Apply the shift correction
    data <- data %>%
      mutate(
        x_m = X + shift_values$x_shift,
        y_m = Y + shift_values$y_shift
      )
  }
  
  # Debug output
  cat("Shift applied. Sample of results:\n")
  print(head(data %>% select(X, Y, x_m, y_m)))
  
  return(data)
} 