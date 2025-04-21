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