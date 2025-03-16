#' @import dplyr tidyr readxl
NULL

#' Read metadata from Excel file
#' @param file_path Path to Excel file
#' @param config EPM configuration list
#' @return Tibble containing metadata
#' @noRd
read_metadata <- function(file_path, config) {
  tryCatch({
    # Read metadata using configured rows and column
    metadata <- readxl::read_xlsx(
      file_path,
      range = paste0(
        config$metadata$column,
        config$metadata$start_row,
        ":",
        config$metadata$column,
        config$metadata$end_row
      ),
      col_names = FALSE,  # Explicitly set col_names to FALSE
      .name_repair = "minimal"  # Don't try to repair names
    )
    
    if (nrow(metadata) != length(config$metadata$fields)) {
      stop("Metadata rows do not match expected fields")
    }
    
    # Process metadata - explicitly name the column
    metadata <- metadata %>%
      setNames("value") %>%  # Explicitly name the column instead of using rename
      mutate(field = config$metadata$fields) %>%
      pivot_wider(names_from = field, values_from = value)
    
    # Debug output
    cat("Metadata read successfully:\n")
    print(metadata)
    
    return(metadata)
    
  }, error = function(e) {
    cat("Error details:\n")
    print(e)
    stop("Error reading metadata: ", e$message)
  })
}

#' Read time series data from Excel file
#' @param file_path Path to Excel file
#' @param config EPM configuration list
#' @return Tibble containing time series data
#' @noRd
read_time_series <- function(file_path, config) {
  tryCatch({
    # First read the headers to check column names
    headers <- readxl::read_xlsx(
      file_path, 
      skip = config$data_structure$header_row - 1, 
      n_max = 1,
      .name_repair = "minimal"
    )
    
    cat("Available columns:\n")
    print(names(headers))
    
    # Check for required columns
    missing_cols <- setdiff(config$data_structure$required_columns, names(headers))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    # Read the actual data
    data <- readxl::read_xlsx(
      file_path,
      skip = config$data_structure$data_start_row - 1,
      col_names = names(headers),
      na = c("", "-", "NA")
    )
    
    cat("Rows read:", nrow(data), "\n")
    
    return(data)
    
  }, error = function(e) {
    cat("Error reading time series data:\n")
    print(e)
    stop(e)
  })
}

#' Process zone data
#' @param data Raw time series data
#' @param config EPM configuration list
#' @return Processed data with combined zone indicators
#' @noRd
process_zones <- function(data, config) {
  tryCatch({
    # Find zone columns using patterns
    zone_cols <- list()
    for (zone_type in names(config$zones)) {
      pattern <- config$zones[[zone_type]]$pattern
      matching_cols <- grep(pattern, names(data), value = TRUE)
      
      if (length(matching_cols) == 0 && config$zones[[zone_type]]$required) {
        stop("No columns found for required zone type: ", zone_type)
      }
      
      if (!is.null(config$zones[[zone_type]]$expected_count) && 
          length(matching_cols) != config$zones[[zone_type]]$expected_count) {
        stop("Expected ", config$zones[[zone_type]]$expected_count, 
             " columns for zone type ", zone_type, 
             " but found ", length(matching_cols))
      }
      
      zone_cols[[zone_type]] <- matching_cols
    }
    
    # Convert zone columns to numeric
    data <- data %>%
      mutate(across(unlist(zone_cols), ~as.numeric(ifelse(. == "-", 0, .))))
    
    # Create combined indicators for each zone type
    for (zone_type in names(zone_cols)) {
      cols <- zone_cols[[zone_type]]
      if (length(cols) > 0) {
        data[[paste0("In", zone_type)]] <- apply(data[cols], 1, max, na.rm = TRUE)
      }
    }
    
    return(data)
    
  }, error = function(e) {
    cat("Error processing zones:\n")
    print(e)
    stop(e)
  })
}

#' Process coordinates and calculate metrics
#' @param data Data frame with X, Y coordinates
#' @param config EPM configuration list
#' @return Processed data with calculated metrics
#' @noRd
process_coordinates <- function(data, config) {
  tryCatch({
    if (!all(c("X", "Y", "Time") %in% names(data))) {
      stop("Missing required coordinate columns")
    }
    
    cat("\nProcessing coordinates...\n")
    
    # If coordinate processing is enabled in config
    if (config$coordinates$center_detection) {
      cat("Center detection enabled, attempting shift correction...\n")
      
      # Get shift values
      shift_values <- detect_center_shift(data)
      
      # Apply the correction
      data <- apply_center_shift(data, shift_values)
    } else {
      cat("Center detection disabled, using raw coordinates...\n")
      # If center detection is disabled, just copy coordinates
      data <- data %>%
        mutate(
          x_m = X,
          y_m = Y
        )
    }
    
    # Calculate distance and velocity
    processed_data <- data %>%
      mutate(
        # Calculate distance between consecutive points
        distance = sqrt((x_m - lag(x_m))^2 + (y_m - lag(y_m))^2),
        # Calculate velocity (distance/time)
        velocity = distance / (Time - lag(Time))
      ) %>%
      select(Time, X, Y, starts_with("In"), x_m, y_m, distance, velocity)
    
    cat("Coordinate processing complete.\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error processing coordinates:\n")
    print(e)
    stop(e)
  })
}

#' Read and clean EPM trial data
#' @param file_path Path to Excel file
#' @param config EPM configuration list
#' @return Nested tibble with metadata and processed time series data
#' @export
read_and_clean_data <- function(file_path, config = default_epm_config) {
  # Debug output
  cat("Reading file:", file_path, "\n")
  cat("Working directory:", getwd(), "\n")
  
  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  if (!grepl("\\.xlsx$", file_path)) {
    stop("File must be an Excel (.xlsx) file")
  }
  
  # Validate configuration
  config <- validate_epm_config(config)
  
  # Read metadata with error handling
  metadata <- tryCatch({
    read_metadata(file_path, config)
  }, error = function(e) {
    cat("Failed to read metadata:\n")
    print(e)
    stop(e)
  })
  
  # Read time series data
  raw_data <- read_time_series(file_path, config)
  
  # Process zone data
  processed_data <- process_zones(raw_data, config)
  
  # Clean coordinates and calculate basic metrics
  final_data <- processed_data %>%
    select(
      Time = `Trial time`,
      X = `X center`,
      Y = `Y center`,
      starts_with("In")
    ) %>%
    # Remove rows where X or Y is NA
    filter(!is.na(X), !is.na(Y))
  
  # Process coordinates and calculate metrics
  final_data <- process_coordinates(final_data, config)
  
  # Create nested structure
  result <- metadata %>%
    mutate(
      filename = basename(file_path),
      data = list(final_data)
    )
  
  # Debug output
  cat("\nProcessed data structure:\n")
  print(str(result$data[[1]]))
  
  # Verify data structure
  cat("\nVerifying data structure:\n")
  cat("Is data a list column?", is.list(result$data), "\n")
  cat("Is first element a data frame?", is.data.frame(result$data[[1]]), "\n")
  cat("Number of rows in first data frame:", nrow(result$data[[1]]), "\n")
  
  return(result)
} 