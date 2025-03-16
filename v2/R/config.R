#' Default configuration for EPM data processing
#' 
#' This configuration file defines the structure for processing Elevated Plus Maze (EPM) data.
#' Users can modify these settings to match their experimental setup and data format.
#' 
#' @examples
#' # Create custom configuration for different metadata fields
#' my_config <- create_epm_config(
#'   metadata = list(
#'     fields = c("Mouse ID", "Group", "Sex", "Age")
#'   )
#' )
#' 
#' # Modify zone patterns for different naming conventions
#' my_config <- create_epm_config(
#'   zones = list(
#'     center = list(
#'       pattern = "In zone\\(middle_zone.*\\)",
#'       required = TRUE
#'     )
#'   )
#' )
#' @export
default_epm_config <- list(
  # Example Metadata configuration
  metadata = list(
    start_row = 32,      # First row of metadata
    end_row = 35,        # Last row of metadata
    column = "B",        # Column containing metadata values
    fields = c("Rat ID", "Diet", "Strain", "Treatment")  # Names for metadata fields
  ),
  
  # Data structure configuration
  data_structure = list(
    header_row = 37,     # Row containing column headers
    data_start_row = 39, # First row of actual data
    required_columns = c(
      "Trial time",      # Time column name
      "X center",        # X coordinate column name
      "Y center"         # Y coordinate column name
    )
  ),
  
  # Zone configuration: Only reads the "In zone" columns
  zones = list(
    center = list(
      pattern = "In zone\\(centre_zone|center_zone.*\\)",  # Matches "In zone(centre_zone / center-point)"
      required = TRUE
    ),
    open_arms = list(
      pattern = "In zone\\(open_arm_[0-9].*\\)",          # Matches "In zone(open_arm_1 / center-point)"
      required = TRUE,
      expected_count = 2
    ),
    closed_arms = list(
      pattern = "In zone\\(closed_arm_[0-9].*\\)",        # Matches "In zone(closed_arm_1 / center-point)"
      required = TRUE,
      expected_count = 2
    )
  ),
  
  # Coordinate processing configuration
  coordinates = list(
    center_detection = TRUE,    # Detect center point from data
    shift_correction = TRUE,    # Apply coordinate shift correction
    distance_calculation = TRUE # Calculate distances between points
  )
)

#' Validate EPM configuration
#' @param config List containing configuration parameters
#' @param base_config Optional base configuration to compare against
#' @return Validated and completed configuration list
#' @export
validate_epm_config <- function(config, base_config = default_epm_config) {
  if (!is.list(config)) {
    stop("Configuration must be a list")
  }
  
  # Merge with default config, keeping user's values where specified
  config <- modifyList(base_config, config)
  
  # Validate metadata configuration
  if (config$metadata$start_row >= config$metadata$end_row) {
    stop("Metadata start_row must be less than end_row")
  }
  
  if (length(config$metadata$fields) == 0) {
    stop("At least one metadata field must be specified")
  }
  
  # Validate data structure
  if (config$data_structure$header_row >= config$data_structure$data_start_row) {
    stop("Header row must be before data start row")
  }
  
  if (length(config$data_structure$required_columns) == 0) {
    stop("At least one required column must be specified")
  }
  
  # Validate zone configuration
  if (!all(sapply(config$zones, function(x) !is.null(x$required)))) {
    stop("All zone configurations must specify if they are required")
  }
  
  # Additional validation for expected counts
  for (zone in names(config$zones)) {
    if (!is.null(config$zones[[zone]]$expected_count)) {
      if (!is.numeric(config$zones[[zone]]$expected_count) || 
          config$zones[[zone]]$expected_count <= 0) {
        stop("Expected count for ", zone, " must be a positive number")
      }
    }
  }
  
  return(config)
}

#' Create a new EPM configuration
#' @param ... Named parameters to override default configuration
#' @return Validated configuration list
#' @examples
#' # Create config with custom metadata fields
#' my_config <- create_epm_config(
#'   metadata = list(
#'     fields = c("Mouse ID", "Treatment", "Age")
#'   )
#' )
#' @export
create_epm_config <- function(...) {
  user_config <- list(...)
  validate_epm_config(user_config)
} 