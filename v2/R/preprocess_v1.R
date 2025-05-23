---
title: "Novel Exploration Growth Analysis in Elevated Plus Maze"
author: "Matt Zelko"
format: html            # Default path that can be overridden
---

```{r setup, include=FALSE}
# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(dbscan)    # For center detection algorithm
library(data.table)
library(purrr)
```

# Novel Exploration Growth in Elevated Plus Maze

This document provides a streamlined workflow for preprocessing Elevated Plus Maze (EPM) data and calculating Novel Exploration Growth (NEG) metrics. NEG tracks the first visit to discrete areas of the maze over time, capturing both exploratory and avoidance behaviors.

## Test Trial Data Structure Overview

The test trial data file has the following structure:

1.  Metadata for the test trial (rows 32-35):
    -   RatID
    -   Diet
    -   Strain
    -   Treatment Condition
2.  Data Structure:
    -   Column headers are in row 37
    -   Time series data starts from row 39
    -   Zone columns for Centre, Open arms and Closed arms

## Step 1: Load and Preprocess Data

The first step is to load the data files and run the preprocessing functions:

```{r load_preprocess}
# Load preprocessing functions
source("R/preprocess_v1.R")
```

# Raw Data / Experimental Data
```{r config}
# Dynamic configuration for data preprocessing
metadata_range <- "B32:B35"
metadata_fields <- c("RatID", "Diet", "Strain", "Treatment")
header_row <- 37
data_start_row <- 39
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
open_arm_cols <- c("In zone(open_arm_1 / center-point)", "In zone(open_arm_2 / center-point)")
closed_arm_cols <- c("In zone(closed_arm_1 / center-point)", "In zone(closed_arm_2 / center-point)")
centre_col <- "In zone(centre_zone / center-point)"
na_values <- c("", "-", "NA")
x_col <- "X center"
y_col <- "Y center"
time_col <- "Trial time"
```


# Example usage - load a single file
```{r load_single_file}
# Replace with your file path
file_path <- paste0(getwd(), "/Raw/Trial/NEG_Test_1.xlsx")

# Read and preprocess the data file
trials_data <- read_and_clean_data(
  file_path,
  metadata_range = metadata_range,
  metadata_fields = metadata_fields,
  header_row = header_row,
  data_start_row = data_start_row,
  required_cols = required_cols,
  open_arm_cols = open_arm_cols,
  closed_arm_cols = closed_arm_cols,
  centre_col = centre_col,
  na_values = na_values,
  x_col = x_col,
  y_col = y_col,
  time_col = time_col
)

# Examine the structure of the preprocessed data
glimpse(trials_data)

# Examine the time series data
glimpse(trials_data$data[[1]])
```

## Step 2: Visualize Raw and Corrected Trajectories (Optional)

The preprocessing includes center shift correction to normalize coordinates around (0,0). We can visualize this to verify the correction:

```{r visualize_trajectory, eval=FALSE}
# Function to unnest data for plotting
unnest_trials <- function(nested_data) {
  nested_data %>%
    unnest(data) %>%
    group_by(filename, RatID, Diet, Strain, Treatment)
}

# Plot original vs shifted coordinates
trials_data_unnested <- unnest_trials(trials_data)

ggplot(trials_data_unnested) +
  # Original coordinates
  geom_path(aes(x = X, y = Y), color = "grey", alpha = 0.5) +
  # Corrected coordinates
  geom_path(aes(x = x_m, y = y_m), color = "blue", alpha = 0.7) +
  # Origin point
  geom_point(aes(x = 0, y = 0), color = "red", size = 2) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Original vs. Corrected Trajectory",
       subtitle = "Grey = Original, Blue = Corrected")
```

## Step 3: Create Grid for Exploration Analysis

We need to create a grid of discrete areas to track where the animal has explored:

```{r create_grid}
# Unnest data and create grid
arm_grids <- create_complete_arm_grids(trials_data_unnested)
```

## Step 4: Visualize the Grid (Optional)

```{r visualize_grid, eval=FALSE}
# Plot the created grid bands for each arm type
ggplot() +
  # For open arms (vertical bands)
  geom_rect(data = arm_grids$data[[1]], 
            aes(xmin = x_min, xmax = x_max,
                ymin = depth_start, ymax = depth_end),
            fill = "lightblue", color = "darkgrey", alpha = 0.3) +
  # For closed arms (horizontal bands)
  geom_rect(data = arm_grids$data[[2]], 
            aes(xmin = depth_start, xmax = depth_end,
                ymin = y_min, ymax = y_max),
            fill = "lightgreen", color = "darkgrey", alpha = 0.3) +
  # Add points to show actual movement data
  geom_point(data = trials_data_unnested %>% filter(InOpen == 1 | InClosed == 1), 
             aes(x = x_m, y = y_m, color = factor(InOpen)), 
             alpha = 0.3, size = 0.5) +
  coord_equal() +
  scale_color_manual(values = c("red", "blue"), 
                    labels = c("Closed Arm", "Open Arm"),
                    name = "Location") +
  labs(title = "Grid Bands by Arm Type",
       x = "X Position (cm)",
       y = "Y Position (cm)") +
  theme_minimal()
```

## Step 5: Calculate Novel Exploration Growth

```{r calculate_exploration}
# Prepare data for exploration calculation
trials_with_exploration <- trials_data %>%
  # Convert data list to data frame and add rounded coordinates
  mutate(data2 = map(data, ~as.data.frame(.))) %>%
  mutate(data2 = map(data2, ~mutate(., x_mr = round(x_m), y_mr = round(y_m)))) %>%
  # Calculate exploration for each trial
  mutate(exploration_data = map(data2, ~calculate_grid_exploration(., arm_grids)))
```

## Step 6: Visualize Novel Exploration Growth

```{r plot_exploration}
# Plot exploration percentage over time by arm type
ggplot(trials_with_exploration$exploration_data[[1]], 
       aes(x = Time, y = exploration_percentage, color = grid_type)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Novel Exploration Growth Over Time",
       x = "Time (s)",
       y = "Percentage of Grid Squares Explored",
       color = "Arm Type") +
  ylim(0, 100)
```

# Optional: Process multiple files
## Step 1: Multiple Files Processing

If you need to process multiple files, you can use this approach:

```{r process_multiple_files, eval=FALSE}
# User-configurable parameters
file_pattern <- "^NEG_Test.*\\.xlsx$"  # Adjust this pattern to match your files
data_folder <- "Raw/Trial"             # Folder containing your data files

# Process multiple files with user-defined pattern
data_path <- paste0(getwd(), "/", data_folder)

# Process files with the specified pattern
all_exploration_data <- process_multiple_files(data_path, file_pattern,
metadata_range = metadata_range,
  metadata_fields = metadata_fields,
  header_row = header_row,
  data_start_row = data_start_row,
  required_cols = required_cols,
  open_arm_cols = open_arm_cols,
  closed_arm_cols = closed_arm_cols,
  centre_col = centre_col,
  na_values = na_values,
  x_col = x_col,
  y_col = y_col,
  time_col = time_col)
```

## Step 8: Summarize Exploration by Groups (Optional)

If you have multiple trials with different conditions, you can create summary visualizations:

```{r summarize_exploration, eval=FALSE}
# Create summary data
exploration_summary <- all_exploration_data %>%
  group_by(Diet, Strain, grid_type, Time) %>%
  summarise(
    mean_exploration_percentage = mean(exploration_percentage), 
    sd_exploration_percentage = sd(exploration_percentage),
    .groups = 'drop'
  )

# Plot summary
ggplot(exploration_summary %>% filter(grid_type != "total")) +
  geom_line(aes(x = Time, y = mean_exploration_percentage, 
                color = grid_type, 
                group = interaction(Diet, Strain, grid_type))) +
  geom_ribbon(aes(x = Time, 
                 ymin = mean_exploration_percentage - sd_exploration_percentage, 
                 ymax = mean_exploration_percentage + sd_exploration_percentage, 
                 fill = grid_type), alpha = 0.2) +
  scale_color_discrete(name = "Arm Type") +
  scale_fill_discrete(name = "Arm Type") +
  labs(title = "Novel Exploration Growth by Group",
       x = "Time (s)",
       y = "NEG (%)") +
  facet_grid(Diet ~ Strain) +
  theme_minimal()
```
