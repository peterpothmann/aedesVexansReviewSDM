# Title: Visualize Four Field Importance Matrix
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0

# Description:
# This script processes environmental data to visualize the importance of environmental parameters
# in Aedes vexans species distribution models. It creates a four-field matrix that categorizes
# the variables based on their frequency of use and the proportion marked as important in the studies.

# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS, ggrepel
# - Input files "environmentalData.ods" must be located in the 'data/raw' directory
# - The generated plot will be saved in the 'paper/plots' directory

# Load necessary libraries
library(tidyverse)
library(readODS)
library(ggrepel)

# Set paths for main directory, data directory, and plot directory
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- paste0(mainPath, "paper/plots/")

# Import environmental data (ODS format)
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))

# Step 1: Prepare data for all studies (include all parameters, regardless of importance)
data <- envData |> 
  filter(bestParameterVexans == "true" | bestParameterVexans == "false") |> 
  filter(subcategory != "-") |> 
  filter(subcategory != "unspec.") |> 
  distinct(author, category, subcategory, .keep_all = TRUE) |> 
  count(category, subcategory, sort = TRUE) |> 
  rename("all" = n)  # Renaming column to "all" for clarity

# Step 2: Prepare data for only important parameters (best parameters marked as important)
important <- envData |> 
  filter(bestParameterVexans == "true") |> 
  filter(subcategory != "-") |> 
  distinct(author, category, subcategory, .keep_all = TRUE) |>  # Unique records only for "important"
  count(category, subcategory, sort = TRUE) |> 
  rename("important" = n)  # Renaming column to "important" for clarity

# Step 3: Combine the data from all studies and the important ones
combine <- left_join(data, important, by = c("category", "subcategory")) |>
  replace_na(list(important = 0)) |>  # Replace missing values in "important" column with 0
  mutate(perc = important / all) |>  # Calculate proportion of important studies
  mutate(allStand = {(all - min(all)) / (max(all) - min(all))},  # Standardize the "all" column
         label = paste0(category, "-", subcategory))  # Create a label for each combination

# Step 4: Create the plot
ggplot(combine, aes(perc, allStand, label = label)) +
  theme_minimal() + 
  # Add colored rectangles for the four-field matrix
  annotate("rect", xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5, fill = "#CC79A7", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5, fill = "#009E73", alpha = 0.3) +
  annotate("rect", xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1, fill = "#56B4E9", alpha = 0.3) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1, fill = "#F0E442", alpha = 0.3) +
  # Plot points and lines for the four-field matrix
  geom_point(size = 3) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = 0.5) +
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = 0.5) +
  geom_segment(aes(x = 1, xend = 1, y = 1, yend = 0), size = 0.5) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 0), size = 0.5) +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 1), size = 0.5) +
  geom_segment(aes(x = 0, xend = 1, y = 0.5, yend = 0.5), size = 0.5) +
  # Label points on the plot using ggrepel
  geom_label_repel(size = 4, 
                   seed = 9,
                   max.time = 3,
                   point.padding = 0.2,
                   box.padding = 0.45,
                   label.padding = 0.19) +
  xlab("Proportion of studies using the variable and flagging it as important") +
  ylab("Number of uses (max. standardisation)") +
  labs() +
  theme(text = element_text(size = 14))

# Step 5: Save the plot
ggsave(
  filename = "importance_environmental_params.png",  # Set file name
  device = "png",  # Save as PNG file
  path = plotPath,  # Save to specified path
  dpi = 900,  # Set high resolution (900 dpi)
  height = 6,  # Set height of the plot
  width = 8,  # Set width of the plot
  bg = "white"  # Set background color to white
)