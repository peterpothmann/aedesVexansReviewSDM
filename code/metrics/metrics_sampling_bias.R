# Title: Aedes Vexans Habitat Modeling - Data Processing Script
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0

# Description:
# This script processes environmental and review data to assess the importance of 
# artificial classes in the context of Aedes vexans species distribution models. 
# It joins data to identify best parameter relevance, spatial sampling bias. 
# The processed data is used to visualize the importance of environmental
# parameters across different studies.

# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS
# - Input files "environmental_variables.ods" and "review_aedes_vexans.ods" must be located 
#   in the 'data/raw' directory
# - The generated plots will be saved in the 'paper/plots' directory

# Load necessary libraries
library(tidyverse)
library(readODS)

# Set paths for main directory, data directory, and plot directory
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- paste0(mainPath, "paper/plots/")

# Import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmental_variables.ods"))

# Import review data for Aedes vexans species models
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods"))

# Transform data by filtering for best parameter relevance and artificial subcategory
data <- envData |> 
  filter(bestParameterVexans == "true" | bestParameterVexans == "false") |> 
  filter(subcategory == "artificial") |> 
  distinct(author, subcategory, .keep_all = TRUE) |> 
  select(author, subcategory, bestParameterVexans) |> 
  left_join(reviewData |> select(author, spatialSamplingBiasAccounted)) |> 
  count(subcategory, bestParameterVexans, spatialSamplingBiasAccounted)

# Import review data for artificial classes and collinearity considerations (for further analysis)
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods")) |>
  select(key, colinearityAcoounted)

# Join the environmental data with the review data to assess artificial classes
arti <- envData |>
  filter(subcategory == "artificial") |>
  left_join(reviewData)

# Count the number of occurrences of artificial classes across all studies
artiAll <- arti |>
  filter(bestParameterVexans == "true" | bestParameterVexans == "false") |>
  count(category, subcategory) |>
  rename("all" = n)

# Count the number of occurrences of artificial classes marked as "important"
artiBest <- arti |>
  filter(bestParameterVexans == "true") |>
  count(category, subcategory) |>
  rename("important" = n)

# Combine the all and important artificial classes data for final analysis
artiCombine <- left_join(artiAll, artiBest)
