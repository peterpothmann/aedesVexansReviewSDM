# Title: Aedes Vexans Habitat Modeling - Data Processing Script
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0

# Description:
# This script calculates the percentage distribution of different observation periods 
# used in the environmental data for Aedes vexans species distribution modeling. 
# The observation periods are extracted from the environmental data, then the percentage of 
# each period is computed and stored in a summary table.

# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS
# - Ensure the input file "environmental_variables.ods" is located in the 'data/raw' directory 
#   as specified in the 'dataPath' variable

# Load necessary libraries
library(tidyverse)
library(readODS)

# Set paths for data directory and main project folder
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/paper/plots/"

# Import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmental_variables.ods"))

# Process the observation period data, count the occurrences, and calculate percentages
observationPeriod <- envData |>
  rowwise() |> 
  mutate(observationPeriod = str_split(observationPeriod, pattern = " ")[[1]][1]) |> 
  ungroup() |> 
  count(observationPeriod) |> 
  mutate(perc = n / sum(n) * 100)
