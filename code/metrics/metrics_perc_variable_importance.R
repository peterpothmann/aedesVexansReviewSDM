# Title: Aedes Vexans Habitat Modeling - Data Processing Script
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0

# Description:
# This script processes the review data for Aedes vexans species distribution models, 
# filtering out studies based on the relevance of the best parameter evaluation. 
# It calculates the percentage of studies with no assessment of the best parameter 
# relative to the total number of studies considered.

# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS
# - Ensure the input file "review_aedes_vexans.ods" is located in the 'data/raw' directory 
#   as specified in the 'dataPath' variable
# - Output is the proportion of studies with "no assessment" of the best parameter 
#   in relation to all the studies considered.

# Load necessary libraries
library(tidyverse)
library(readODS)

# Set paths for data directory and main project folder
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# Import review data and filter out irrelevant evaluations
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods")) |> 
  filter(bestParameterEvalutation != "not relevant")

# Filter studies where no assessment was made for the best parameter
noIndication <- reviewData |> 
  filter(bestParameterEvalutation == "no assesment")

# Calculate the proportion of studies with no assessment
noIndicationRatio <- n_distinct(noIndication$key) / n_distinct(reviewData$key)

# Print the result
noIndicationRatio
