# Title: Aedes Vexans Habitat Modeling - Data Processing Script
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0
# 
# Description:
# This R script processes and analyzes environmental and literature data related to 
# *Aedes vexans* habitat models. It imports datasets from ODS files and performs data merging and collinearity analysis.
# The script outputs the percentage of studies that accounted for collinearity in their models.
# 
# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS
# - Ensure the input files are located in the 'data/raw/' directory as specified in the 'dataPath'
# - Run the script in an R environment with the required packages installed
# 
# Usage:
# 1. Set the correct file paths in the 'mainPath' and 'dataPath' variables.
# 2. Place the dataset files (e.g., 'review_aedes_vexans.ods', 'environmental_variable.ods') in the specified directory.
# 3. Run the script in an R environment with the necessary dependencies installed.
# 4. Review the processed data outputs for further analysis.

library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods")) 

# percantage of studies with colin analyses 
colin <- reviewData |> 
  distinct(author, .keep_all = TRUE) |> 
  count(colinearityAcoounted) |> 
  mutate(perc = n / sum(n))
