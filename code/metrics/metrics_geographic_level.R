# Title: Aedes Vexans Habitat Modeling - Data Processing Script
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0

# Description:
# This R script processes environmental and review data related to *Aedes vexans* habitat modeling.
# It imports and analyzes datasets regarding environmental data sources, geographic levels, and model approaches.
# The script calculates metrics on the usage of different environmental data sources and geographic levels 
# across studies, and provides a summary of the most commonly used sources and levels.

# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS
# - Ensure the input files are located in the 'data/raw/' directory as specified in the 'dataPath' variable
# - Run the script in an R environment with the required packages installed

library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmental_variable.ods"))

# import review data
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods"))

# import geograhic level information of environmental data
envDataSourceGeographicLevel <- read_ods(paste0(dataDir, "raw/geographic_level_environmental_data_source.ods"))

# mean percentage of geographic level per author/study
meanPercEnvDataSource <- left_join(envData, envDataSourceGeographicLevel) |>
  semi_join(reviewData |> 
              filter(geographicLevel == "national" | geographicLevel == "sub-national") |>  
              select(author)) |> 
  count(author, geographicLevel) |> 
  complete(geographicLevel, author, fill = list(n = 0)) |>
  group_by(author) |> 
  mutate(perc = (n/sum(n) * 100)) |> 
  group_by(geographicLevel) |> 
  summarise(mean = mean(perc))
