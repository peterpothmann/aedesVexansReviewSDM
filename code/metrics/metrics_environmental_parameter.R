# Title: Aedes Vexans Habitat Modeling - Data Processing Script
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0

# Description:
# It creates metrics for the subcategory and aggregation of environmental parameters, focusing on the best variables for the species.
# The script calculates the frequency and percentage of various environmental parameters such as category, subcategory, observation period, and calculation method.
# It also separates the best variables based on their relevance for *Aedes vexans* and calculates the corresponding metrics.

# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS
# - Ensure the input files are located in the 'data/raw/' directory as specified in the 'dataPath' variable
# - Run the script in an R environment with the required packages installed

# Load necessary libraries
library(tidyverse)
library(readODS)
library(xtable)

# Set paths for data directory and main project folder
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# Import environmental data from ODS file
envData <- read_ods(paste0(dataPath, "raw/environmental_variables.ods")) 

# Filter data for the best variables for Aedes vexans
envDataBestVariables <- envData |> 
  filter(bestParameterVexans == "true")

# Calculate frequency and percentage for all environmental variables (grouped by category, subcategory, observation period, and calculation method)
allVariables <- envData |> 
  count(category, subcategory, observationPeriod, calculation, sort = TRUE) |> 
  mutate(perc = n / sum(n) * 100)

# Calculate frequency and percentage for the best environmental variables (grouped by category, subcategory, observation period, and calculation method)
bestVariables <- envDataBestVariables |> 
  count(category, subcategory, observationPeriod, calculation, sort = TRUE) |> 
  mutate(perc = n / sum(n) * 100)

# Calculate frequency and percentage for observation periods of the best variables
observationPeriodVariables <- envDataBestVariables |> 
  count(observationPeriod) |> 
  mutate(perc = n / sum(n) * 100)
