# Title: Aedes Vexans Habitat Modeling - Data Processing Script
# Author: Peter Pothmann
# Date: 13.02.2025
# Version: 1.0
# License: CC BY 4.0

# Description:
# This R script analyzes the literature database.
# It imports multiple CSV files from different databases (PubMed, Scopus, Web of Science), aggregates study counts, and generates a set of metrics regarding the number of included and excluded studies, duplicate removal, and title relevance.
# The script processes the unique studies, assesses relevant titles, and adds them to a summary table for an overview of the literature.

# Reproducibility:
# - Requires R version 4.x or higher
# - Dependencies: tidyverse, readODS
# - Ensure the input files are located in the 'data/raw/literature_data' and 'data/edit/literature_data' directories as specified in the 'dataPath' variable
# - Run the script in an R environment with the required packages installed
# Load necessary libraries
library(tidyverse)
library(readODS)

# Set paths for data directory and main project folder
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# Import all studies data from multiple databases
dataFiles <- list.files(path = paste0(dataPath, "raw/literature_data"), pattern = "_all.csv", full.names = TRUE)
databases <- c("pubmed", "scopus", "webofscience")

allStudies <- tibble()
for (i in 1:3) {
  data <- read_csv(dataFiles[i], col_types = cols(.default = "c")) |> 
    mutate(source = databases[i]) # Add a column specifying the database
  allStudies <- allStudies |> 
    bind_rows(data)
}

# Create metric table and aggregate the number of studies by database
metrics <- allStudies |> 
  group_by(source) |> 
  summarise(NumberOfStudies = n())

metrics <- metrics |> 
  add_row(source = "All databases",
          NumberOfStudies = sum(metrics$NumberOfStudies)) |> 
  mutate(source = paste0(source, " (input)"))

# Import unique studies and handle duplicate removal
uniqueStudies <- list.files(path = paste0(dataPath, "edit/literature_data"), pattern = "_unique.csv", full.names = TRUE) |> 
  map(read_csv, col_types = cols(.default = "c")) |>  # Don't add database info here because Zotero has removed duplicates
  bind_rows() |> 
  distinct(Key, .keep_all = TRUE)

metrics <- metrics |> 
  add_row(source = "Included studies (duplicate removal)",
          NumberOfStudies = nrow(uniqueStudies)) |> 
  add_row(source = "Excluded studies (duplicate removal)",
          NumberOfStudies = nrow(allStudies) - nrow(uniqueStudies)) # Exclude unique studies

# Get the number of studies with relevant titles based on filtering
relevantTitle <- read_ods(paste0(dataPath, "edit/literature_data/relevant_title.ods"))

relevantStudies <- relevantTitle |> 
  filter(titleRelevant == TRUE) |> 
  nrow()

removedStudies <- nrow(relevantTitle) - relevantStudies

metrics <- metrics |> 
  add_row(source = "Included studies (based on title)",
          NumberOfStudies = relevantStudies) |> 
  add_row(source = "Excluded studies (based on title)",
          NumberOfStudies = removedStudies)

# Add the number of included and excluded studies based on the whole study inclusion status
excludedStudies <- relevantTitle |> 
  filter(included != "included") |> 
  nrow()

includedStudies <- relevantTitle |> 
  filter(included == "included") |> 
  nrow()

metrics <- metrics |> 
  add_row(source = "Excluded Studies (Whole study)",
          NumberOfStudies = excludedStudies) |> 
  add_row(source = "Included Studies (Whole study)",
          NumberOfStudies = includedStudies)