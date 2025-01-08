# get metrics of the datasources used

library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))

# import review data
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods"))

metricsDataSource <- envData |> 
  separate_rows(envDataSource, sep = ",") |>
  mutate(envDataSource = trimws(envDataSource)) |> 
  count(envDataSource, sort = TRUE) |> 
  mutate(perc = (n / sum(n)) * 100)

# class: data-driven
dataDrivenStudies <- reviewData |> 
  filter(modelApproaches == "data driven") |> 
  pull(key)

dataDrivenSources <- envData |>
  filter(key %in% dataDrivenStudies) |> 
  separate_rows(envDataSource, sep = ",") |>
  mutate(envDataSource = trimws(envDataSource)) |> 
  count(envDataSource, sort = TRUE) |> 
  mutate(perc = (n / sum(n)) * 100)
