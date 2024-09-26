# get tzhe percentage of each observation period

# init library
library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/paper/plots/"

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))

observationPeriod <- envData |>
  rowwise() |> 
  mutate(observationPeriod = str_split(observationPeriod, pattern = " ")[[1]][1]) |> 
  ungroup() |> 
  count(observationPeriod) |> 
  mutate(perc = n / sum(n) * 100)
