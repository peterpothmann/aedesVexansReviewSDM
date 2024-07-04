# get metrics of the datasources used

library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))

nrow(envData)

metricsDataSource <- envData |> 
  count(envDataSource, sort = TRUE) |> 
  mutate(perc = (n / nrow(envData)) * 100)