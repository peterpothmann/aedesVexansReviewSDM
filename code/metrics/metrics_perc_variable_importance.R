library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods")) |> 
  filter(bestParameterEvalutation != "not relevant")

noIndication <- reviewData |> 
  filter(bestParameterEvalutation == "no assesment")

n_distinct(noIndication$key) / n_distinct(envData$key)