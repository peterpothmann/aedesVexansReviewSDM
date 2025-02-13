# init library
library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/paper/plots/"

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmental_variables.ods")) |> 
  filter(bestParameterVexans == "true")

calc <- envData |> 
  count(category, subcategory, calculation, sort = TRUE)
  