# table with potenitial environmental parameter combinations

# plot to create a tree based on environmental data classification

library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) #|> 
#   filter(category == "weather")


t <- envData |> 
  count(category, subcategory, sort = TRUE)

write_ods(t, path = paste0(dataPath, "edit/envData/envData_combinations1.ods"))
