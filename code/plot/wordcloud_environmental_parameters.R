# make wordcloud for internal presentation

library(wordcloud2)
library(readODS)
library(tidyverse)

# set patidyverse# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) |> 
  mutate(envData = tolower(envData)) |> 
  count(envData)
  
par(bg="black") 
wordcloud2(envData)
