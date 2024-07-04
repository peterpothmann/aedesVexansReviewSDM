# create metrics for subsubcategory and aggregation of environmental parameters

library(tidyverse)
library(readODS)
library(xtable)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) 

envDataBestParams <- envData |> 
  filter(bestParameterVexans == "true")


allParams <- envData |> 
  count(category, subcategory, observationPeriod, calculation, sort = TRUE) |> 
  mutate(perc = n / sum(n) * 100) |> 
  filter(perc > 2)
print(xtable(allParams), include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = I)

bestParams <- envDataBestParams |> 
  count(category, subcategory, observationPeriod, calculation, sort = TRUE) |> 
  mutate(perc = n / sum(n) * 100) |> 
  filter(perc > 2)

print(xtable(bestParams), include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = I)
