library(tidyverse)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods")) 

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) |> 
  filter(subcategory == "artificial") |> 
  distinct(key)

# get the studies that use artificial classes
arti <- left_join(envData, reviewData)

# percantage of studies with colin analyses 
colin <- arti |> 
  count(colinearityAcoounted) |> 
  mutate(perc = n / sum(n))
