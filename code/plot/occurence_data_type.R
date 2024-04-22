# viz the three different occurence data types
library(tidyverse)
library(readxl)
library(readODS)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods"))

plot(envData$occurenceDataType)

ggplot(envData) +
  geom_bar(mapping = aes(occurenceDataType))
