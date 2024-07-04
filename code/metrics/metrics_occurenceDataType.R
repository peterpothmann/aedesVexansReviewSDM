# metrics of occurence data type

library(tidyverse)
library(readODS)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")

# read raw 
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods"))

metricsOccurenceDataTypee <- reviewTable |> 
  count(occurenceDataType, sort = TRUE) |> 
  mutate(perc = n / sum(n) * 100)
