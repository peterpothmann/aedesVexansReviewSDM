# metrics occurence data

library(tidyverse)
library(readODS)
library(ggfittext)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")

# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods"))

# occurenceDataType
reviewTable |> 
  separate_rows(occurenceDataType, sep = ",") |> 
  count(occurenceDataType)

# lifestageofData
metricsLifestageOfData <- reviewTable |> 
  separate_rows(lifestageOfData, sep = ",") |> 
  mutate(lifestageOfData = trimws(lifestageOfData)) |> 
  count(lifestageOfData)

s <- sum(metricsLifestageOfData$n)

metricsLifestageOfData <- metricsLifestageOfData |> 
  mutate(perc = n / s * 100)
