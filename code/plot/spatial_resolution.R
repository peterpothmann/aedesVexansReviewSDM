# spatialResolution plot
# 
library(tidyverse)
library(readODS)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")

# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods")) |> 
  select(spatialResolutionKm) 

reviewTable <- reviewTable |> 
  mutate(spatialResolutionKm = as.numeric(spatialResolutionKm)) |> 
  count(spatialResolutionKm)

ggplot(reviewTable, aes(spatialResolutionKm)) +
  geom_histogram(binwidth = 0.1)+
  geom_freqpoly()
# gefällt mir nicht so. sollte ich wahrscheinlich eher als text beschreiben 
  