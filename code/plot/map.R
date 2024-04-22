# create a map 

library(tidyverse)
library(readxl)
library(readODS)
library(sf)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")

# read country data
country <- st_read(paste0(dataDir, "raw/gadm/gadm_410-levels.gpkg"), layer = "ADM_0")

# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods")) |> 
  filter(is.na(sortOut))

# clean review data
reviewTable <- reviewTable |> 
  separate_rows(COUNTRY) |> 
  distinct(key, COUNTRY) |> 
  group_by(COUNTRY) |> 
  summarise(n = n())

mapData <- country |> 
  left_join(reviewTable)

ggplot(mapData) +
  geom_sf(mapping = aes(fill = n))
