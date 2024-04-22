# in diesem Skript möchte ich die einzelnen Variablen standardisieren und vielleicht sogar aggregieren

library(tidyverse)
library(readODS)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")


# read raw data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods")) |> 
  filter(is.na(sortOut))

# edit column occurencedataSource
occurenceDataSourceTable <- reviewTable |> 
  distinct(key, .keep_all = T) |> # remove duplicate entries 
  separate_rows(occurenceDataSource, sep = ",") |> 
  select(occurenceDataSource) |> 
  mutate(occurenceDataSource = trimws(occurenceDataSource),
         occurenceDataSource = case_when(
           occurenceDataSource == "CULBASE" ~ "Database",
           occurenceDataSource == "Kramer" ~ "Database",
           occurenceDataSource == "VBORNET" ~ "Database",
           occurenceDataSource == "vectorNet" ~ "Database",
           occurenceDataSource == "gbif" ~ "Database",
           .default = occurenceDataSource))

ggplot(occurenceDataSourceTable) +
  geom_bar(mapping = aes(occurenceDataSource))

