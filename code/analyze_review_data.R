# analyze literature database 

library(tidyverse)
library(readODS)


# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import all studies data  
dataFiles <- list.files(path = paste0(dataPath, "raw/literature_data"), pattern = "_all.csv", full.names = TRUE)
databases <- c("pubmed", "scopus", "webofscience")

allStudies <- tibble()
for (i in 1:3) {
  data <- read_csv(dataFiles[i], col_types = cols(.default = "c")) |> 
    mutate(source = databases[i]) # add a column specifying the database 
  allStudies <- allStudies |> 
    bind_rows(data)
}

# create metric table
# add the number of studies grouped by database and aggregated 
metrics <- allStudies |> 
  group_by(source) |> 
  summarise(NumberOfStudies = n ())

metrics <- metrics |> 
  add_row(source = "All databases",
          NumberOfStudies = sum(metrics$NumberOfStudies)) |> 
  mutate(source = paste0(source, " (input)"))

# import unique studies
uniqueStudies <- list.files(path = paste0(dataPath, "edit/literature_data"), pattern = "_unique.csv", full.names = TRUE) |> 
  map(read_csv,col_types = cols(.default = "c")) |>  # dont add the database here because I dont know how zotero removed the duplicate studies
  bind_rows() |> 
  distinct(Key, .keep_all = TRUE)

metrics <- metrics |> 
  add_row(source = "Included studies (duplicate removal)",
          NumberOfStudies = nrow(uniqueStudies)) |> 
  add_row(source = "Excluded studies (duplicate removal)",
          NumberOfStudies = nrow(allStudies) - nrow(uniqueStudies)) # here I dont count the unique studies
  

# get the number of studies with relevant title
relevantTitle <- read_ods(paste0(dataPath, "edit/literature_data/relevant_title.ods"))

releventStudies <- relevantTitle |> 
  filter(titleRelevant == TRUE) |> 
  nrow()

removedStudies <- nrow(relevantTitle) - releventStudies

metrics <- metrics |> 
  add_row(source = "Included studies (based on title)",
          NumberOfStudies = releventStudies) |> 
  add_row(source = "Excluded studies (based on title)",
          NumberOfStudies = removedStudies)

# now add the excluded and included studies with relevant titles --> the ones that where actually included in the study 
excludedStudies <- relevantTitle |> 
  filter(included != "included") |> 
  nrow()

includedStudies <- relevantTitle |> 
  filter(included == "included") |> 
  nrow()

metrics <- metrics |> 
  add_row(source = "Excluded Studies (Whole study)",
          NumberOfStudies = excludedStudies) |> 
  add_row(source = "Included Studies (Whole study",
          NumberOfStudies = includedStudies)
