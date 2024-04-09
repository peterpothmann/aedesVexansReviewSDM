# in diesem Skript möchte ich die einzelnen Variablen standardisieren und vielleicht sogar aggregieren

library(tidyverse)
library(readxl)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")


# read raw data
reviewTable <- read_excel(paste0(dataDir, "raw/review_aedes_vexans.xlsx")) |> 
  filter(is.na(sortOut))

# edit column modelType
# ---

# separete rows of modelTypes
modelTypeTable <- reviewTable |> 
  select(modelType) |> 
  separate_rows(modelType, sep = ',') |> 
  mutate(modelType = trimws(modelType),
         modelType = tolower(modelType),
         modelType = case_when(modelType == "random forests" ~ "random forest",
                               modelType == "logisitic regression model" ~ "logistic regression",
                               .default = modelType))

modelTypePlotTable <- modelTypeTable |> 
  group_by(modelType) |> 
  summarise(n = n())

ggplot(data = modelTypePlotTable, aes(x = modelType, y = n)) +
  labs(title =  element_text(paste0("Gesamtanzahl: ", sum(modelTypePlotTable$n)))) + 
  geom_bar(stat = 'identity')


# edit column occurencedataSource
occurenceDataSourceTable <- reviewTable |> 
  separate_rows(occurenceDataSource, sep = ",") |> 
  select(occurenceDataSource) |> 
  mutate(occurenceDataSource = trimws(occurenceDataSource)) |> 
  group_by(occurenceDataSource) |> 
  summarise(n = n())

