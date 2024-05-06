# edit column modelType and plot 

library(tidyverse)
library(readxl)
library(readODS)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")


# read raw 
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods")) 

# separete rows of modelTypes
modelTypeTable <- reviewTable |> 
  select(modelType) |> 
  separate_rows(modelType, sep = ',') |> 
  mutate(modelType = trimws(modelType),
         modelType = tolower(modelType),
         modelType = case_when(modelType == "random forests" ~ "random forest",
                               modelType == "logisitic regression model" ~ "logistic regression",
                               modelType == "generalized linear mixed model" ~ "GLMM",
                               modelType == "non linear discriminant analysis" ~ "non-linear discri. analysis",
                               modelType == "polynomial distributed lag models" ~ "PDL models",
                               .default = modelType))

modelTypePlotTable <- modelTypeTable |> 
  group_by(modelType) |> 
  summarise(n = n())

ggplot(data = modelTypePlotTable, aes(x = reorder(modelType, n), y = n)) +
  labs(title =  element_text(paste0("Gesamtanzahl: ", sum(modelTypePlotTable$n)))) + 
  geom_bar(stat = 'identity')
