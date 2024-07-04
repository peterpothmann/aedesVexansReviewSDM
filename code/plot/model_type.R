# edit column modelType and plot 

library(tidyverse)
library(readxl)
library(readODS)
library(ggfittext)

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
         #modelType = tolower(modelType),
         modelType = case_when(modelType == "random forests" ~ "random forest",
                               modelType == "Logisitic regression model" ~ "Logistic regression",
                               .default = modelType))

modelTypeTable <- modelTypeTable %>%
  mutate(modelClass = case_when(
    grepl("Maxent", modelType) ~ "Maxent",
    grepl("Logistic regression|Generalized linear mixed model", modelType) ~ "Regression models",
    grepl("Boosted Regression Trees|Random forest|XGBoost", modelType) ~ "Tree-based models",
    grepl("Polynomial distributed lag models|Non Linear Discriminant Analysis|Genetic Algorithm for Rule Set Production", modelType) ~ "Specialized models",
    grepl("Custom|ZPOMs", modelType) ~ "Custom models",
    grepl("Stochastic partial differential equation|Simple kriging", modelType) ~ "geostatistic models"
  ))

modelTypePlotTable <- modelTypeTable |> 
  group_by(modelType, modelClass) |> 
  summarise(n = n())

modelTypePlotTable$modelClass <- reorder(modelTypePlotTable$modelClass, modelTypePlotTable$n, FUN = sum)

ggplot(data = modelTypePlotTable, aes(x = modelClass, y = n, fill = modelType, label = modelType)) +
  labs(title =  element_text(paste0("Total number: ", sum(modelTypePlotTable$n)))) + 
  ylab("Frequency of models applied")+
  xlab("Model type") + 
  geom_bar(position = "stack", stat = "identity") + 
  # geom_text(aes(x = modelClass, y = n, label = modelType),
  #           position = position_stack(vjust = .5), size = 3.9) + 
  geom_bar_text(position = "stack", reflow = TRUE) + 
  scale_fill_hue() +
  theme_bw(base_size = 16) +
  theme(legend.position="none")

sumModelTypes <- sum(modelTypePlotTable$n)

# get percentage of the model type
metricsModelType <- modelTypePlotTable |> 
  mutate(perc = n / sumModelTypes * 100)

metricsModelClass <- modelTypePlotTable |> 
  group_by(modelClass) |> 
  summarise(perc = sum(n) / sumModelTypes * 100)
