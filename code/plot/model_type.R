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
         modelType = tolower(modelType),
         modelType = case_when(modelType == "random forests" ~ "random forest",
                               modelType == "logisitic regression model" ~ "logistic regression",
                               .default = modelType))

modelTypeTable <- modelTypeTable %>%
  mutate(modelClass = case_when(
    grepl("maxent", modelType) ~ "Maxent",
    grepl("logistic regression|generalized linear mixed model", modelType) ~ "regression models",
    grepl("boosted regression trees|random forest|xgboost", modelType) ~ "tree-based models",
    grepl("polynomial distributed lag models|non linear discriminant analysis|garp", modelType) ~ "specialized models",
    grepl("custom|zpoms", modelType) ~ "Custom models",
    grepl("spde|simple kriging", modelType) ~ "geostatistic models"
  ))

modelTypePlotTable <- modelTypeTable |> 
  group_by(modelType, modelClass) |> 
  summarise(n = n())

modelTypePlotTable$modelClass <- reorder(modelTypePlotTable$modelClass, modelTypePlotTable$n, FUN = sum)

ggplot(data = modelTypePlotTable, aes(x = modelClass, y = n, fill = modelType, label = modelType)) +
  labs(title =  element_text(paste0("Total number: ", sum(modelTypePlotTable$n)))) + 
  ylab("Count")+
  xlab("model type") + 
  geom_bar(position = "stack", stat = "identity") + 
  # geom_text(aes(x = modelClass, y = n, label = modelType),
  #           position = position_stack(vjust = .5), size = 3.9) + 
  geom_bar_text(position = "stack", reflow = TRUE) + 
  scale_fill_hue() +
  theme_bw(base_size = 16) +
  theme(legend.position="none")

s <- sum(modelTypePlotTable$n)

# get percentage of the model type
metricsModelType <- modelTypePlotTable |> 
  mutate(perc = n / s * 100)

metricsModelClass <- modelTypePlotTable |> 
  group_by(modelClass) |> 
  summarise(perc = sum(n) / s * 100)
