# edit column modelType and plot 

library(tidyverse)
library(readxl)
library(readODS)
library(ggfittext)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")
plotPath <- paste0(mainDir, "paper/plots/")

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
    grepl("Stochastic partial differential equation|Simple kriging", modelType) ~ "Geostatistic models"
  ))

modelTypePlotTable <- modelTypeTable |> 
  group_by(modelType, modelClass) |> 
  summarise(n = n())

pal <- c('#D9CCE3', '#AE76A3', '#882E72', '#1965B0', '#5289C7', '#7BAFDE', '#4EB265', '#90C987', '#CAE0AB', '#F7F056', '#F7CB45', '#E8601C', '#A5170E')

modelTypePlotTable$modelClass <- reorder(modelTypePlotTable$modelClass, modelTypePlotTable$n, FUN = sum)

ggplot(data = modelTypePlotTable, aes(x = modelClass, y = n, fill = modelType, label = modelType)) +
  labs(title =  "Frequency of different model types used",
       subtitle = element_text(paste0("Total number: ", sum(modelTypePlotTable$n)))) + 
  ylab("Frequency of applied models")+
  xlab("Model type") + 
  geom_bar(position = "stack", stat = "identity") + 
  # geom_text(aes(x = modelClass, y = n, label = modelType),
  #           position = position_stack(vjust = .5), size = 3.9) + 
  geom_bar_text(position = "stack", reflow = TRUE, min.size = 12) + 
  #scale_fill_hue(c = 90) +
  scale_fill_manual(values = pal) +
  theme_bw(base_size = 22) +
  theme(legend.position="none",
        plot.title = element_text(face="bold"))

ggsave(filename = "model_type.png",
       device = "png",
       path = plotPath,
       dpi = 600,
       height = 10,
       width = 15)

sumModelTypes <- sum(modelTypePlotTable$n)

# get percentage of the model type
metricsModelType <- modelTypePlotTable |> 
  mutate(perc = n / sumModelTypes * 100)

metricsModelClass <- modelTypePlotTable |> 
  group_by(modelClass) |> 
  summarise(perc = sum(n) / sumModelTypes * 100)
