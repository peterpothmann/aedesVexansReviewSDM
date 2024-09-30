# bar plot for model approaches
library(tidyverse)
library(readODS)
library(ggfittext)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")
plotPath <- paste0(mainDir, "paper/plots/")
tmpDir <- "C:/Users/pothmann/tmp/"

# import environmental data
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods"))

# separete rows of modelTypes
modelTypeTable <- reviewData |>
  select(modelType, modelApproaches) |>
  separate_rows(modelType, sep = ',') |>
  mutate(modelType = trimws(modelType),
         #modelType = tolower(modelType),
         modelType = case_when(modelType == "random forests" ~ "random forest",
                               modelType == "Logisitic regression model" ~ "Logistic regression",
                               .default = modelType))

# dichtotomisierung ist bei Modellen meistens ein Problem, weil es einen Grenzbereich gibt
# knowledge-driven to occurrence data-driven 
# occurrence daten in data - driven umbennen
# alle sind occurrence driven
# modelle versucht man in knowledge und data-driven zu unterscheiden

# wollen sagen, dass Wissen wenig berücksichtigt wird
# nicht einfach nur alles reinklatschen was geht sondern vorher Gedanken machen

modelTypeTable <- modelTypeTable |> 
  mutate(modelClass = case_when(
    grepl("Maxent", modelType) ~ "Maximum entropy model",
    grepl("Logistic regression|Generalized linear mixed model", modelType) ~ "Regression models",
    grepl("Boosted Regression Trees|Random forest|XGBoost", modelType) ~ "Tree-based models",
    grepl("Polynomial distributed lag models|Non Linear Discriminant Analysis|Genetic Algorithm for Rule Set Production", modelType) ~ "Nonlinear predictive models",
    grepl("Custom|ZPOMs", modelType) ~ "Custom models",
    grepl("Stochastic partial differential equation|Simple kriging", modelType) ~ "Geostatistic models"
  )) |> 
  count(modelApproaches, modelClass)

modelTypeTable$modelApproaches = factor(modelTypeTable$modelApproaches, levels = c("knowledge driven", "expert derived", "hybrid predictive", "data driven"), ordered = TRUE)
# more complex plot
ggplot(data = modelTypeTable, aes(x = modelApproaches, y = n, fill = modelClass, label = modelClass)) +
  labs(title =  "Frequency of different model types used",
       subtitle = element_text(paste0("Total number: ", sum(modelTypeTable$n)))) + 
  ylab("Frequency of applied models")+
  xlab("Modeling approach spectrum from knowledge-driven to data-driven") + 
  geom_bar(position = "stack", stat = "identity") + 
  # geom_text(aes(x = modelClass, y = n, label = modelType),
  #           position = position_stack(vjust = .5), size = 3.9) + 
  geom_bar_text(position = "stack", reflow = TRUE, min.size = 12) + 
  #scale_fill_hue(c = 90) +
  scale_fill_brewer(palette = "Set3") + 
  theme_bw(base_size = 22) +
  theme(legend.position="none",
        plot.title = element_text(face="bold"))

ggsave(filename = "model_approaches.png",
       device = "png",
       path = plotPath,
       dpi = 600,
       height = 9,
       width = 15)

